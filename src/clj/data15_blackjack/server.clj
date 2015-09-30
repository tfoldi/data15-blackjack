(ns data15-blackjack.server
  (:require
    [ring.middleware.resource :as resources]
    [clojure.string :as str]
    [ring.middleware.defaults]
    [compojure.core :as comp :refer (defroutes GET POST)]
    [compojure.route :as route]
    [hiccup.core :as hiccup]
    [hiccup.page :refer (include-js include-css)]
    [clojure.core.async :as async :refer (<! <!! >! >!! put! chan go go-loop)]
    [taoensso.encore :as encore :refer ()]
    [taoensso.timbre :as timbre :refer (tracef debugf infof warnf errorf)]
    [taoensso.sente :as sente]
    [org.httpkit.server :as http-kit]
    [taoensso.sente.server-adapters.http-kit :refer (sente-web-server-adapter)]
    [data15-blackjack.blackjack :as blackjack])
  (:gen-class))

;;;; Logging config
(sente/set-logging-level! :trace)                           ; uncomment if you enjoy silence

(defn start-web-server!* [ring-handler port]
  (println "Starting http-kit...")
  (let [http-kit-stop-fn (http-kit/run-server ring-handler {:port port})]
    {:server  nil                                           ; http-kit doesn't expose this
     :port    (:local-port (meta http-kit-stop-fn))
     :stop-fn (fn [] (http-kit-stop-fn :timeout 100))}))

;;;; Packer (client<->server serializtion format) config
(def packer :edn)

;;;; Server-side setup
(let [{:keys [ch-recv send-fn ajax-post-fn ajax-get-or-ws-handshake-fn
              connected-uids]}
      (sente/make-channel-socket! sente-web-server-adapter {:packer packer})]
  (def ring-ajax-post ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk ch-recv)                                     ; ChannelSocket's receive channel
  (def chsk-send! send-fn)                                  ; ChannelSocket's send API fn
  (def connected-uids connected-uids))                      ; Watchable, read-only atom


(defn login!
  "Login methods sets the user name. No real login, just associate the username with
  the websocket. Later on this user-id will be used to send messages to one client only"
  [ring-request]
  (let [{:keys [session params]} ring-request
        {:keys [role user-id]} params]
    (debugf "Login request: %s" params)
    (blackjack/set-player-name! role user-id)
    {:status 200 :session (assoc session :role role :uid user-id)}))

(defn landing-pg-handler [req]
  "Langing page containing the tableau JS API vizardry"
  (hiccup/html
    [:head
     (include-js "http://public.tableau.com/javascripts/api/tableau-2.0.1.min.js")
     (include-css "css/page.css")]
    [:div#tableau-viz]
    [:div#div-login
     [:h2 "Set user user-id"]
     [:p [:input#input-login {:type :text :placeholder "Enter your name:"}]
      [:button#btn-player1 {:class "login-button" :type "button"} "I'm Player 1"]
      [:button#btn-player2 {:class "login-button" :type "button"} "I'm Player 2"]]]
    [:p
     [:button#btn-hit {:class "game-button" :type "button"} "hit"]
     [:button#btn-stand {:class "game-button" :type "button"} "stand"]
     [:button#btn-reset {:class "game-button" :type "button"} "new"]]
    [:script {:src "js/client.js"}]                         ; Include our cljs target

    ))

(defroutes my-routes
           "Basic endpoints: /      langing page,
                             /chsh  sente channels,
                             /login to set user name
           plus the usual static resources"

           (GET "/" req (landing-pg-handler req))
           ;;
           (GET "/chsk" req (ring-ajax-get-or-ws-handshake req))
           (POST "/chsk" req (ring-ajax-post req))
           (POST "/login" req (login! req))
           ;;
           (route/resources "/")                            ; Static files, notably public/js/client.js (our cljs target)
           (route/not-found (hiccup/html
                              [:h1 "Invalid URL"])))

(def my-ring-handler
  "The ring handler is reponsible to start ring web server, setup routing
  and session management default"
  (let [ring-defaults-config
        (assoc-in ring.middleware.defaults/site-defaults [:security :anti-forgery]
                  {:read-token (fn [req] (-> req :params :csrf-token))})]
    (ring.middleware.defaults/wrap-defaults my-routes ring-defaults-config)))

;;;; Routing handlers

(defn server->all-users!
  [message]
  (doseq [uid (:any @connected-uids)]
    (when-not (= uid :sente/nil-uid)
      (chsk-send! uid message))))

(defn broadcast-state! []
  (server->all-users! [:data15-blackjack/broadcast-state @blackjack/game]))

(defmulti event-msg-handler :id)                            ; Dispatch on event-id
;; Wrap for logging, catching, etc.:
(defn event-msg-handler* [{:as ev-msg :keys [id ?data event]}]
  (debugf "Event: %s" event)
  (event-msg-handler ev-msg))


(do
  (defmethod event-msg-handler :default                     ; Fallback
    [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
    (let [session (:session ring-req)
          uid (:uid session)]
      (debugf "Unhandled event: %s" event)
      (when ?reply-fn
        (?reply-fn {:umatched-event-as-echoed-from-from-server event}))))

  (defmethod event-msg-handler :data15-blackjack/click
    [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
    (let [session (:session ring-req)
          uid (:uid session)]
      (debugf "Initalize request: %s uid: %s data: %s" event uid ?data)
      (condp = ?data
        "reset" (blackjack/start-game)
        "stand" (blackjack/stand (get session :role))
        "hit" (blackjack/hit-me (get session :role)))
      (broadcast-state!)))

  ;; Add your (defmethod event-msg-handler <event-id> [ev-msg] <body>)s here...
  )

;;;; Some utility
(defn number-of-connected-users
  "Number of identified users connected"
  []
  (count (filter #(not= :taoensso.sente/nil-uid %) (:any @connected-uids))))


;;;; Init

(defonce web-server_ (atom nil))                            ; {:server _ :port _ :stop-fn (fn [])}
(defn stop-web-server! [] (when-let [m @web-server_] ((:stop-fn m))))

(defn start-web-server! [& [port]]
  (stop-web-server!)
  (let [{:keys [stop-fn port] :as server-map}
        (start-web-server!* (var my-ring-handler)
                            (or port 3000))
        uri (format "http://localhost:%s/" port)]
    (debugf "Web server is running at `%s`" uri)
    (reset! web-server_ server-map)))

(defonce router_ (atom nil))

(defn stop-router! [] (when-let [stop-f @router_] (stop-f)))

(defn start-router! []
  (stop-router!)
  (reset! router_ (sente/start-chsk-router! ch-chsk event-msg-handler*)))

(defn start! []
  (start-router!)
  (start-web-server!))

; start things from REPL immediately
(start-router!)

; in case you start from command line
(defn -main [& args] (start!))
