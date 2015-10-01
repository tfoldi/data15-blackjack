(ns data15_blackjack.client
  (:require
    [data15_blackjack.tableau :as tableau]
    [clojure.string :as str]
    [cljs.core.async :as async :refer (<! >! put! chan)]
    [dommy.core :as dommy :refer-macros [sel sel1]]
    [taoensso.encore :as encore :refer ()]
    [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]
    [taoensso.sente :as sente :refer (cb-success?)])
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer (go go-loop)]))

;;;; Logging config
(sente/set-logging-level! :trace)                           ; Uncomment for more logging

;;;; Client-side setup

(debugf "ClojureScript appears to have loaded correctly.")
(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/chsk"
                                  ; Note the same URL as before
                                  {:type   :auto
                                   :packer :edn})]
  (def chsk chsk)
  (def ch-chsk ch-recv)                                     ; ChannelSocket's receive channel
  (def chsk-send! send-fn)                                  ; ChannelSocket's send API fn
  (def chsk-state state)                                    ; Watchable, read-only atom
  )

;;;; Client-side logic
(defn- unbuttonize
  "Convert a ClickEvent object to string while removing the btn- prefix"
  [click-event]
  (-> (.-toElement click-event)
      (.-id)
      (clojure.string/replace "btn-" "")))

(defn login-event-handler
  "Setup username and player role"
  [event]
  (debugf "setting username to user-id")
  (let [user-id (dommy/value (sel1 :#input-login))]
    (if (str/blank? user-id)
      (js/alert "Please enter user name first")
      (do
        (debugf "Logging in with user-id %s" user-id)
        (sente/ajax-call "/login"
                         {:method :post
                          :params {:user-id    (str user-id)
                                   :role       (unbuttonize event) ; button name
                                   :csrf-token (:csrf-token @chsk-state)}}
                         (fn [ajax-resp]
                           (debugf "Ajax login response: %s" ajax-resp)
                           (sente/chsk-reconnect! chsk)))))))

;;;; Client-side UI
(doseq [button (sel :.login-button)]
  (dommy/listen! button :click login-event-handler))

(doseq [button (sel :.game-button)]
  (dommy/listen! button :click
                 (fn [e] (chsk-send! [:data15-blackjack/click (unbuttonize e)]))))

(defn show-game-buttons!
  [show?]
  (doseq [button (sel :.game-button)]
    (dommy/toggle! button show?)))

(defn show-login!
  [show?]
  (dommy/toggle! (sel1 :#div-login) show?))

;;;; Message handlers

(defmulti event-msg-handler :id)                            ; Dispatch on event-id
;; Wrap for logging, catching, etc.:
(defn event-msg-handler* [{:as ev-msg :keys [id ?data event]}]
  (debugf "Event: %s" event)
  (event-msg-handler ev-msg))

(do                                                         ; Client-side methods
  (defmethod event-msg-handler :default                     ; Fallback
    [{:as ev-msg :keys [event]}]
    (debugf "Unhandled event: %s" event))

  (defmethod event-msg-handler :chsk/state
    [{:as ev-msg :keys [?data]}]
    (if (= ?data {:first-open? true})
      (debugf "Channel socket successfully established!")
      (debugf "Channel socket state change: %s" ?data)))

  (defmethod event-msg-handler :chsk/recv
    [{:as ev-msg :keys [?data]}]
    (dommy/set-text! (sel1 :div#debug) (.toString ?data))
    (debugf "Push event from server: %s" ?data))

  (defmethod event-msg-handler :chsk/handshake
    [{:as ev-msg :keys [?data]}]
    (let [[?uid] ?data]
      (debugf "Handshake: %s" ?data)
      (let [logged-in? (not= ?uid :taoensso.sente/nil-uid)]
        (show-game-buttons! logged-in?)
        (show-login! (not logged-in?)))))

  )


;;;; Routers & Initalization
(def router_ (atom nil))
(defn stop-router! [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! []
  (stop-router!)
  (reset! router_ (sente/start-chsk-router! ch-chsk event-msg-handler*)))

(defn start! []
  (start-router!))

(start!)
