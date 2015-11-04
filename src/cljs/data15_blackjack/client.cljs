;; # Client-side browser functions
;;
;; This namespace responsible for the client side code
;; running in the browser side. This includes the websocket/ajax
;; communication channel management, sending, receiving and
;; dispatching messages.
(ns data15_blackjack.client
  (:require
    [data15-blackjack.tableau :as tableau]
    [clojure.string :as str]
    [cljs.core.async :as async :refer (<! >! put! chan)]
    [dommy.core :as dommy :refer-macros [sel sel1]]
    [taoensso.encore :as encore :refer ()]
    [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]
    [taoensso.sente :as sente :refer (cb-success?)])
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer (go go-loop)]))

;; Logging config. We do love logs, so level is `:trace`
(sente/set-logging-level! :trace)

;; ------------------
;; ## Client-side setup

;; Create a new `sente` socket (websocket if possible, ajax as
;; a fallback option). The location handler on server side is
;; `/chsk`, packer type (serialization) is edn based (which is
;; like json for clojure). If the socket is ready, bind
;; async channels and a watchable atom with the socket state.
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

;; ------------------
;; ## Client-side logic and helpers
(defn- unbuttonize
  "Convert a ClickEvent object to string while removing the btn- prefix"
  [click-event]
  (-> (.-toElement click-event)
      (.-id)
      (clojure.string/replace "btn-" "")))

(defn login-event-handler
  "Setup username and player role. Called from login buttons, thus,
  `event` is `ClickEvent` object. Basically:

  * Check user supplied name is blank? If yes, show an alert
  * Otherwise `/login` with user-id (textbox content) and role
    (player1 or player2) based on the pressed button.
  * Re-establish sente socket - with the new user-id"
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

;; ------------------
;; ## Client-side UI

;; Call `login-event-handler` on login button click
(doseq [button (sel :.login-button)]
  (dommy/listen! button :click login-event-handler))

;; Send new/hit/stand events to the server. The message payload is simply
;; the button's name without the `btn-` prefix
(doseq [button (sel :.game-button)]
  (dommy/listen! button :click
                 (fn [e] (chsk-send! [:data15-blackjack/click (unbuttonize e)]))))

(defn show-game-buttons!
  "Toggle buttons in game-button class based on `show?` param"
  [show?]
  (doseq [button (sel :.game-button)]
    (dommy/toggle! button show?)))

(defn show-tableau-viz!
  "Toggle tableau viz based on `show?` param"
  [show?]
  (dommy/toggle! (sel1 :div#tableau-viz) show?))

(defn show-login!
  "Toggle login box based on `show?` parameter"
  [show?]
  (dommy/toggle! (sel1 :div#div-login) show?))

;; -------------------
;; ## Message handlers

;; Watch for `:viz-ready` status in `tableau/viz` atom. If the `viz`
;; status have changed and the new status is :viz-ready, then send
;; `load` message to server.
(add-watch tableau/viz :ui
           (fn [_ _ _ new-state]
             (when (= :viz-ready (new-state :status))
               (chsk-send! [:data15-blackjack/click "load"]))))

;; Define new multi-function which will dispatch on event-id
(defmulti event-msg-handler :id)
;; Wrap for logging and dispatching. We will use a `defmulti`
;; to deliver message based on event
(defn event-msg-handler* [{:as ev-msg :keys [id ?data event]}]
  (debugf " Event: % s " event)
  (event-msg-handler ev-msg))

;; Just log unhandled events (like `:check-state`, `:ws-ping`).
(defmethod event-msg-handler :default
  [{:as ev-msg :keys [event]}]
  (debugf " Unhandled event: % s " event))

;; We just received a new push notification from the server.
;; If we already authenticated, update the tableau visualization
;; with `tableau/update-tableau`.
(defmethod event-msg-handler :chsk/recv
  [{:as ev-msg :keys [?data]}]
  (when-not (= (get @chsk-state :uid) :sente/nil-uid)
    (tableau/update-tableau (get @chsk-state :uid) (second ?data)))
  (debugf " Push event from server: % s " ?data))

;; The handshare is the first callback from server side after
;; establishing connection. Change visible div's according to
;; user-id: if the user-id is `nil-uid` we should hide the
;; viz and show the login.
(defmethod event-msg-handler :chsk/handshake
  [{:as ev-msg :keys [?data]}]
  (let [[?uid] ?data]
    (debugf " Handshake: % s " ?data)
    (let [logged-in? (not= ?uid :taoensso.sente/nil-uid)]
      (show-game-buttons! logged-in?)
      (show-tableau-viz! logged-in?)
      (show-login! (not logged-in?)))))

;; -----------------------
;; ## Router Initalization
(def router_ " Atom to store stop function for stopping the router " (atom nil))
(defn stop-router!
  " Stop the message router by calling the previously saved stop function "
  [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! []
  " Stop router first, then start and save the result (which is a stop callback)
in `router_ `. "
  (stop-router!)
  (reset! router_ (sente/start-chsk-router! ch-chsk event-msg-handler*)))

;; Start router by default, on-load
(start-router!)

