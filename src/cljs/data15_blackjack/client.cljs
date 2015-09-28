(ns data15_blackjack.client
  (:require
    [clojure.string :as str]
    [cljs.core.async :as async :refer (<! >! put! chan)]
    [taoensso.encore :as encore :refer ()]
    [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]
    [taoensso.sente :as sente :refer (cb-success?)])
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer (go go-loop)]))

;;;; Logging config
(sente/set-logging-level! :trace)                           ; Uncomment for more logging

;;;; Packer (client<->server serializtion format) config
(def packer :edn)

;;;; Client-side setup

(debugf "ClojureScript appears to have loaded correctly.")
(let [rand-chsk-type (if (>= (rand) 0.5) :ajax :auto)

      {:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/chsk"                   ; Note the same URL as before
                                  {:type   rand-chsk-type
                                   :packer packer})]
  (debugf "Randomly selected chsk type: %s" rand-chsk-type)
  (def chsk chsk)
  (def ch-chsk ch-recv)                                     ; ChannelSocket's receive channel
  (def chsk-send! send-fn)                                  ; ChannelSocket's send API fn
  (def chsk-state state)                                    ; Watchable, read-only atom
  )


;; So you'll want to define one server-side and one client-side
;; (fn event-msg-handler [ev-msg]) to correctly handle incoming events. How you
;; actually do this is entirely up to you. In this example we use a multimethod
;; that dispatches to a method based on the `event-msg`'s event-id. Some
;; alternatives include a simple `case`/`cond`/`condp` against event-ids, or
;; `core.match` against events.
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
    (debugf "Push event from server: %s" ?data))

  (defmethod event-msg-handler :chsk/handshake
    [{:as ev-msg :keys [?data]}]
    (let [[?uid ?csrf-token ?handshake-data] ?data]
      (debugf "Handshake: %s" ?data)))

  ;; Add your (defmethod handle-event-msg! <event-id> [ev-msg] <body>)s here...
  )

;;;; Client-side UI
(when-let [target-el (.getElementById js/document "btn1")]
  (.addEventListener target-el "click"
                     (fn [ev]
                       (debugf "Button 1 was clicked (won't receive any reply from server)")
                       (chsk-send! [:example/button1 {:had-a-callback? "nope"}]))))

(when-let [target-el (.getElementById js/document "btn2")]
  (.addEventListener target-el "click"
                     (fn [ev]
                       (debugf "Button 2 was clicked (will receive reply from server)")
                       (chsk-send! [:example/button2 {:had-a-callback? "indeed"}] 5000
                                   (fn [cb-reply] (debugf "Callback reply: %s" cb-reply))))))

(when-let [target-el (.getElementById js/document "btn-login")]
  (.addEventListener target-el "click"
                     (fn [ev]
                       (let [user-id (.-value (.getElementById js/document "input-login"))]
                         (if (str/blank? user-id)
                           (js/alert "Please enter a user-id first")
                           (do
                             (debugf "Logging in with user-id %s" user-id)

                             ;;; Use any login procedure you'd like. Here we'll trigger an Ajax
                             ;;; POST request that resets our server-side session. Then we ask
                             ;;; our channel socket to reconnect, thereby picking up the new
                             ;;; session.

                             (sente/ajax-call "/login"
                                              {:method :post
                                               :params {:user-id    (str user-id)
                                                        :csrf-token (:csrf-token @chsk-state)}}
                                              (fn [ajax-resp]
                                                (debugf "Ajax login response: %s" ajax-resp)
                                                (let [login-successful? true ; Your logic here
                                                      ]
                                                  (if-not login-successful?
                                                    (debugf "Login failed")
                                                    (do
                                                      (debugf "Login successful")
                                                      (sente/chsk-reconnect! chsk))))))))))))


