(ns data15-blackjack.blackjack)

(defonce game (atom {:deck             (into [] (shuffle (range 0 52)))
                     :dealer-hand      []
                     :player1-hand     []
                     :player1-name     nil
                     :player2-hand     []
                     :player2-name     nil
                     :player1-status   :na
                     :player2-status   :na
                     :player1-feedback ""
                     :player2-feeback  ""}))

(defn- other-player
  [player]
  (if (= (name player) "player1")
    :player2
    :player1))

(defn- keywordize
  [player postfix]
  (keyword (str (name player) "-" (name postfix))))

(defn set-player-name!
  [player name]
  (when (= (@game (keywordize (other-player player) :name)) name)
    ;; Log out other player if the user-id is the same
    (swap! game assoc
           (keywordize (other-player player) :name) nil
           (keywordize (other-player player) :status) :na))
  (swap! game assoc
         (keywordize player :name) name
         (keywordize player :status) :signed))



