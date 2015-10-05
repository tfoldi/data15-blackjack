;; This namespace contains the game specific, non-tableau related
;; functions required. It's designed for two players (at the moment)
;; plus a dealer. The namespace has no external dependency: it can
;; run on server or client side as required. The namespaced methods
;; are invoked from the `server` namespace, from ring handlers.
;;
;; Majority of these functions are fork from: https://github.com/jdeisenberg/cljs-blackjack
(ns data15-blackjack.blackjack
  (:require [data15-blackjack.utils :refer [other-player keywordize]]))

;; `game` is the main state container atom. It contains:
;;
;;  * Deck and discard pile
;;  * Cards in hand (for players & dealer)
;;  * Status of hands (actual score & status info like ok, bust, blackjack)
;;  * Feedback (text status message)
(defonce game (atom {:deck             (into [] (shuffle (range 0 52)))
                 :dealer-hand      []
                 :dealer-status    nil
                 :player1-hand     []
                 :player1-name     nil
                 :player2-hand     []
                 :player2-name     nil
                 :player1-status   [0 :na]
                 :player2-status   [0 :na]
                 :player1-feedback ""
                 :player2-feeback  ""}))

(defn set-player-name!
  "Set player name and ensure that one player has only one
  user id"
  [player name]
  (when (= (get @game (keywordize (other-player player) :name)) name)
    ;; Log out other player if the user-id is the same
    (swap! game assoc
           (keywordize (other-player player) :name) nil))
  (swap! game assoc
         (keywordize player :name) name))


;; ---------------------------------------------------------
;; # Game Logic

(defn deal
  "Deal one card from the deck to a hand in the given
  position (face up or face down), possibly using the
  discard pile. Return a vector of
  the remaining deck, new hand, and discard pile"
  [[deck hand discard-pile] position]
  (let [new-deck (if (empty? deck) (shuffle discard-pile) deck)
        new-discard (if (empty? deck) [] discard-pile)
        card [(first new-deck) position]]
    [(rest new-deck) (conj hand card) new-discard]))

(defn discard
  "Discard contents of a hand onto a pile; just the card number,
  not the up/down position. Return an empty hand and the new
  discard pile."
  [[hand pile]]
  [[] (vec (reduce (fn [acc x] (conj acc (first x))) pile hand))])


(defn accumulate-value
  "Helper function to total a hand. The accumulator is
  a vector giving the current total of the hand and the
  current value of an ace (1 or 11)"
  [[acc ace-value] card]
  (let [card-mod (inc (mod (first card) 13))
        card-value (if (= card-mod 1)
                     ace-value
                     (min 10 card-mod))]
    [(+ acc card-value) (if (= card-mod 1) 1 ace-value)]))


(defn evaluate-hand
  "Get total value of hand. Return a vector with total and
  the status `(:ok, :blackjack, :bust, :stand)`"
  [hand]
  (let [[pre-total ace-value] (reduce accumulate-value [0 11] hand)
        total (if (and (> pre-total 21) (= ace-value 1)) (- pre-total 10) pre-total)]
    (vec [total (cond
                  (and (= total 21) (= (count hand) 2)) :blackjack
                  (<= total 21) :ok
                  :else :bust)])))

(defn immediate-win
  "Given players hand and dealer hand, return true if dealer
  has blackjack, players have blackjack, false otherwise."
  [dealer-hand player1-hand player2-hand]
  (let [[p1total _] (evaluate-hand player1-hand)
        [p2total _] (evaluate-hand player2-hand)
        [dtotal _] (evaluate-hand dealer-hand)]
    (or (and (= p1total 21) (= p2total 21)) (= dtotal 21))))

(defn reveal
  "This function takes a player's hand and returns a new hand
  with all the cards in the :up position"
  [hand]
  (vec (map (fn [card] [(first card) :up]) hand)))


(defn feedback
  "Store personalized feedback message, practically the
  result from `end-game`."
  [player message]
  (swap! game assoc (keywordize player :feedback) message))

(defn end-game
  "Evaluate the dealer's and player's hands when the
  game has ended."
  [dealer-hand player-hand player]
  (let [[ptotal pstatus] (evaluate-hand player-hand)
        [dtotal dstatus] (evaluate-hand dealer-hand)]
    (cond
      (> ptotal 21) (feedback player "Sorry, you are busted.")
      (> dtotal 21) (feedback player "Dealer goes bust. You win!")
      (= ptotal dtotal) (feedback player "Tie game.")
      (= pstatus :blackjack) (feedback player "You win with blackjack!")
      (= dstatus :blackjack) (feedback player "Dealer has blackjack.")
      (< ptotal dtotal) (feedback player "Dealer wins.")
      (> ptotal dtotal) (feedback player "You win!")
      :else (feedback player "Unknown result (Shouldn't happen.)"))))


(defn start-game
  "Deal two cards to the player (both face up), and two to the dealer (one
  face down and one face up). Update the game atom, and check for an immediate
  win. "
  []
  (let [{:keys [deck discard-pile dealer-hand player1-hand player2-hand]} @game
        [player1-1 pile0] (discard [player1-hand discard-pile])
        [player2-1 pile1] (discard [player2-hand pile0])
        [dealer1 pile2] (discard [dealer-hand pile1])
        [deck2 dealer2 pile3] (deal (deal [deck dealer1 pile2] :up) :down)
        [deck3 player1-2 pile4] (deal (deal [deck2 player1-1 pile3] :up) :up)
        [deck4 player2-2 pile-after-deal] (deal (deal [deck3 player2-1 pile4] :up) :up)
        ]
    (swap! game assoc :playing true :discard-pile pile-after-deal :player1-hand player1-2
           :player2-hand player2-2 :dealer-hand dealer2 :deck deck4
           :player1-feedback "" :player2-feedback ""
           :player1-status (evaluate-hand player1-2)
           :player2-status (evaluate-hand player2-2)
           :dealer-status nil
           )
    (if (immediate-win dealer2 player1-2 player2-2)
      (do
        (swap! game assoc :dealer-hand (reveal dealer2))
        (end-game dealer2 player1-2 :player1)
        (end-game dealer2 player2-2 :player2)))))


(defn hit-me
  "Deal a card face up to the player, and evaluate the hand.
  If the player went bust, end the game."
  [player]
  (let [{:keys [deck discard-pile dealer-hand]} @game
        player-hand (@game (keywordize player :hand))
        [deck2 player2 discard2] (deal [deck player-hand discard-pile] :up)
        [total status] (evaluate-hand player2)]
    (swap! game assoc (keywordize player :hand) player2 (keywordize player :status) [total status]
           :deck deck2 :discard-pile discard2)
    (if (= status :bust)
      (end-game dealer-hand player2 player))))



(defn stand
  "Player is satisfied with hand. Reveal the dealer's hand,
  then deal cards one at a time until the dealer has to stand
  or goes bust."
  [role num-players]
  (swap! game assoc-in [(keywordize role :status) 2] :stand)
  (if (and (= num-players 2) (not= (get-in @game
                                           [(keywordize (other-player role) :status) 2])
                                   :stand))
    ; Other player is still playing
    (swap! game assoc (keywordize role :feedback) "Waiting for other player to stand.")
    ; We are done
    (let [{:keys [deck dealer-hand player1-hand player2-hand discard-pile]} @game
          dhand (reveal dealer-hand)]
      (swap! game assoc :dealer-hand dhand)
      (loop [loop-deck deck
             loop-hand dhand
             loop-pile discard-pile]
        (let [[total status] (evaluate-hand loop-hand)]
          (if (or (= status :bust) (>= total 17))
            (do
              (swap! game assoc :dealer-hand loop-hand :dealer-status [total status])
              (end-game loop-hand player1-hand :player1)
              (end-game loop-hand player2-hand :player2))
            (let [[new-deck new-hand new-discard] (deal [loop-deck loop-hand loop-pile] :up)]
              (swap! game assoc :dealer-hand new-hand)
              (recur new-deck new-hand new-discard))))))))

