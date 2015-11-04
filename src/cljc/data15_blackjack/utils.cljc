;; # Utility functions
;;
;; These functions are available from both clojure (web server)
;; and clojurescript (borwser).
(ns data15-blackjack.utils)

(defn other-player
  "Refer to the other player's id"
  [player]
  (if (= (name player) "player1")
    :player2
    :player1))

(defn keywordize
  "Create keyword like `:player1-hand` from `player1` and `hand`"
  [player postfix]
  (keyword (str (name player) "-" (name postfix))))

(defn select-values [map ks]
  "Select values based on keys from while keeping order"
  (reduce #(conj %1 (map %2)) [] ks))
