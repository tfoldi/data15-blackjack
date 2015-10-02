(ns data15-blackjack.tableau
  (:require
    [dommy.core :as dommy :refer-macros [sel sel1]]))

; atom to store tableau states
(def viz (atom {:status :not-loaded}))

; The public URL of the tableau viz
(def viz-url "https://public.tableau.com/views/Blackjack/BlackjackTableau")

; tableau viz's DOM element
(def placeholder-div (sel1 :div#tableau-viz))

; Hide things from the embedded viz
(def viz-options
  (js-obj
    "hideTabs" true
    "hideToolbar" true
    "onFirstInteractive" #(swap! viz assoc :status :viz-ready)))

; for debug: access
(defn get-vizobj [] (get @viz :vizobj))

; Initialize viz and store it in viz atom
(swap! viz assoc :status :initalize :vizobj
       (js/tableau.Viz. placeholder-div viz-url viz-options))

(defn workbook [] (.getWorkbook (get @viz :vizobj)))

(defn get-sheet-in-active-sheet [sheet]
  (-> (workbook)
      (.getActiveSheet)
      (.getWorksheets)
      (.get sheet)))

(defn filter-update [sheet key values]
  (-> (get-sheet-in-active-sheet sheet)
      (.applyFilterAsync key (clj->js values) js/tableau.FilterUpdateType.REPLACE)))

(defn parameter-update [workbook key value]
  (.changeParameterValueAsync workbook key (clj->js value)))


(defn pad-with-blank-cards
  [cards]
  (let [num-cards (count cards)]
    (concat cards (take (- 5 num-cards) [452 352 252 152 52]))))

(defn update-cards [sheet cards]
  (filter-update sheet "Location-ID" (pad-with-blank-cards cards)))

(defn get-cards
  [hand]
  (doall (map-indexed
           (fn [idx [card pos]]
             (if (= pos :down)
               99                                           ; face down card
               (+ (* idx 100) card)))                       ; 100 * pos in hand + card ID
           hand)))

(defn update-tableau [uid state]
  (let [{:keys [player1-name player2-name player1-status player2-status
                dealer-hand player1-hand player2-hand
                player1-feedback player2-feedback]} state]
    (parameter-update (workbook) "player1-name" (or player1-name "Player2"))
    (parameter-update (workbook) "player2-name" (or player2-name "Player1"))
    (parameter-update (workbook) "player1-score" (first player1-status))
    (parameter-update (workbook) "player2-score" (first player2-status))
    (update-cards "Dealer" (get-cards dealer-hand))
    (if (= uid player1-name)
      (do
        (parameter-update (workbook) "feedback" player1-feedback )
        (update-cards "Player" (get-cards player1-hand)))
      (do
        (parameter-update (workbook) "feedback" player2-feedback )
        (update-cards "Player" (get-cards player2-hand))))))
