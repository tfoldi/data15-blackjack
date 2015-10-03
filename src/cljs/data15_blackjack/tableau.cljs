(ns data15-blackjack.tableau
  (:require
    [data15-blackjack.utils :refer [other-player keywordize select-values]]
    [dommy.core :refer-macros [sel sel1]]))

(def viz
  "Map (atom) to store tableau viz load status & object reference "
  (atom {:status :not-loaded :vizobj nil}))

(def viz-url
  "The public URL of the tableau viz"
  "https://public.tableau.com/views/Blackjack/BlackjackTableau")

(def placeholder-div
  "tableau viz's DOM element"
  (sel1 :div#tableau-viz))

;
(def viz-options
  "Hide things from the embedded viz"
  (js-obj
    "hideTabs" true
    "hideToolbar" true
    "onFirstInteractive" #(swap! viz assoc :status :viz-ready)))

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
  "Calculate cards unique ID in tableau. Basic calculation is: `99` for all face
  down cards, otherwise position in hand * 100 + card id."
  (doall (map-indexed
           (fn [idx [card pos]]
             (if (= pos :down)
               99                                           ; face down card
               (+ (* idx 100) card)))                       ; 100 * pos in hand + card ID
           hand)))


(defn update-tableau [uid state]
  "Synchronize Tableau report with the blackjack game state broadcastet by
  the server after every action"
  (let [{:keys [player1-name dealer-hand]} state
        my-role (if (= uid player1-name) :player1 :player2)
        other-user (other-player my-role)
        [other-status your-status feedback other-name your-hand]
        (select-values state
                     [(keywordize other-user "status")
                      (keywordize my-role "status")
                      (keywordize my-role "feedback")
                      (keywordize other-user "name")
                      (keywordize my-role "hand")])]

    (update-cards "Player" (get-cards your-hand))
    (update-cards "Dealer" (get-cards dealer-hand))
    (parameter-update (workbook) "other-score" (first other-status))
    (parameter-update (workbook) "your-score" (first your-status))
    (parameter-update (workbook) "feedback" feedback)
    (parameter-update (workbook) "other-name" (or other-name "Player2"))))
