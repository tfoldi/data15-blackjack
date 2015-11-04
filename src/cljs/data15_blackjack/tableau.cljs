;; # Tableau related functions
;;
;; This namespace loads and changes the tableau visualisation.
(ns data15-blackjack.tableau
  (:require
    [data15-blackjack.utils :refer [other-player keywordize select-values]]
    [dommy.core :refer-macros [sel sel1]]))

;; Initializing Viz
;; ================

(def viz
  "Map (atom) to store tableau viz load status & object reference "
  (atom {:status :not-loaded :vizobj nil}))

(def viz-url
  "The public URL of the tableau viz."
  "https://public.tableau.com/views/Blackjack/BlackjackTableau")

(def placeholder-div
  "Tableau viz's div element. This is where the visualization will go"
  (sel1 :div#tableau-viz))


(def viz-options
  "Javascript object to interface with `Viz` constructor. Hide tabs & toolbars.
  After initalizing the Viz change `:status` to `:viz-ready` in our `viz` atom.
  The clint UI watch this atom: when the status change the dom node will be visible
  to the gamer."
  (js-obj
    "hideTabs" true
    "hideToolbar" true
    "onFirstInteractive" #(swap! viz assoc :status :viz-ready)))

;; Initialize our visualization with `viz-options` and store it in `viz` atom.
;; This `swap!` will trigger the watcher defined in `client.cljs` to show the
;; dom element.
(swap! viz assoc :status :initalize :vizobj
       (js/tableau.Viz. placeholder-div viz-url viz-options))

;; Interaction with Viz
;; ====================

(defn workbook
  "Get the `Workbook` object from the previously created viz."
  [] (.getWorkbook (get @viz :vizobj)))

(defn get-sheet-in-active-sheet
  "Get the `Sheet` object from the active sheet. Active sheet must be a `Dashboard`"
  [sheet]
  (-> (workbook)
      (.getActiveSheet)
      (.getWorksheets)
      (.get sheet)))

(defn filter-update
  "Filter supplied `values` for `key` on `sheet`. Filter change is asynchronous.
  Values can be CLJS or JS values and sequences."
  [sheet key values]
  (-> (get-sheet-in-active-sheet sheet)
      (.applyFilterAsync key (clj->js values) js/tableau.FilterUpdateType.REPLACE)))

(defn parameter-update
  "Update `key` parameter to `value` in `workbook`."
  [workbook key value]
  (.changeParameterValueAsync workbook key (clj->js value)))


;; Card related helper functions
;; =============================

(defn pad-with-blank-cards
  "We show always five cards for the players. If the player has less then
  five cards then we are padding his hand with \"empty\" cards. Empty cards
   are the ones with ID=52. Thus, `Location-ID=452` stands for empty card on
   fifth position."
  [cards]
  (let [num-cards (count cards)]
    (concat cards (take (- 5 num-cards) [452 352 252 152 52]))))

(defn update-cards
  "Show hand in `sheet`, padded with empty cards to show at least five of them."
  [sheet cards]
  (filter-update sheet "Location-ID" (pad-with-blank-cards cards)))

(defn get-cards
  "Calculate cards unique ID in tableau. Basic calculation is: `99` for all face
  down cards, otherwise `position in hand * 100 + card id`."
  [hand]
  (map-indexed
    (fn [idx [card pos]]
      (if (= pos :down)
        99                                                  ; face down card
        (+ (* idx 100) card)))                              ; 100 * pos in hand + card ID
    hand))

;; -------------------------------------
;; Synchronize Server state with Tableau
;; =====================================


(defn update-tableau
  "Synchronize Tableau report with the blackjack game state broadcasted by
  the server after every action:

  1. First deconstruct player name and dealer hand
  2. Find out `my-role` and `other-user`: who is player1 and player2
  3. Deconstruct status, feedback, name and hand information from state
  4. Call `update-cards` and `parameter-update` with these state information

  If values are not changed no re-rendering required. Tableau keeps track
  the filter and parameter information client side and invokes Server only
  when necessary"
  [uid state]
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
