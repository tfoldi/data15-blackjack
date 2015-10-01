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
