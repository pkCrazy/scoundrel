(ns game.ui
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdomc]
            [game.core :as core]))

(defn app []
  [:div {:style {:text-align "center" :margin-top "4rem"}}
   [:h1 "Scoundrel"]
   [:p "Card game — coming soon"]])

(defonce root (delay (rdomc/create-root (.getElementById js/document "app"))))

(defn ^:export init []
  (rdomc/render @root [app]))
