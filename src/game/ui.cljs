(ns game.ui
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdomc]
            [game.core :as core]))

;; ---------------------------------------------------------------------------
;; Game state
;; ---------------------------------------------------------------------------

(defonce game-state (r/atom nil))

(defn start-new-game! []
  (reset! game-state (core/new-game)))

(defn deal-room! []
  (swap! game-state core/deal-room))

(defn resolve-card! [idx use-weapon?]
  (swap! game-state core/resolve-card idx use-weapon?))

(defn end-room! []
  (swap! game-state core/end-room))

(defn escape-room! []
  (swap! game-state core/escape-room))

;; ---------------------------------------------------------------------------
;; Display helpers
;; ---------------------------------------------------------------------------

(defn suit-symbol [suit]
  (case suit
    :hearts   "♥"
    :diamonds "♦"
    :clubs    "♣"
    :spades   "♠"))

(defn suit-color [suit]
  (case suit
    :hearts   "text-red-500"
    :diamonds "text-red-500"
    :clubs    "text-gray-100"
    :spades   "text-gray-100"))

(defn rank-label [rank]
  (case rank
    14 "A"
    13 "K"
    12 "Q"
    11 "J"
    (str rank)))

(defn card-type-label [card]
  (case (core/card-type card)
    :potion  "Potion"
    :weapon  "Weapon"
    :monster "Monster"))

(defn card-type-icon [card]
  (case (core/card-type card)
    :potion  "🧪"
    :weapon  "⚔️"
    :monster "👹"))

(defn card-type-bg [card]
  (case (core/card-type card)
    :potion  "from-rose-900/40 to-rose-950/60 border-rose-700/50 hover:border-rose-500/70"
    :weapon  "from-amber-900/40 to-amber-950/60 border-amber-700/50 hover:border-amber-500/70"
    :monster "from-violet-900/40 to-violet-950/60 border-violet-700/50 hover:border-violet-500/70"))

;; ---------------------------------------------------------------------------
;; Components
;; ---------------------------------------------------------------------------

(defn hp-bar []
  (let [gs @game-state
        hp (:hp gs)
        max-hp (:max-hp gs)
        pct (max 0 (* 100 (/ hp max-hp)))
        bar-color (cond
                    (> pct 60) "bg-emerald-500"
                    (> pct 30) "bg-amber-500"
                    :else      "bg-red-500")]
    [:div.mb-6
     [:div.flex.items-center.justify-between.mb-1
      [:span.text-sm.font-medium "HP"]
      [:span.text-sm.font-bold {:class (cond
                                         (> hp 12) "text-emerald-400"
                                         (> hp 6)  "text-amber-400"
                                         :else     "text-red-400")}
       (str hp " / " max-hp)]]
     [:div.w-full.h-3.bg-gray-700.rounded-full.overflow-hidden
      [:div {:class (str bar-color " h-full rounded-full transition-all duration-500 ease-out")
             :style {:width (str pct "%")}}]]]))

(defn weapon-display []
  (let [gs @game-state
        weapon (:weapon gs)
        last-used (:weapon-last-used gs)]
    [:div {:class "mb-6 p-3 rounded-lg bg-gray-800/60 border border-gray-700/50"}
     [:div.flex.items-center.justify-between
      [:span.text-sm.font-medium.text-gray-400 "Weapon"]
      (if weapon
        [:div.flex.items-center.gap-2
         [:span {:class (str "text-lg font-bold " (suit-color (:suit weapon)))}
          (str (rank-label (:rank weapon)) (suit-symbol (:suit weapon)))]
         [:span.text-xs.text-gray-500
          (str "⚔️ " (core/card-value weapon))
          (when last-used
            (str " · max target < " last-used))]]
        [:span.text-sm.text-gray-600.italic "None"])]]))

(defn dungeon-counter []
  (let [remaining (count (:dungeon @game-state))]
    [:div {:class "mb-6 p-3 rounded-lg bg-gray-800/60 border border-gray-700/50"}
     [:div.flex.items-center.justify-between
      [:span.text-sm.font-medium.text-gray-400 "Dungeon"]
      [:div.flex.items-center.gap-2
       [:span.text-lg "🃏"]
       [:span.font-bold (str remaining " cards")]]]]))

(defn card-component
  "Renders a single card in the room. idx is the card's index in the room."
  [card idx]
  (let [gs       @game-state
        ctype    (core/card-type card)
        monster? (= :monster ctype)
        can-wep? (and monster? (core/can-use-weapon? gs card))]
    [:div {:class (str "relative flex flex-col items-center justify-between "
                       "p-4 rounded-xl border-2 bg-gradient-to-b "
                       "transition-all duration-200 cursor-pointer "
                       "min-h-[180px] w-full "
                       (card-type-bg card))}
     ;; Top: rank + suit
     [:div.flex.items-center.justify-between.w-full
      [:span {:class (str "text-2xl font-bold " (suit-color (:suit card)))}
       (rank-label (:rank card))]
      [:span {:class (str "text-2xl " (suit-color (:suit card)))}
       (suit-symbol (:suit card))]]
     ;; Middle: type icon
     [:div.text-4xl.my-2 (card-type-icon card)]
     ;; Bottom: type label + value
     [:div.text-center
      [:div.text-xs.font-medium.text-gray-400.uppercase.tracking-wide
       (card-type-label card)]
      [:div.text-sm.font-bold.mt-1
       (case ctype
         :potion  (str "+" (core/card-value card) " HP")
         :weapon  (str "⚔️ " (core/card-value card))
         :monster (str "💀 " (core/card-value card)))]]
     ;; Action buttons
     [:div {:class "mt-3 flex flex-col gap-1.5 w-full"}
      (if monster?
        [:<>
         [:button {:on-click #(resolve-card! idx false)
                   :class "w-full py-1.5 px-3 text-xs font-semibold rounded-lg bg-red-600/80 hover:bg-red-500 transition-colors"}
          "Fight bare-handed"]
         (when can-wep?
           [:button {:on-click #(resolve-card! idx true)
                     :class "w-full py-1.5 px-3 text-xs font-semibold rounded-lg bg-amber-600/80 hover:bg-amber-500 transition-colors"}
            "Use weapon"])]
        [:button {:on-click #(resolve-card! idx false)
                  :class "w-full py-1.5 px-3 text-xs font-semibold rounded-lg bg-indigo-600/80 hover:bg-indigo-500 transition-colors"}
         (case ctype
           :potion "Drink"
           :weapon "Equip")])]]))

(defn room-display []
  (let [gs   @game-state
        room (:room gs)]
    [:div.mb-6
     [:div.flex.items-center.justify-between.mb-3
      [:h2.text-lg.font-semibold "Room"]
      [:span.text-sm.text-gray-400
       (str (:room-resolved gs) " resolved")]]
     (if (seq room)
       [:div {:class "grid grid-cols-2 sm:grid-cols-4 gap-3"}
        (doall
          (map-indexed
            (fn [idx card]
              ^{:key (str (:suit card) "-" (:rank card) "-" idx)}
              [card-component card idx])
            room))]
       [:div {:class "text-center text-gray-600 py-12 border-2 border-dashed border-gray-800 rounded-xl"}
        "No cards in room"])]))

(defn action-bar []
  (let [gs    @game-state
        phase (:phase gs)]
    [:div.flex.gap-3.justify-center.mt-4
     (when (= :room-draw phase)
       [:button {:on-click deal-room!
                 :class "px-6 py-3 text-sm font-bold rounded-xl bg-indigo-600 hover:bg-indigo-500 transition-colors shadow-lg shadow-indigo-900/50"}
        "Draw Room"])
     (when (and (= :room-action phase) (core/can-skip-remaining? gs))
       [:button {:on-click end-room!
                 :class "px-6 py-3 text-sm font-bold rounded-xl bg-gray-700 hover:bg-gray-600 transition-colors"}
        "Skip & End Room"])
     (when (and (= :room-action phase) (core/can-escape? gs))
       [:button {:on-click escape-room!
                 :class "px-6 py-3 text-sm font-bold rounded-xl bg-yellow-700 hover:bg-yellow-600 transition-colors"}
        "Escape Room"])]))

(defn game-over-screen []
  (let [gs @game-state
        win? (= :win (:result gs))]
    [:div {:class "fixed inset-0 bg-black/80 flex items-center justify-center z-50"}
     [:div {:class "text-center p-8 rounded-2xl bg-gray-800 border border-gray-700 shadow-2xl max-w-sm mx-4"}
      [:div.text-6xl.mb-4 (if win? "🏆" "💀")]
      [:h2 {:class "text-3xl font-bold mb-2"}
       (if win? "Victory!" "Defeat")]
      [:p.text-gray-400.mb-6
       (if win?
         "You cleared the dungeon!"
         (str "You fell with " (:hp gs) " HP."))]
      [:button {:on-click start-new-game!
                :class "px-8 py-3 text-sm font-bold rounded-xl bg-indigo-600 hover:bg-indigo-500 transition-colors shadow-lg"}
       "Play Again"]]]))

(defn start-screen []
  [:div {:class "flex flex-col items-center justify-center min-h-screen px-4"}
   [:h1 {:class "text-5xl font-bold mb-2 tracking-tight"} "Scoundrel"]
   [:p {:class "text-gray-400 mb-8 text-center max-w-md"}
    "A solo dungeon crawler card game. Draw rooms, fight monsters, find weapons, and drink potions to survive the dungeon."]
   [:button {:on-click start-new-game!
             :class "px-8 py-4 text-lg font-bold rounded-xl bg-indigo-600 hover:bg-indigo-500 transition-colors shadow-lg shadow-indigo-900/50"}
    "New Game"]])

(defn game-board []
  [:div {:class "max-w-2xl mx-auto px-4 py-6"}
   [:div.flex.items-center.justify-between.mb-6
    [:h1.text-2xl.font-bold.tracking-tight "Scoundrel"]
    [:button {:on-click #(reset! game-state nil)
              :class "text-xs text-gray-500 hover:text-gray-300 transition-colors"}
     "← Menu"]]
   [hp-bar]
   [:div {:class "grid grid-cols-2 gap-3 mb-4"}
    [weapon-display]
    [dungeon-counter]]
   [room-display]
   [action-bar]
   (when (core/game-over? @game-state)
     [game-over-screen])])

(defn app []
  (if (nil? @game-state)
    [start-screen]
    [game-board]))

;; ---------------------------------------------------------------------------
;; Init
;; ---------------------------------------------------------------------------

(defonce root (delay (rdomc/create-root (.getElementById js/document "app"))))

(defn ^:export init []
  (rdomc/render @root [app]))
