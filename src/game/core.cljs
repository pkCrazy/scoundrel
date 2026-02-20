(ns game.core)

;; ---------------------------------------------------------------------------
;; Card utilities
;; ---------------------------------------------------------------------------

(defn card-value
  "Returns the numeric value of a card. Rank is stored as int 2-14."
  [card]
  (:rank card))

(defn card-type
  "Returns :potion (hearts), :weapon (diamonds), or :monster (clubs/spades)."
  [card]
  (case (:suit card)
    :hearts   :potion
    :diamonds :weapon
    (:clubs :spades) :monster))

;; ---------------------------------------------------------------------------
;; Deck creation
;; ---------------------------------------------------------------------------

(defn create-deck
  "Creates the 44-card Scoundrel deck.
   Standard 52 minus the 6 red face cards (J/Q/K of hearts & diamonds)
   and the 2 red aces (A of hearts & diamonds)."
  []
  (let [black-suits [:clubs :spades]
        red-suits   [:hearts :diamonds]
        black-cards (for [suit black-suits
                          rank (range 2 15)]
                      {:suit suit :rank rank})
        red-cards   (for [suit red-suits
                          rank (range 2 11)]
                      {:suit suit :rank rank})]
    (vec (concat black-cards red-cards))))

(defn shuffle-deck
  "Returns a shuffled copy of the deck."
  [deck]
  (vec (clojure.core/shuffle deck)))

;; ---------------------------------------------------------------------------
;; Game initialization
;; ---------------------------------------------------------------------------

(defn- make-game-state
  "Creates an initial game state map with the given dungeon deck."
  [dungeon]
  {:dungeon              dungeon
   :room                 []
   :discard              []
   :carried-over         []
   :hp                   20
   :max-hp               20
   :weapon               nil
   :weapon-last-used     nil
   :room-resolved        0
   :escaped-last-room    false
   :phase                :room-draw
   :result               nil})

(defn new-game
  "Starts a new game with a shuffled deck."
  []
  (make-game-state (shuffle-deck (create-deck))))

(defn new-game-with-deck
  "Starts a new game with a specific deck (for deterministic testing)."
  [deck]
  (make-game-state (vec deck)))

;; ---------------------------------------------------------------------------
;; Room dealing
;; ---------------------------------------------------------------------------

(defn deal-room
  "Draws up to 4 cards from the dungeon into the room.
   Any carried-over cards from the previous room are included.
   If dungeon, room, and carried-over are all empty, the player wins."
  [game]
  (let [carried  (:carried-over game)
        dungeon  (:dungeon game)
        n        (min (- 4 (count carried)) (count dungeon))
        new-room (vec (concat carried (take n dungeon)))]
    (if (and (empty? new-room) (empty? (:room game)))
      (assoc game :phase :game-over :result :win)
      (assoc game
             :room           new-room
             :dungeon        (vec (drop n dungeon))
             :carried-over   []
             :phase          :room-action
             :room-resolved  0))))

;; ---------------------------------------------------------------------------
;; Weapon eligibility
;; ---------------------------------------------------------------------------

(defn can-use-weapon?
  "Returns true if the player can use their weapon against the given monster.
   A weapon can be used if:
   - The player has a weapon
   - Either the weapon hasn't been used yet (weapon-last-used is nil),
     or the monster's value is strictly less than weapon-last-used."
  [game monster-card]
  (let [weapon (:weapon game)]
    (and (some? weapon)
         (let [last-used (:weapon-last-used game)]
           (or (nil? last-used)
               (< (card-value monster-card) last-used))))))

;; ---------------------------------------------------------------------------
;; Resolve card
;; ---------------------------------------------------------------------------

(defn- remove-card-at
  "Removes the card at the given index from the room."
  [room idx]
  (vec (concat (subvec room 0 idx) (subvec room (inc idx)))))

(defn- check-room-cleared
  "After resolving a card, check if the room is empty.
   If dungeon and carried-over are also empty → win.
   If room is empty but cards remain → transition to :room-draw."
  [game]
  (if (empty? (:room game))
    (if (and (empty? (:dungeon game))
             (empty? (:carried-over game)))
      (assoc game :phase :game-over :result :win)
      (assoc game :phase :room-draw))
    game))

(defn- resolve-potion
  [game card room-after]
  (-> game
      (update :hp #(min (:max-hp game) (+ % (card-value card))))
      (assoc :room room-after)
      (update :discard conj card)
      (update :room-resolved inc)
      check-room-cleared))

(defn- resolve-weapon
  [game card room-after]
  (let [old-weapon (:weapon game)]
    (-> game
        (assoc :weapon card
               :weapon-last-used nil
               :room room-after)
        (update :discard #(if old-weapon (conj % old-weapon) %))
        (update :room-resolved inc)
        check-room-cleared)))

(defn- resolve-monster
  [game card room-after use-weapon?]
  (let [monster-val  (card-value card)
        weapon       (:weapon game)
        weapon-val   (if (and use-weapon? weapon) (card-value weapon) 0)
        damage       (max 0 (- monster-val weapon-val))
        new-hp       (- (:hp game) damage)
        new-game     (-> game
                         (assoc :hp new-hp
                                :room room-after)
                         (update :discard conj card)
                         (update :room-resolved inc))]
    (if use-weapon?
      (let [g (assoc new-game :weapon-last-used monster-val)]
        (if (<= new-hp 0)
          (assoc g :phase :game-over :result :lose)
          (check-room-cleared g)))
      (if (<= new-hp 0)
        (assoc new-game :phase :game-over :result :lose)
        (check-room-cleared new-game)))))

(defn resolve-card
  "Resolves the card at the given index in the room.
   use-weapon? indicates whether to use the equipped weapon (only for monsters)."
  [game card-index use-weapon?]
  (let [card       (get (:room game) card-index)
        room-after (remove-card-at (:room game) card-index)]
    (case (card-type card)
      :potion  (resolve-potion game card room-after)
      :weapon  (resolve-weapon game card room-after)
      :monster (resolve-monster game card room-after use-weapon?))))

;; ---------------------------------------------------------------------------
;; Room ending / skipping
;; ---------------------------------------------------------------------------

(defn can-end-room?
  "Returns true if the player has resolved at least 2 cards this room."
  [game]
  (>= (:room-resolved game) 2))

(defn can-skip-remaining?
  "Returns true if the player can end the room early.
   Must have resolved >= 2 cards, and exactly 1 card remains."
  [game]
  (and (can-end-room? game)
       (= 1 (count (:room game)))))

(defn end-room
  "Ends the current room: the remaining card carries over to the next room,
   phase → :room-draw."
  [game]
  (-> game
      (assoc :carried-over (vec (:room game))
             :room []
             :escaped-last-room false
             :phase :room-draw)))

;; ---------------------------------------------------------------------------
;; Escape
;; ---------------------------------------------------------------------------

(defn can-escape?
  "Returns true if the player can escape the current room.
   The player can escape if:
   - Phase is :room-action
   - No cards have been resolved yet (room-resolved = 0)
   - The player did not escape the previous room"
  [game]
  (and (= :room-action (:phase game))
       (zero? (:room-resolved game))
       (not (:escaped-last-room game))))

(defn escape-room
  "Escapes the current room: all room cards go to the bottom of the dungeon,
   phase → :room-draw, and :escaped-last-room is set to true."
  [game]
  (-> game
      (update :dungeon into (:room game))
      (assoc :room []
             :escaped-last-room true
             :phase :room-draw)))

;; ---------------------------------------------------------------------------
;; Game state queries
;; ---------------------------------------------------------------------------

(defn game-over?
  "Returns true if the game is over."
  [game]
  (= :game-over (:phase game)))

(defn valid-actions
  "Returns a seq of available actions for the current game state.
   Each action is a map with :action key and optional params."
  [game]
  (case (:phase game)
    :room-draw   [{:action :deal-room}]
    :room-action (let [card-actions (map-indexed
                                      (fn [idx card]
                                        (let [base {:action :resolve-card :index idx :card card}]
                                          (if (and (= :monster (card-type card))
                                                   (can-use-weapon? game card))
                                            (assoc base :can-use-weapon? true)
                                            base)))
                                      (:room game))
                       end-action   (when (can-skip-remaining? game)
                                      [{:action :end-room}])
                       escape-action (when (can-escape? game)
                                       [{:action :escape-room}])]
                   (vec (concat card-actions end-action escape-action)))
    ;; :game-over or any other phase
    []))
