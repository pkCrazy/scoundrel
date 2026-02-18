(ns game.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [game.core :as core]))

;; ---------------------------------------------------------------------------
;; Card utilities
;; ---------------------------------------------------------------------------

(deftest test-card-value
  (testing "Number cards return their rank"
    (is (= 2 (core/card-value {:suit :hearts :rank 2})))
    (is (= 5 (core/card-value {:suit :clubs :rank 5})))
    (is (= 10 (core/card-value {:suit :spades :rank 10}))))
  (testing "Face cards and ace"
    (is (= 11 (core/card-value {:suit :clubs :rank 11})))
    (is (= 12 (core/card-value {:suit :spades :rank 12})))
    (is (= 13 (core/card-value {:suit :clubs :rank 13})))
    (is (= 14 (core/card-value {:suit :spades :rank 14})))))

(deftest test-card-type
  (testing "Hearts are potions"
    (is (= :potion (core/card-type {:suit :hearts :rank 5}))))
  (testing "Diamonds are weapons"
    (is (= :weapon (core/card-type {:suit :diamonds :rank 7}))))
  (testing "Clubs are monsters"
    (is (= :monster (core/card-type {:suit :clubs :rank 10}))))
  (testing "Spades are monsters"
    (is (= :monster (core/card-type {:suit :spades :rank 14})))))

;; ---------------------------------------------------------------------------
;; Deck creation
;; ---------------------------------------------------------------------------

(deftest test-create-deck
  (let [deck (core/create-deck)]
    (testing "Deck has 44 cards (52 - 6 red face cards - 2 red aces)"
      (is (= 44 (count deck))))
    (testing "No red face cards (J/Q/K of hearts or diamonds)"
      (let [red-faces (filter #(and (#{:hearts :diamonds} (:suit %))
                                    (>= (:rank %) 11))
                              deck)]
        (is (= 0 (count red-faces)))))
    (testing "All clubs and spades A-K present (26 cards)"
      (let [black (filter #(#{:clubs :spades} (:suit %)) deck)]
        (is (= 26 (count black)))))
    (testing "Hearts and diamonds 2-10 present (18 cards)"
      (let [red (filter #(#{:hearts :diamonds} (:suit %)) deck)]
        (is (= 18 (count red)))
        (is (every? #(<= 2 (:rank %) 10) red))))))

;; ---------------------------------------------------------------------------
;; Game initialization
;; ---------------------------------------------------------------------------

(deftest test-new-game
  (let [game (core/new-game)]
    (testing "Player starts with 20 HP"
      (is (= 20 (:hp game))))
    (testing "Max HP is 20"
      (is (= 20 (:max-hp game))))
    (testing "No weapon equipped"
      (is (nil? (:weapon game))))
    (testing "Dungeon has 44 cards"
      (is (= 44 (count (:dungeon game)))))
    (testing "Room is empty"
      (is (empty? (:room game))))
    (testing "Discard is empty"
      (is (empty? (:discard game))))
    (testing "Phase is :room-draw"
      (is (= :room-draw (:phase game))))))

(deftest test-new-game-with-deck
  (let [deck (take 8 (core/create-deck))
        game (core/new-game-with-deck deck)]
    (testing "Uses the provided deck as dungeon"
      (is (= deck (:dungeon game))))
    (testing "HP is 20"
      (is (= 20 (:hp game))))))

;; ---------------------------------------------------------------------------
;; Room dealing
;; ---------------------------------------------------------------------------

(deftest test-deal-room
  (let [deck [{:suit :clubs :rank 2}
              {:suit :hearts :rank 3}
              {:suit :diamonds :rank 4}
              {:suit :spades :rank 5}
              {:suit :clubs :rank 6}
              {:suit :hearts :rank 7}
              {:suit :diamonds :rank 8}
              {:suit :spades :rank 9}]
        game (core/new-game-with-deck deck)
        dealt (core/deal-room game)]
    (testing "Room has 4 cards"
      (is (= 4 (count (:room dealt)))))
    (testing "Dungeon has 4 fewer cards"
      (is (= 4 (count (:dungeon dealt)))))
    (testing "Phase transitions to :room-action"
      (is (= :room-action (:phase dealt))))
    (testing "Room resolved counter is 0"
      (is (= 0 (:room-resolved dealt))))
    (testing "Room monsters skipped is 0"
      (is (= 0 (:room-monsters-skipped dealt))))))

(deftest test-deal-room-fewer-than-4
  (let [deck [{:suit :clubs :rank 2}
              {:suit :hearts :rank 3}]
        game (core/new-game-with-deck deck)
        dealt (core/deal-room game)]
    (testing "Room gets remaining cards when fewer than 4"
      (is (= 2 (count (:room dealt))))
      (is (= 0 (count (:dungeon dealt)))))))

(deftest test-deal-room-empty-dungeon-wins
  (let [game (core/new-game-with-deck [])
        dealt (core/deal-room game)]
    (testing "Empty dungeon with empty room → game over, win"
      (is (= :game-over (:phase dealt)))
      (is (= :win (:result dealt))))))

;; ---------------------------------------------------------------------------
;; Resolve card — potions
;; ---------------------------------------------------------------------------

(deftest test-resolve-potion
  (let [game (-> (core/new-game-with-deck [])
                 (assoc :room [{:suit :hearts :rank 5}
                               {:suit :clubs :rank 3}
                               {:suit :diamonds :rank 4}
                               {:suit :spades :rank 6}]
                        :hp 15
                        :phase :room-action
                        :room-resolved 0
                        :room-monsters-skipped 0))
        result (core/resolve-card game 0 false)]
    (testing "Potion heals by card value"
      (is (= 20 (:hp result))))
    (testing "Card removed from room"
      (is (= 3 (count (:room result)))))
    (testing "Room resolved incremented"
      (is (= 1 (:room-resolved result))))))

(deftest test-resolve-potion-caps-at-max-hp
  (let [game (-> (core/new-game-with-deck [])
                 (assoc :room [{:suit :hearts :rank 10}]
                        :hp 18
                        :phase :room-action
                        :room-resolved 0
                        :room-monsters-skipped 0))
        result (core/resolve-card game 0 false)]
    (testing "HP cannot exceed max-hp"
      (is (= 20 (:hp result))))))

;; ---------------------------------------------------------------------------
;; Resolve card — weapons
;; ---------------------------------------------------------------------------

(deftest test-resolve-weapon
  (let [game (-> (core/new-game-with-deck [])
                 (assoc :room [{:suit :diamonds :rank 7}
                               {:suit :clubs :rank 3}]
                        :phase :room-action
                        :room-resolved 0
                        :room-monsters-skipped 0))
        result (core/resolve-card game 0 false)]
    (testing "Weapon is equipped"
      (is (= {:suit :diamonds :rank 7} (:weapon result))))
    (testing "Weapon last used is nil (fresh weapon)"
      (is (nil? (:weapon-last-used result))))
    (testing "Card removed from room"
      (is (= 1 (count (:room result)))))))

(deftest test-resolve-weapon-replaces-old
  (let [game (-> (core/new-game-with-deck [])
                 (assoc :room [{:suit :diamonds :rank 9}
                               {:suit :clubs :rank 3}]
                        :weapon {:suit :diamonds :rank 5}
                        :weapon-last-used 3
                        :phase :room-action
                        :room-resolved 0
                        :room-monsters-skipped 0))
        result (core/resolve-card game 0 false)]
    (testing "New weapon is equipped"
      (is (= {:suit :diamonds :rank 9} (:weapon result))))
    (testing "Old weapon goes to discard"
      (is (some #(= {:suit :diamonds :rank 5} %) (:discard result))))
    (testing "Weapon last used resets"
      (is (nil? (:weapon-last-used result))))))

;; ---------------------------------------------------------------------------
;; Resolve card — monsters
;; ---------------------------------------------------------------------------

(deftest test-resolve-monster-bare-handed
  (let [game (-> (core/new-game-with-deck [])
                 (assoc :room [{:suit :clubs :rank 8}
                               {:suit :hearts :rank 3}]
                        :hp 20
                        :phase :room-action
                        :room-resolved 0
                        :room-monsters-skipped 0))
        result (core/resolve-card game 0 false)]
    (testing "Takes full damage without weapon"
      (is (= 12 (:hp result))))
    (testing "Card removed from room"
      (is (= 1 (count (:room result)))))))

(deftest test-resolve-monster-with-weapon
  (let [game (-> (core/new-game-with-deck [])
                 (assoc :room [{:suit :clubs :rank 8}
                               {:suit :hearts :rank 3}]
                        :hp 20
                        :weapon {:suit :diamonds :rank 6}
                        :weapon-last-used nil
                        :phase :room-action
                        :room-resolved 0
                        :room-monsters-skipped 0))
        result (core/resolve-card game 0 true)]
    (testing "Damage is reduced by weapon value"
      (is (= 18 (:hp result))))
    (testing "Weapon last used is set to monster value"
      (is (= 8 (:weapon-last-used result))))))

(deftest test-resolve-monster-weapon-overkill-no-negative-damage
  (let [game (-> (core/new-game-with-deck [])
                 (assoc :room [{:suit :clubs :rank 3}]
                        :hp 20
                        :weapon {:suit :diamonds :rank 10}
                        :weapon-last-used nil
                        :phase :room-action
                        :room-resolved 0
                        :room-monsters-skipped 0))
        result (core/resolve-card game 0 true)]
    (testing "Weapon stronger than monster → 0 damage"
      (is (= 20 (:hp result))))))

(deftest test-resolve-monster-weapon-degradation
  (let [game (-> (core/new-game-with-deck [])
                 (assoc :room [{:suit :clubs :rank 5}]
                        :hp 20
                        :weapon {:suit :diamonds :rank 10}
                        :weapon-last-used 8
                        :phase :room-action
                        :room-resolved 0
                        :room-monsters-skipped 0))]
    (testing "Can use weapon against monster with value < weapon-last-used"
      (is (core/can-use-weapon? game (first (:room game)))))
    (let [result (core/resolve-card game 0 true)]
      (testing "Weapon last used updated to current monster"
        (is (= 5 (:weapon-last-used result)))))))

(deftest test-cannot-use-weapon-against-equal-or-higher
  (let [game (-> (core/new-game-with-deck [])
                 (assoc :room [{:suit :clubs :rank 8}]
                        :weapon {:suit :diamonds :rank 10}
                        :weapon-last-used 8
                        :phase :room-action
                        :room-resolved 0
                        :room-monsters-skipped 0))]
    (testing "Cannot use weapon against monster >= weapon-last-used"
      (is (not (core/can-use-weapon? game (first (:room game))))))))

(deftest test-resolve-monster-lethal
  (let [game (-> (core/new-game-with-deck [])
                 (assoc :room [{:suit :spades :rank 14}]
                        :hp 10
                        :phase :room-action
                        :room-resolved 0
                        :room-monsters-skipped 0))
        result (core/resolve-card game 0 false)]
    (testing "HP drops below 0 → game over, lose"
      (is (= :game-over (:phase result)))
      (is (= :lose (:result result)))
      (is (>= 0 (:hp result))))))

;; ---------------------------------------------------------------------------
;; Room ending / skipping
;; ---------------------------------------------------------------------------

(deftest test-can-end-room
  (testing "Cannot end room with fewer than 2 resolved"
    (let [game {:room-resolved 1 :room [{:suit :clubs :rank 2}] :room-monsters-skipped 0}]
      (is (not (core/can-end-room? game)))))
  (testing "Can end room with 2+ resolved"
    (let [game {:room-resolved 2 :room [{:suit :clubs :rank 2}] :room-monsters-skipped 0}]
      (is (core/can-end-room? game)))))

(deftest test-can-skip-remaining
  (testing "Can skip when no monsters remain"
    (let [game {:room-resolved 2
                :room [{:suit :hearts :rank 3}]
                :room-monsters-skipped 0}]
      (is (core/can-skip-remaining? game))))
  (testing "Can skip when 1 monster remains"
    (let [game {:room-resolved 2
                :room [{:suit :clubs :rank 5}]
                :room-monsters-skipped 0}]
      (is (core/can-skip-remaining? game))))
  (testing "Cannot skip when 2 monsters remain"
    (let [game {:room-resolved 2
                :room [{:suit :clubs :rank 5} {:suit :spades :rank 3}]
                :room-monsters-skipped 0}]
      (is (not (core/can-skip-remaining? game))))))

(deftest test-end-room
  (let [game (-> (core/new-game-with-deck [{:suit :clubs :rank 10}
                                            {:suit :hearts :rank 2}
                                            {:suit :diamonds :rank 3}
                                            {:suit :spades :rank 4}])
                 (assoc :room [{:suit :hearts :rank 5}]
                        :room-resolved 3
                        :room-monsters-skipped 0
                        :phase :room-action))
        result (core/end-room game)]
    (testing "Remaining room cards go to discard"
      (is (empty? (:room result)))
      (is (some #(= {:suit :hearts :rank 5} %) (:discard result))))
    (testing "Phase transitions to room-draw"
      (is (= :room-draw (:phase result))))))

;; ---------------------------------------------------------------------------
;; Escape
;; ---------------------------------------------------------------------------

(deftest test-can-escape
  (testing "Can escape at start of room (nothing resolved, didn't escape last room)"
    (let [game (-> (core/new-game-with-deck [{:suit :clubs :rank 2}
                                              {:suit :hearts :rank 3}
                                              {:suit :diamonds :rank 4}
                                              {:suit :spades :rank 5}
                                              {:suit :clubs :rank 6}
                                              {:suit :hearts :rank 7}
                                              {:suit :diamonds :rank 8}
                                              {:suit :spades :rank 9}])
                   (core/deal-room))]
      (is (core/can-escape? game))))
  (testing "Cannot escape after resolving a card"
    (let [game (-> (core/new-game-with-deck [{:suit :hearts :rank 2}
                                              {:suit :hearts :rank 3}
                                              {:suit :diamonds :rank 4}
                                              {:suit :spades :rank 5}
                                              {:suit :clubs :rank 6}])
                   (core/deal-room)
                   (core/resolve-card 0 false))]
      (is (not (core/can-escape? game)))))
  (testing "Cannot escape if escaped last room"
    (let [game (-> (core/new-game-with-deck [{:suit :clubs :rank 2}
                                              {:suit :hearts :rank 3}
                                              {:suit :diamonds :rank 4}
                                              {:suit :spades :rank 5}
                                              {:suit :clubs :rank 6}
                                              {:suit :hearts :rank 7}
                                              {:suit :diamonds :rank 8}
                                              {:suit :spades :rank 9}])
                   (core/deal-room)
                   (core/escape-room)
                   (core/deal-room))]
      (is (not (core/can-escape? game))))))

(deftest test-escape-room
  (let [room-cards [{:suit :clubs :rank 2}
                    {:suit :hearts :rank 3}
                    {:suit :diamonds :rank 4}
                    {:suit :spades :rank 5}]
        remaining  [{:suit :clubs :rank 6}
                    {:suit :hearts :rank 7}]
        game (-> (core/new-game-with-deck (concat room-cards remaining))
                 (core/deal-room))
        result (core/escape-room game)]
    (testing "Room is cleared"
      (is (empty? (:room result))))
    (testing "Room cards go to bottom of dungeon"
      (is (= (concat remaining room-cards) (:dungeon result))))
    (testing "Phase transitions to room-draw"
      (is (= :room-draw (:phase result))))
    (testing "escaped-last-room is true"
      (is (true? (:escaped-last-room result))))))

(deftest test-escape-resets-after-normal-room
  (let [game (-> (core/new-game-with-deck [{:suit :hearts :rank 2}
                                            {:suit :hearts :rank 3}
                                            {:suit :hearts :rank 4}
                                            {:suit :hearts :rank 5}
                                            {:suit :hearts :rank 6}
                                            {:suit :hearts :rank 7}
                                            {:suit :hearts :rank 8}
                                            {:suit :hearts :rank 9}
                                            {:suit :hearts :rank 10}
                                            {:suit :diamonds :rank 2}
                                            {:suit :diamonds :rank 3}
                                            {:suit :diamonds :rank 4}])
                 ;; Room 1: escape
                 (core/deal-room)
                 (core/escape-room)
                 ;; Room 2: deal and resolve 2, then end normally
                 (core/deal-room)
                 (core/resolve-card 0 false)
                 (core/resolve-card 0 false)
                 (core/end-room)
                 ;; Room 3: should be able to escape again
                 (core/deal-room))]
    (testing "Can escape again after a normal room completion"
      (is (core/can-escape? game)))))

;; ---------------------------------------------------------------------------
;; Win / lose scenarios
;; ---------------------------------------------------------------------------

(deftest test-win-scenario
  (let [;; Tiny dungeon: 4 potions only
        deck [{:suit :hearts :rank 2}
              {:suit :hearts :rank 3}
              {:suit :hearts :rank 4}
              {:suit :hearts :rank 5}]
        game (-> (core/new-game-with-deck deck)
                 (core/deal-room))
        ;; Resolve all 4 potions
        g1 (core/resolve-card game 0 false)
        g2 (core/resolve-card g1 0 false)
        g3 (core/resolve-card g2 0 false)
        g4 (core/resolve-card g3 0 false)]
    (testing "Resolving all cards in last room triggers win"
      (is (= :game-over (:phase g4)))
      (is (= :win (:result g4))))))

(deftest test-lose-scenario
  (let [deck [{:suit :spades :rank 14}
              {:suit :clubs :rank 13}
              {:suit :clubs :rank 12}
              {:suit :clubs :rank 11}]
        game (-> (core/new-game-with-deck deck)
                 (core/deal-room))
        ;; Fight the Ace bare-handed (14 damage → HP 6)
        g1 (core/resolve-card game 0 false)
        ;; Fight the King bare-handed (13 damage → HP -7)
        g2 (core/resolve-card g1 0 false)]
    (testing "HP <= 0 means game over, lose"
      (is (= :game-over (:phase g2)))
      (is (= :lose (:result g2))))))

;; ---------------------------------------------------------------------------
;; Valid actions
;; ---------------------------------------------------------------------------

(deftest test-valid-actions-room-draw
  (let [game (core/new-game-with-deck [{:suit :clubs :rank 2}
                                        {:suit :hearts :rank 3}
                                        {:suit :diamonds :rank 4}
                                        {:suit :spades :rank 5}])]
    (testing "In room-draw phase, only action is :deal-room"
      (is (= #{:deal-room} (set (map :action (core/valid-actions game))))))))

(deftest test-valid-actions-room-action
  (let [game (-> (core/new-game-with-deck [{:suit :clubs :rank 2}
                                            {:suit :hearts :rank 3}
                                            {:suit :diamonds :rank 4}
                                            {:suit :spades :rank 5}])
                 (core/deal-room))]
    (testing "In room-action phase, can resolve any card"
      (let [actions (core/valid-actions game)
            resolve-actions (filter #(= :resolve-card (:action %)) actions)]
        (is (= 4 (count resolve-actions)))))
    (testing "Escape action is available in a fresh room"
      (let [actions (core/valid-actions game)
            escape-actions (filter #(= :escape-room (:action %)) actions)]
        (is (= 1 (count escape-actions)))))))

(deftest test-valid-actions-game-over
  (let [game {:phase :game-over :result :win}]
    (testing "No actions when game is over"
      (is (empty? (core/valid-actions game))))))