;; Copyright © 2020 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning

(ns pamela.tools.Qlearning.DMQL
  "Q-Learning"
  (:require [clojure.string :as string]
            [clojure.repl :refer [pst]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.tools.reader :as reader]
            [clojure.java.io :as io]
            [clojure.core :as cc]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]
            [environ.core :refer [env]]
            [pamela.tools.Qlearning.GYMinterface :as gym])
  (:gen-class))

;(in-ns 'pamela.tools.Qlearning.DMQL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning

(defn initialize-learner
  "Establishes the data structure that governs the operation of the learner."
  [cycletime max-steps mode learningrate discount epsilon episodes explore ssdi numobs numacts initialQ platform]
  {
   :q-table (atom initialQ)             ; Q-table
   :cycletime cycletime                 ; cycle time in milliseconds
   :mode mode                           ; Choose action selection mode
   :max-steps max-steps                 ; Maximum number of steps in an episode
   :alpha learningrate                  ; 0 lt learningrate lt 1
   :gamma discount                      ; 0 lt discount lt 1
   :epsilon epsilon                     ; Starting point for explore
   :episodes episodes                   ; number of episodes
   :explore explore
   :discretization ssdi
   :numobs numobs
   :numacts numacts
   :platform platform
   })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Q-Table representation

(defn make-fixed-sized-q-table-uniform-random
  "Q-table constructor that fills the q-table with random values."
  [num-state-variables discretization num-actions low high]
  (if (= num-state-variables 0)
    (vec (doall (repeatedly num-actions
                            (fn [] (atom
                                    (+ low (* (- high low)(rand))))))))
    (vec (doall (repeatedly discretization
                            (fn [] (make-fixed-sized-q-table-uniform-random
                                    (- num-state-variables 1) discretization num-actions low high)))))))

(defn fixed-sized-q-value
  "Helper function for q-table indexing."
  [q-table & args]
  (reduce nth q-table args))

(defn get-all-actions-quality
  [learner d-state]
  (apply fixed-sized-q-value (deref (:q-table learner)) d-state))

(defn get-action-quality
  "Return the atom representing the Q value for the given action in the given discretized state."
  [learner d-state action]
  (fixed-sized-q-value (apply fixed-sized-q-value (deref (:q-table learner)) d-state) action))

(defn deatomize-q-table
  [qtable]
  ;;(println "In deatomize-q-table with: " qtable)
  (cond (coll? qtable)
        (vec (doall (map deatomize-q-table qtable)))

        (= (type qtable) (type (atom 42)))
        (deref qtable)

        :otherwise qtable))

(defn save-q-table
  "Save the supplied Q-Table using a filename that includes the episode number."
  [q-table episode name]
  (let [fn (str name episode "-q-table.edn")]
    (with-open [w (io/writer fn)]
      (binding [*out* w]
        (println ";;; Readable EDN Q-Table after episode " episode)
        (pr (deatomize-q-table q-table))))
    fn))

(defn reatomize-q-table
  [qtable]
  ;;(println "In deatomize-q-table with: " qtable)
  (cond (coll? qtable)
        (vec (doall (map reatomize-q-table qtable)))

        (number? qtable)
        (atom qtable)

        :otherwise qtable))

(defn read-q-table
  "Read a Q-Table previously written out in EDN format;"
  [filename]
  (with-open [r (java.io.PushbackReader. (io/reader filename))]
    (reatomize-q-table (edn/read r)))) ;(clojure.java.io.PushbackReader. r)

;;; (def test-q-table (make-fixed-sized-q-table-uniform-random 2 3 2 -2 0))
;;; (pprint (make-fixed-sized-q-table-uniform-random 2 3 2 -2 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State space discretization

(defn win-size
  "Compute the vector of window sizes for each state variable according to the discretization factor."
  [numobs ssdi]
  (vec (map (fn [high low] (/ (- high low) ssdi)) (gym/get-obs-high numobs) (gym/get-obs-low numobs))))

(defn get-discrete-state
  "Given a raw state, discretize the state."
  [state obslow disc-os-win-size]
  ;; (println "state=" state "low=" obslow "win=" disc-os-win-size)
  (let [discstate (vec
                   (doall
                    (map (fn [state low winsize] (int (/ (- state low) winsize)))
                         state obslow disc-os-win-size)))]
    ;; (println "discstate=" discstate)
    discstate))

(defn max-atom
  "Given a list of Q-table entries representing, in order, the available actions, select the greatest quality."
  [list-of-atoms]
  ;;(println "max-atom called with " list-of-atoms)
  (apply max (map deref list-of-atoms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statistics

(defn save-statistics
  "Save statistics to a named file."
  [stats episode name]
  (let [fn (str name "-learning-statistics.edn")]
    (with-open [w (io/writer fn)]
      (binding [*out* w]
        (println ";;; Readable EDN Statistics")
        (pr stats)))
    fn))

(def successes 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Action Selection

(defn textbook-action-selector-with-epsilon-randomization
  "Select an action either randomly according to epsilon or the best."
  [list-of-atoms numacts eps]
  ;; (println "In action-selector with list-of-atoms=" list-of-atoms)
  (if (> (rand) eps)
    (let [best (apply max-key (fn [x] (deref x)) list-of-atoms)]
      ;; (println "Best of" list-of-atoms "is " best)
      (.indexOf list-of-atoms best))
    (int (* (rand) numacts))))

(defn mc-select-nth
  [atoms eps]
  (let [vals (map deref atoms)
        powr (* 4.0 (- 1 eps))          ;eps varies from 1 to 0, Powr varies from 0 to 4
        minv (reduce min vals)
        maxv (reduce max vals)
        span (float (- maxv minv))
        adjustment (- (/ span (+ 1 powr)) minv)
        rebased (map (fn [n] (math/expt (/ (+ n adjustment) span) powr)) vals)
        ;; rebased (map (fn [n] (* (/ (+ n adjustment) span) (/ (+ n adjustment) span))) vals)
        total (reduce + rebased)
        rn (* (rand) total)]
    ;;(println "vals=" vals "rebased=" rebased "minv=" minv "maxv=" maxv "total=" total "rn=" rn)
    (loop [order 0
           rvals rebased
           rnum rn]
      ;;(println "order=" order "rval=" rvals "rnum=" rnum)
      (if (or (< rnum (first rvals)) (= (rest rvals) ()))
        order
        (recur (+ order 1) (rest rvals) (- rnum (first rvals)))))))

(defn monte-carlo-action-selector
  "Select an action using monte-carlo sampling."
  [list-of-atoms numacts eps epsilon mode] ; epsilon is epsilon randomness, eps is Monte-Carlo confidence
  ;; (println "In action-selector with list-of-atoms=" list-of-atoms)
  (if (and (= mode 2) (< (rand) epsilon))
    (int (* (rand) numacts))
    (mc-select-nth list-of-atoms eps)))

(def discrepencies 0)
(def comparisons 0)

(defn select-action
  "Select an action using algorithm selected by --mode."
  [learner dstate epsilon]
  (let [{q-table :q-table
         mode    :mode
         numacts :numacts} learner
        all-actions (get-all-actions-quality learner dstate)]
    (case mode
      ;; Mode 0, the default is the textbook selection with epsilon randomized selection.
      0 (textbook-action-selector-with-epsilon-randomization all-actions numacts epsilon)

      ;; Mode 1, DOLL Monte-Carlo selection based on a Bayesian model of knowledge (Preferred!)
      1 (monte-carlo-action-selector all-actions numacts epsilon 0.0 mode)

      ;; Mode 2, Same as above overlaid with epsilon randomization (just for comparison purposes)
      2 (monte-carlo-action-selector all-actions numacts epsilon epsilon mode)

      ;; Compare mode
      3 (let [tb (textbook-action-selector-with-epsilon-randomization all-actions numacts epsilon)
              mc (monte-carlo-action-selector all-actions numacts epsilon 0.0 mode)
              aa (map deref all-actions)]
          (def comparisons (+ 1 comparisons))
          (if (not (= mc tb))
            (do
              (def discrepencies (+ 1 discrepencies))
              (println "*** Voting on " aa "tb=" tb "mc=" mc "discrepencies=" discrepencies "/" comparisons)))
          tb)

      ;; Any unspecified mode uses textbook select "best" without epsilon randomization nor Monte-Carlo.
      (textbook-action-selector-with-epsilon-randomization all-actions numacts 0.0)))) ; Practically useless!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Training

(defn run-episode
  "Train a single episode."
  [learner episode epsilon]
  (let [{;; Pull out all of the pieces aheadof the loop
         mode      :mode
         qt        :q-table
         cycletime :cycletime
         max-steps :max-steps
         alpha     :alpha
         gamma     :gamma
         ssdi      :discretization
         numobs    :numobs
         numacts   :numacts
         platform  :platform} learner
        q-table (deref qt)
        current-state ((:get-current-state platform) platform numobs)
        obslow (gym/get-obs-low numobs)
        disc-os-size (vec (repeatedly numobs (fn [] ssdi)))
        disc-os-win-size (win-size numobs ssdi)
        discstate (get-discrete-state current-state obslow disc-os-win-size)
        ]
    (loop [current-d-state discstate
           donep false
           ereward 0
           step 0]
      ;; (println "state = " discstate)
      (if (not donep)
        (let [;; Select a action
              action (select-action learner current-d-state epsilon)
              _ ((:perform platform) platform action)
              _ (Thread/sleep 10) ; was cycletime
              new-state ((:get-current-state platform) platform numobs)
              reward ((:get-field-value platform) platform :reward)
              episode-done ((:get-field-value platform) platform :done)
              new-d-state (get-discrete-state  new-state obslow disc-os-win-size)]
          (if (= 0 (mod episode 1000)) ((:render platform) platform)) ;+++
          ;; (println "step=" step "Action=" action "state=" new-state "reward=" reward "done?=" episode-done "disc.State=" new-d-state)
          (cond
            (and (not episode-done) (not (>= step max-steps)))
            (let [max-future-q (max-atom (get-all-actions-quality learner new-d-state))
                  q-pos (get-action-quality learner current-d-state action)
                  current-q (deref q-pos)
                  ;;_ (println "alpha=" alpha "currentQ=" current-q "reward=" reward "gamma=" gamma "max-future-q=" max-future-q)
                  ;; Bellman's Equation
                  new-q (+ (* (- 1.0 alpha) current-q) (* alpha (+ reward (* gamma max-future-q))))]
              (reset! q-pos new-q))

            ((:goal-achieved platform) platform new-state episode-done)
            (let [q-pos (get-action-quality learner new-d-state action)]
              (reset! q-pos (+ reward (+ 1 (/ (float episode) max-steps)))) ; was (+ ereward reward)
              (def successes (+ 1 successes))
              (println "*** Success #" successes "on step" step "in" episode "episodes ***")))
          (recur  new-d-state episode-done (+ ereward reward) (+ step 1)))
        [ereward step]))))  ; some episodes end early and can lead to incorrect average calculations.

(defn train
  "Train with a given number of episodes, saving statistics and q-tables at regular intervals."
  [learner]
  (let [{episodes :episodes
         epsilon  :epsilon
         explore  :explore
         platform :platform
         q-table  :q-table} learner
        ;;epsilon  0.25
        stats-every 100 ;+++
        save-every 100 ;+++
        start-eps-decay 1
        end-eps-decay (int (* episodes explore))
        decay-by (/ epsilon (- end-eps-decay start-eps-decay))
        maxreward (atom :unset)
        minreward (atom :unset)
        totalreward (atom :unset)
        learning-history (atom [])]
    ;;(pprint learner)
    (dotimes [episode episodes]
      ;; Setup the simulator
      (let [eps (if (> episode end-eps-decay) 0 (- epsilon (* episode decay-by)))]
        (if (= 0 (mod eps 10)) (print "*** Starting Episode " episode "Epsilon=" eps "\r"))
        ((:reset platform) platform)
        (Thread/sleep 10)               ;Reset the platform and give it time to settle down
        (let [[reward steps] (run-episode learner episode eps)]
          (if (= 0 (mod episode stats-every))
            (do (if (> episode 0)
                  (do
                    (println "Episode=" episode
                             "Epsilon=" eps
                             "Max reward=" (deref maxreward)
                             "Average reward=" (/ (deref totalreward) stats-every)
                             "Min reward=" (deref minreward))
                    (reset! learning-history (conj (deref learning-history)
                                                   {:episode episode
                                                    :max-reward (deref maxreward)
                                                    :min-reward (deref minreward)
                                                    :average-reward (/ (deref totalreward) stats-every)}))
                    (println "Saved statistics as: " ; maybe write as CSV file?
                             (save-statistics (deref learning-history) episode "DMQL"))))
                ;; The first result of each set sets the starting values.
                (reset! maxreward reward)
                (reset! minreward reward)
                (reset! totalreward reward))
            (do
              (if (> reward (deref maxreward)) (reset! maxreward reward))
              (if (> (deref minreward) reward) (reset! minreward reward))
              (reset! totalreward (+ (deref totalreward) reward))))
          (if (and (> episode 0) (= 0 (mod episode save-every)))
            (println "Saved Q Table as: " (save-q-table (deref q-table) episode "DMQL"))))))))

;;; Fin
