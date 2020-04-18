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
            [environ.core :refer [env]]
            [pamela.tools.Qlearning.GYMinterface :as gym])
  (:gen-class))

;(in-ns 'pamela.tools.Qlearning.DMQL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning

(defn initialize-learner
  [cycletime learningrate discount epsilon episodes explore ssdi numobs numacts initialQ platform]
  {
   :q-table (atom initialQ)             ; Q-table
   :cycletime cycletime                 ; cycle time in milliseconds
   :alpha learningrate                  ; 0 lt learningrate lt 1
   :gamma discount                      ; 0 lt discount lt 1
   :epsilon epsilon                     ; Starting point for explore
   :episodes episodes                   ; number of episodes
   :explore explore
   :discretization ssdi
   :numobs numobs
   :numacts numacts
   :platform platform
   ;;; below moved to DPL interface
   ;;:reset-state gym/reset-state             ; function returns starting discrete-state
   ;;:perform gym/perform
   ;;:reset gym/reset
   ;;:initialize-plant gym/initialize-simulator
   ;;:render gym/render
   ;;:shutdown gym/shutdown
   ;;:discretize-state gym/get-discrete-state
   ;;:goal-achieved gym/goal-achieved
   ;;; this belongs in the platform -- or does it?
   ;;;:actionselectfunction gym/actionselector ; function that selects an action.
   })

(defn actionselector
  [list-of-atoms numacts eps]
  ;; (println "In action-selector with list-of-atoms=" list-of-atoms)
  (if (> (rand) eps)
    (let [best (apply max-key (fn [x] (deref x)) list-of-atoms)]
      ;; (println "Best of" list-of-atoms "is " best)
      (.indexOf list-of-atoms best))
    (int (* (rand) numacts))))

;;;
(defn make-fixed-sized-q-table-uniform-random
  [num-state-variables discretization num-actions low high]
  (if (= num-state-variables 0)
    (vec (doall (repeatedly num-actions
                            (fn [] (atom
                                    (+ low (* (- high low)(rand))))))))
    (vec (doall (repeatedly discretization
                            (fn [] (make-fixed-sized-q-table-uniform-random
                                    (- num-state-variables 1) discretization num-actions low high)))))))

(defn fixed-sized-q-value
  [q-table & args]
  (reduce nth q-table args))

;;; (def test-q-table (make-fixed-sized-q-table-uniform-random 2 3 2 -2 0))

;;; (pprint (make-fixed-sized-q-table-uniform-random 2 3 2 -2 0))

(defn max-atom
  [list-of-atoms]
  ;;(println "max-atom called with " list-of-atoms)
  (apply max (map deref list-of-atoms)))

(defn win-size
  [numobs ssdi]
  (vec (map (fn [high low] (/ (- high low) ssdi)) (gym/get-obs-high numobs) (gym/get-obs-low numobs))))

(defn get-discrete-state
  [state obslow disc-os-win-size]
  ;; (println "state=" state "low=" obslow "win=" disc-os-win-size)
  (let [discstate (vec
                   (doall
                    (map (fn [state low winsize] (int (/ (- state low) winsize)))
                         state obslow disc-os-win-size)))]
    ;; (println "discstate=" discstate)
    discstate))

(def successes 0)

(defn run-episode
  [learner episode epsilon]
  (let [{;; Pull out all of the pieces aheadof the loop
         qt :q-table
         cycletime :cycletime
         alpha   :alpha
         gamma   :gamma
         ssdi    :discretization
         numobs  :numobs
         numacts :numacts
         platform :platform} learner
        q-table (deref qt)
        current-state ((:get-current-state platform) platform numobs)
        obslow (gym/get-obs-low numobs)
        disc-os-size (vec (repeatedly numobs (fn [] ssdi)))
        disc-os-win-size (win-size numobs ssdi)
        discstate (get-discrete-state current-state obslow disc-os-win-size)
        ]
    (loop [state discstate
           donep false
           ereward 0
           prevreward 0
           step 1]
      ;; (println "state = " discstate)
      (if (not donep)
        (let [;; Select a action
              action (actionselector (apply fixed-sized-q-value q-table state) numacts epsilon)
              ;; Perform action
              _ ((:perform platform) platform action)
              _ (Thread/sleep 10) ; was cycletime
              new-state ((:get-current-state platform) platform numobs)
              reward ((:get-field-value platform) platform :reward)
              episode-done ((:get-field-value platform) platform :done)
              discrete-state (get-discrete-state  new-state obslow disc-os-win-size)]
          (if (= 0 (mod episode 1000)) ((:render platform) platform)) ;+++
          ;; (println "step=" step "Action=" action "state=" new-state "reward=" reward "done?=" episode-done "disc.State=" discrete-state)
          (cond
            (not episode-done)
            (let [max-future-q (max-atom (apply fixed-sized-q-value q-table discrete-state))
                  q-pos (fixed-sized-q-value (apply fixed-sized-q-value q-table state) action)
                  current-q (deref q-pos)
                  ;;_ (println "alpha=" alpha "currentQ=" current-q "reward=" reward "gamma=" gamma "max-future-q=" max-future-q)
                  ;; Bellman's Equation
                  new-q (+ (* (- 1.0 alpha) current-q) (* alpha (+ reward (* gamma max-future-q))))]
              (reset! q-pos new-q))

            ((:goal-achieved platform) platform new-state episode-done)
            (let [q-pos (fixed-sized-q-value (apply fixed-sized-q-value q-table discrete-state) action)]
              (reset! q-pos (+ ereward reward))
              (def successes (+ 1 successes))
              (println "*** Success #" successes "on step" step "in" episode "episodes ***")
              ))
          ;;(Thread/sleep 1000) ;; Slow it down for debugging
          (recur  discrete-state episode-done (+ ereward reward) reward (+ step 1)))
        prevreward))))

(defn train
  [learner]
  (let [{episodes :episodes
         epsilon  :epsilon
         explore  :explore
         platform :platform} learner
        stats-every 500 ;+++
        start-eps-decay 1
        end-eps-decay (int (* episodes explore))
        decay-by (/ epsilon (- end-eps-decay start-eps-decay))
        maxreward (atom -1000)
        minreward (atom 1000)
        totalreward (atom 0)
        ]
    (dotimes [episode episodes]
      ;; Setup the simulator
      (let [eps (if (> episode end-eps-decay) 0 (- epsilon (* episode decay-by)))]
        (if (= 0 (mod eps 100)) (println "*** Starting Episode " episode "Epsilon=" eps))
        ((:reset platform) platform)
        (Thread/sleep 100)               ;Reset the platform and give it time to settle down
        (let [reward (run-episode learner episode eps)]
          (if (= 0 (mod (+ 1 episode) stats-every))
            (do (println "Episode=" episode
                         "Epsilon=" eps
                         "Max reward=" (deref maxreward)
                         "Average reward=" (/ (deref totalreward) stats-every)
                         "Min reward=" (deref minreward))
                (reset! maxreward -1000)
                (reset! minreward 1000)
                (reset! totalreward 0))
            (do
              (if (> reward (deref maxreward)) (reset! maxreward reward))
              (if (> (deref minreward) reward) (reset! minreward reward))
              (reset! totalreward (+ (deref totalreward) reward)))))))))

;;; Fin
