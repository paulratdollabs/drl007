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
            [clojure.data.json :as json]
            [pamela.tools.Qlearning.fromjson :as fromjson]
            [clojure.math.numeric-tower :as math]
            [environ.core :refer [env]]
            [pamela.tools.Qlearning.DPLinterface :as dpli :refer [v1, v2, v3, v4, v5]]
            [pamela.tools.Qlearning.Qtables :as qtbl]
            [pamela.tools.Qlearning.analytics :as anal]
            [pamela.tools.Qlearning.advisor :as advisor])
(:gen-class))

;(in-ns 'pamela.tools.Qlearning.DMQL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning

(defn initialize-learner
  "Establishes the data structure that governs the operation of the learner."
  [cycletime max-steps mode render stats backup learningrate discount epsilon
   episodes explore ssdi ssav numobs numacts initialQ platform advice-given gpt-response]
  {
   :q-table (atom initialQ)             ; Q-table
   :cycletime cycletime                 ; cycle time in milliseconds
   :mode mode                           ; Choose action selection mode
   :render-every render
   :stats-every stats
   :backup-every backup
   :max-steps max-steps                 ; Maximum number of steps in an episode
   :alpha learningrate                  ; 0 lt learningrate lt 1
   :gamma discount                      ; 0 lt discount lt 1
   :epsilon epsilon                     ; Starting point for explore
   :episodes episodes                   ; number of episodes
   :explore explore
   :discretization ssdi
   :savestateandaction ssav
   :numobs numobs
   :numacts numacts
   :platform platform
   :ask-gpt advice-given
   :gpt-response gpt-response
   })


(def successes 0)
(def episode-of-first-success nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Action Selection

;;; Fix for ;indexOf not working with java data, see below.

(defn myIndexOf
     [aseq anitem]
  (loop [theSeq aseq
         theIndex 0]
    (if (= (first theSeq) == anitem)
      theIndex
      (if (not theSeq)
        -1
        (recur (rest theSeq) (+ 1 theIndex))))))

(defn textbook-action-selector-with-epsilon-randomization
  "Select an action either randomly according to epsilon or the best."
  [list-of-actions numacts eps]
  (if (> (rand) eps)
    (let [best (reduce max list-of-actions)
          posn (myIndexOf list-of-actions best)]
      ; (print "best = " best " posn = " posn "numacts =" numacts)
      posn)   ; Previously used .indexOf but that breaks with java data.
    (int (* (rand) numacts))))

(defn mc-select-nth
  [vals eps]
  (let [powr (* 4.0 (- 1 eps))          ;eps varies from 1 to 0, Powr varies from 0 to 4
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
  [list-of-actions numacts eps epsilon mode] ; epsilon is epsilon randomness, eps is Monte-Carlo confidence
  ;; (println "In action-selector with list-of-atoms=" list-of-atoms)
  (if (and (= mode 2) (< (rand) epsilon))
    (int (* (rand) numacts))
    (mc-select-nth list-of-actions eps)))

(def discrepencies 0)
(def comparisons 0)
(def previous-action nil)

(defn select-action
  "Select an action using algorithm selected by --mode."
  [learner dstate epsilon]
  ;;(println "learner=" learner "dstate=" dstate)
  (let [{q-table :q-table
         mode    :mode
         numacts :numacts} learner
        all-actions (qtbl/get-all-actions-quality learner dstate)]
    (case mode
      ;; Mode 0, the default is the textbook selection with epsilon randomized selection.
      0 (textbook-action-selector-with-epsilon-randomization all-actions numacts epsilon)

      ;; Mode 1, DOLL Monte-Carlo selection based on a Bayesian model of knowledge (Preferred!)
      1 (monte-carlo-action-selector all-actions numacts epsilon 0.0 mode)

      ;; Mode 2, Same as above overlaid with epsilon randomization (just for comparison purposes)
      2 (monte-carlo-action-selector all-actions numacts epsilon epsilon mode)

      ;; Mode 3, DOLL Monte-Carlo selection based on a Bayesian model of knowledge with history
      3 (monte-carlo-action-selector all-actions numacts epsilon 0.0 mode)

      ;; Mode 4, Same as above overlaid with epsilon randomization with history (just for comparison purposes)
      4 (monte-carlo-action-selector all-actions numacts epsilon epsilon mode)

      ;; Compare mode
      -1 (let [tb (textbook-action-selector-with-epsilon-randomization all-actions numacts epsilon)
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

;;; These are the history preserving modes
(defn history-preserving
  [mode]
  (or (= mode 3) (= mode 4)))

(defn engrave
  [learner ds action history]
  (if (history-preserving (:mode learner))
    (cons [ds action] history)
    history))

(defn back-propagation-of-reward
  [initial-reward learner history]
  (let [{ gamma :gamma } learner]
    (loop [reward initial-reward
           hist history]
      (let [payback (* gamma reward)
            [ds action] (first hist)
            q-pos (qtbl/get-action-quality learner ds action)
            currentq-val (deref q-pos)
            rhist (rest hist)]
        (reset! q-pos (+ currentq-val payback))
        (if (not (empty? rhist)) (recur payback rhist))))))

(defn run-episode
  "Train a single episode."
  [learner episode epsilon]
  (let [{;; Pull out all of the pieces aheadof the loop
         mode      :mode
         render-every :render-every
         q-table   :q-table
         cycletime :cycletime
         max-steps :max-steps
         alpha     :alpha
         gamma     :gamma
         ssdi      :discretization
         ssav      :savestateandaction
         numobs    :numobs
         numacts   :numacts
         statedisc :state-discretizer
         platform  :platform
         advice    :ask-gpt} learner
         current-state ((:get-current-state platform) platform numobs)
         discstate ((:get-discrete-state platform) learner current-state)
         ;;gpt-response ((:get-gpt-response platform) learner)
        ]
    (loop [current-d-state discstate
           donep false
           ereward 0.0
           step 0.0
           succeeded false
           history ()]
      ;; (println "state = " discstate)
      (if (and (not donep) (not (>= step max-steps)))
        (let [action-proposed (select-action learner current-d-state epsilon) ; Select a action
              action (advisor/consider-advice-about-action action-proposed previous-action current-d-state)]
          (if (v4) (println "About to run action " action))
          ((:perform platform) platform action cycletime)
          (def previous-action action)
          (if (v4) (println "Action completed"))
          (when (and ssav (== (mod episode ssav) 0)) (anal/write-csv-data current-d-state action))
          #_(println "platform=" platform "(:plantid platform)=" (:plantid platform))
          (let [new-state ((:get-current-state platform) platform numobs)
                reward ((:get-field-value platform) platform (:plantid platform) :reward)
                episode-done ((:get-field-value platform) platform (:plantid platform) :done)
                new-d-state ((:get-discrete-state platform) learner new-state)]
            (if (and (not (= render-every 0))
                     (= 0 (mod episode render-every)))
              ((:render platform) platform))
            (if (v4) (println "step=" step "Action=" action "state=" new-state "reward=" reward "done?=" episode-done
                              "disc.State=" new-d-state "advice=" advice))
            (cond
              (and (not episode-done) (not (>= step max-steps)))
              (let [max-future-q (apply max (qtbl/get-all-actions-quality learner new-d-state))
                    current-q (qtbl/get-action-quality learner current-d-state action)
                    ;;current-q (deref q-pos)
                    ;;_ (println "alpha=" alpha "currentQ=" current-q "reward=" reward
                    ;;           "gamma=" gamma "max-future-q=" max-future-q)
                    ;; Bellman's Equation
                    new-q (+ (* (- 1.0 alpha) current-q) (* alpha (+ reward (* gamma max-future-q))))]
                (qtbl/set-action-quality! learner current-d-state action new-q))

              ((:goal-achieved platform) platform new-state reward episode-done)
              (let [q-val (qtbl/get-action-quality learner new-d-state action)
                    reward-for-success 0] ;+++ should be maxQ
                (qtbl/set-action-quality! learner new-d-state action reward-for-success)
                (def successes (+ 1 successes))
                (cond (not episode-of-first-success)
                      (do (def episode-of-first-success episode)
                          (if (v1) (println "*** FIRST SUCCESS ACHIEVED ON EPISODE ", episode)))
                      :otherwise
                      (if (v1) (println "*** Success #" successes "on step" step "in" episode "episodes ***")))
                (if (and (or (= mode 3) (= mode 4)) (not (empty? history)))
                  (back-propagation-of-reward reward-for-success learner history))))
            (recur  new-d-state episode-done (+ ereward reward) (+ step 1)
                    (and episode-done ((:goal-achieved platform) platform new-state reward episode-done))
                    (engrave learner current-d-state action history))))
        [ereward step succeeded]))))

(defn update-statistic-if
  [new test anatom]
  (let [val (deref anatom)]
    (if (or (= val :unset) (test new val))
      (reset! anatom new))))

(defn train
  "Train with a given number of episodes, saving statistics and q-tables at regular intervals."
  [learner]
  (let [runid (.getTime (new java.util.Date))
        {episodes :episodes
         epsilon  :epsilon
         explore  :explore
         platform :platform
         stats-every :stats-every
         save-every :backup-every
         q-table  :q-table
         ssav :savestateandaction
         advice :ask-gpt
         gpt-response :gpt-response} learner
        start-eps-decay 1
        end-eps-decay (int (* episodes explore))
        decay-by (/ epsilon (- end-eps-decay start-eps-decay))
        maxreward (atom :unset)
        minreward (atom :unset)
        totalreward (atom :unset)
        numsuccesses (atom 0)
        learning-history (atom [])
        processed-episodes (:episodes (deref q-table))
        initial-episode (if (or (not processed-episodes) (= processed-episodes 0))
                          0                          ; Starting a new training
                          processed-episodes)]       ; Continuing from a prior session
    (advisor/setup-advisors advice gpt-response)
    ;;(pprint learner)
    (if (v1) (println "processed-episodes=" processed-episodes "initial-episode=" initial-episode))
    (doseq [episode (range initial-episode episodes)]
      ;; Setup the simulator
      (when (and ssav (== (mod episode ssav) 0)) (anal/open-csv-file runid "DMQL" episode))
      (let [eps (if (> episode end-eps-decay) 0 (- epsilon (* episode decay-by)))]
        #_(if (= 0 (mod episode 10))
          (println "*** Starting Episode " episode "Epsilon=" eps "Q-table size=" (q-table-size (deref q-table))"\r"))
        ((:reset platform) platform)
        ;; Unneeded (Thread/sleep 10) ;Reset the platform and give it time to settle down
        (let [[reward steps succeeded] (run-episode learner episode eps)]
          (if (= 0 (mod episode stats-every))
            (do
              (if (> episode initial-episode)
                (if (v1) (println (str (java.time.LocalDateTime/now))
                         "Episode=" episode
                         "Epsilon=" eps
                         "Steps=" steps
                         "Total successes=" successes
                         "Successes this batch=" (deref numsuccesses)
                         "Max reward=" (deref maxreward)
                         "Average reward=" (/ (deref totalreward) stats-every)
                         "Min reward=" (deref minreward))))
              (reset! learning-history (conj (deref learning-history)
                                             {:episode episode
                                              :first-success episode-of-first-success
                                              :stats-every stats-every
                                              :numsuccesses (deref numsuccesses)
                                              :total-successes successes
                                              :max-reward (deref maxreward)
                                              :min-reward (deref minreward)
                                              :average-reward (if (= (deref totalreward) :unset) :unset (/ (deref totalreward) stats-every))}))
              (if (> episode initial-episode)
                (if (v1) (println "Saved statistics as: " ; maybe write as CSV file?
                           (anal/save-statistics (deref learning-history) episode runid "DMQL"))))
              ;; The first result of each set sets the starting values.
              (reset! numsuccesses (if succeeded 1 0))
              (reset! maxreward reward)
              (reset! minreward reward)
              (reset! totalreward reward))
            (do
              ;;(println "reward=" reward "maxreward=" maxreward "minreward=" minreward)
              (if succeeded (reset! numsuccesses (+ (deref numsuccesses) 1)))
              (update-statistic-if reward > maxreward)
              (update-statistic-if reward < minreward)
              (reset! totalreward (+ (deref totalreward) reward))))
          (if (and (> episode 0) (= 0 (mod episode save-every)))
            (if (v1) (println "Saved Q Table as: " (qtbl/save-q-table q-table episode "DMQL"))))
          (when (and ssav (== (mod episode ssav) 0)) (anal/close-csv-file runid "DMQL" episode)))))))

;;; Fin
