;; Copyright © 2020 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning

(ns pamela.tools.q-learning.DMQL
  "Q-Learning"
  (:require [clojure.string :as string]
            [clojure.repl :refer [pst]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            )
  (:gen-class))

;(in-ns 'pamela.tools.q-learning.DMQL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning

(defn actionselector
  [list-of-atoms]
  (deref (first list-of-atoms))) ;+++

(defn reset-state                       ; Invoke the reset +++
  []
  nil)

(defn perform                           ; Invoke the action +++
  [action]
  nil)

(defn get-discrete-state                ; Discretize the state +++
  [raw-state]
  nil)

(defn goal-achieved                     ; Open to decide differently
  [state done]
  done)

(defn initialize-learner
  [cycletime learningrate discount epsilon actionselecter initialQ]
  {
   :q-table (atom initialQ)             ; Q-table
   :cycletime cycletime                 ; cycle time in milliseconds
   :alpha learningrate                  ; 0 lt learningrate lt 1
   :gamma discount                      ; 0 lt discount lt 1
   :epsilon 1                           ;
   :actionselectfunction actionselector ; function that selects an action.
   :reset-state reset-state             ; function returns starting discrete-state
   })

;;;
(defn make-fixed-sized-q-table-uniform-random
  [num-state-variables discretization num-actions low high]
  (if (= num-state-variables 0)
    (repeatedly num-actions (fn [] (atom
                                    (+ low (* (- high low)(rand))))))
    (repeatedly discretization
                (fn [] (make-fixed-sized-q-table-uniform-random
                        (- num-state-variables 1) discretization num-actions low high)))))

(defn fixed-sized-q-value
  [q-table & args]
  (reduce nth q-table args))

;;; (def test-q-table (make-fixed-sized-q-table-uniform-random 2 3 2 -2 0))

;;; (pprint (make-fixed-sized-q-table-uniform-random 2 3 2 -2 0))

(defn max-atom
  [list-of-atoms]
  (max (map deref list-of-atoms)))

(defn run-episode
  [learner startstate]
  (let [{;; Pull out all of the pieces aheadof the loop
         q-table :q-table
         alpha   :alpha
         gamma   :gamma
         select  :actionselectionfunction} learner]
    (loop [state startstate
           donep false
           ereward 0]
      (if (not donep)
        (let [;; Select a action
              action (select (apply fixed-sized-q-value state))
              ;; Perform action
              [new-state reward d] (perform action)
              discrete-state (get-discrete-state new-state)]
          (cond
            (not d)
            (let [max-future-q (max-atom (fixed-sized-q-value q-table discrete-state))
                  q-pos (fixed-sized-q-value (fixed-sized-q-value q-table state) action)
                  current-q (deref q-pos)
                  new-q (+ (* (- 1.0 alpha) current-q) ; Bellman's Equation
                           (* alpha (+ reward (* gamma max-future-q))))]
              ;;(set! q-pos new-q)
              )

            (goal-achieved new-state d)
            (let [q-pos (fixed-sized-q-value (fixed-sized-q-value q-table discrete-state) action)]
              ;;(set! q-pos (+ ereward reward))
              ))

          (recur  discrete-state d (+ ereward reward)))
        ))))

(defn train
  [learner episodes]
  (let [{q-table :q-table
         reset-state :reset-state} learner]
    (dotimes [episode episodes]
      (run-episode learner (get-discrete-state (reset-state))))))

;;; Fin
