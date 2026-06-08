;; Copyright © 2020 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning

(ns pamela.tools.Qlearning.Qtables
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
            [environ.core :refer [env]])
  (:gen-class))

;(in-ns 'pamela.tools.Qlearning.Qtables)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Q-Table representation

;; {
;;  :q-table-type                          ; Implementation type of the Q-Table
;;  :storage                               ; Fixed storage structure by the implementation type
;;  :episodes                              ; If the q-table was saved, this lets us resule from where we left off.
;;  ;; Fields used by various representation
;;  :obslow                                ; The lowest value possible for an observation
;;  :disc-os-win-size                      ; Window size for a simple discretization.
;;  }

;; This file contains functions for making and manipulating Q-tables in a variety of formats
;; in support of a variety of RL algorithms.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fixed-Sized-Q-Table
;;; Clojure vectors with every element an atom into which q-values may be placed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constructors for Clojure arrays with atoms

;;; Initialize the table with linear random values between [low high)
(defn make-fixed-sized-q-table-uniform-random-aux
  "Q-table constructor that fills the q-table with random values."
  [num-state-variables discretization num-actions low high]
  (if (= num-state-variables 0)
    (vec (doall (repeatedly num-actions
                            (fn [] (atom (+ low (* (- high low)(rand))))))))
    (vec (doall (repeatedly discretization
                            (fn [] (make-fixed-sized-q-table-uniform-random-aux
                                    (- num-state-variables 1) discretization num-actions low high)))))))

;;; Initialize all values to -1 - no randomness
(defn make-fixed-sized-q-table-aux
  "Q-table constructor that fills the q-table with random values."
  [num-state-variables discretization num-actions]
  (if (= num-state-variables 0)
    (vec (doall (repeatedly num-actions (fn [] (atom -1)))))
    (vec (doall (repeatedly discretization
                            (fn [] (make-fixed-sized-q-table-aux
                                    (- num-state-variables 1) discretization num-actions)))))))

;;; Constructor for clojure-fixed-sized with linear random q-values
(defn make-fixed-sized-q-table-uniform-random
  "Q-table constructor that fills the q-table with random values."
  [numobs discretization num-actions low high obslow disc-os-win-size episodes]
  ;; (println "MakeQ: nomobs=" numobs "disc=" discretization "numacts=" num-actions "low="low "high=" high)
  {:q-table-type :clojure-fixed-sized
   :storage (make-fixed-sized-q-table-uniform-random-aux numobs discretization num-actions low high)
   :obslow obslow
   :disc-os-win-size disc-os-win-size
   :episodes episodes})

;;; (def ex (make-fixed-sized-q-table-uniform-random 3 4 5 0.0 1.0 42 7 99))

;;; Constructor for clojure-fixed-sized with constant initial q-values
(defn make-fixed-sized-q-table-constant
  "Q-table constructor that fills the q-table with random values."
  [numobs discretization num-actions obslow disc-os-win-size episodes]
  ;; (println "MakeQ: nomobs=" numobs "disc=" discretization "numacts=" num-actions)
  {:q-table-type :clojure-fixed-sized
   :storage (make-fixed-sized-q-table-aux numobs discretization num-actions)
   :obslow obslow
   :disc-os-win-size disc-os-win-size
   :episodes episodes})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constructors for Java array based Q-tables

(defn set-float-array-values-aux!
  [aobj indices func index]
  ;;(println "set-float-array-values-aux! indices=" indices "index=" index)
  (if (empty? indices)
    (apply aset-float aobj (concat index [(func index)]))
    (dotimes [i (first indices)]
      (set-float-array-values-aux! aobj (rest indices) func (concat index [i])))))

(defn set-float-array-values!
  [aobj indices func]
  (set-float-array-values-aux! aobj indices func []))

;;; (def a (apply make-array Float/TYPE (concat (repeatedly numobs (fn [] discretization)) [num-actions])))
;;; (def n 0.0)
;;; (set-array-values! a [5 5 5 8] (fn [] (def n (+ n 1.0)) n))

;;; Constructor for java-fixed-sized with linear random q-values
(defn make-java-fixed-sized-q-table-uniform-random
  "Q-table constructor that fills the q-table with random values."
  [numobs discretization num-actions low high obslow disc-os-win-size episodes]
  ;; (println "MakeQ: numobs=" numobs "disc=" discretization "numacts=" num-actions "low="low "high=" high)
  (let [indices (concat (repeatedly numobs (fn [] discretization)) [num-actions])
        jarray (apply make-array Float/TYPE indices)]
    (set-float-array-values! jarray indices (fn [index] (+ low (* (- high low)(rand)))))
    {:q-table-type :java-fixed-sized
     :storage jarray
     :obslow obslow
     :disc-os-win-size disc-os-win-size
     :episodes episodes
     :indices indices}))

;;; (def ex (make-java-fixed-sized-q-table-uniform-random 3 4 5 0.0 1.0 42 7 99))

;;; Constructor for java-fixed-sized with constant initial q-values
(defn make-java-fixed-sized-q-table-constant
  "Q-table constructor that fills the q-table with random values."
  [numobs discretization num-actions val obslow disc-os-win-size episodes]
  ;; (println "MakeQ: nomobs=" numobs "disc=" discretization "numacts=" num-actions)
  (let [indices (concat (repeatedly numobs (fn [] discretization)) [num-actions])
        jarray (apply make-array Float/TYPE indices)]
    (set-float-array-values! jarray indices (fn [index] val))
    {:q-table-type :java-fixed-sized
     :storage jarray
     :obslow obslow
     :disc-os-win-size disc-os-win-size
     :episodes episodes
     :indices indices}))

;;; (def ex (make-java-fixed-sized-q-table-constant 3 4 5 -1.0  42 7 99))

;;; Constructor for java-fixed-sized with values q-values
(defn make-java-fixed-sized-q-table-loaded-values
  "Q-table constructor that fills the q-table with random values."
  [numobs discretization num-actions vals obslow disc-os-win-size episodes]
  ;; (println "MakeQ: nomobs=" numobs "disc=" discretization "numacts=" num-actions)
  (let [indices (concat (repeatedly numobs (fn [] discretization)) [num-actions])
        jarray (apply make-array Float/TYPE indices)]
    (set-float-array-values! jarray indices (fn [index] (reduce nth vals index)))
    {:q-table-type :java-fixed-sized
     :storage jarray
     :obslow obslow
     :disc-os-win-size disc-os-win-size
     :episodes episodes
     :indices indices}))

;;; (def ex (make-java-fixed-sized-q-loaded-values 3 4 5 lvs  42 7 99))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calculate table dimensions

(defn table-size-aux
  [storage size]
  (if (not (coll? storage))
    size
    (table-size-aux (first storage) (conj size (count storage)))))

(defn cft-table-size
  [storage]
  (table-size-aux storage []))

;;;
(defn fixed-sized-q-value
  "Helper function for q-table indexing."
  [q-table & args]
  (reduce nth q-table args))

(defn fixed-sized-q-set!
  [q-table d-state action new-q]
  (reset! (fixed-sized-q-value (apply fixed-sized-q-value q-table d-state) action) new-q))

;;; Extracts the vector of atoms
(defn cft-get-all-actions-quality
  [learner d-state]
  ;;(println "In get-all-actions-quality, d-state=" d-state "qt dimensions=" (q-table-size (deref (:q-table learner))))
  (apply fixed-sized-q-value (:storage (deref (:q-table learner))) d-state))

(defn cft-get-action-quality
  "Return the atom representing the Q value for the given action in the given discretized state."
  [learner d-state action]
  (fixed-sized-q-value (apply fixed-sized-q-value (:storage (deref (:q-table learner))) d-state) action))

(defn deatomize-q-table-storage
  [qtable-storage]
  ;;(println "In deatomize-q-table with: " qtable)
  (cond
    (map? qtable-storage)
    (into {} (map (fn [[a b]] {a (deatomize-q-table-storage b)}) qtable-storage))

    (coll? qtable-storage)
    (vec (doall (map deatomize-q-table-storage qtable-storage)))

    (= (type qtable-storage) (type (atom 42)))
    (deref qtable-storage)

    :otherwise qtable-storage))

(defn reatomize-q-table
  [qtable]
  ;;(println "In deatomize-q-table with: " qtable)
  (cond
    (map? qtable)
    (let [{q-table-type :q-table-type
           storage :storage
           obslow :obslow
           disc-os-win-size :disc-os-win-size
           episode :episode} qtable]
      {:q-table-type q-table-type
       :storage (reatomize-q-table storage)
       :obslow obslow
       :disc-os-win-size disc-os-win-size
       :episode episode})
    ;; (into {} (map (fn [[a b]] {a (reatomize-q-table b)}) qtable-storage))

    (coll? qtable)
    (vec (doall (map reatomize-q-table qtable)))

    (number? qtable)
    (atom qtable)

    :otherwise qtable))

(defn max-atom
  "Given a list of Q-table entries representing, in order, the available actions, select the greatest quality."
  [list-of-atoms]
  ;;(println "max-atom called with " list-of-atoms)
  (apply max (map deref list-of-atoms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(defn q-table-size
  [self]
  (case (:q-table-type self)
    :clojure-fixed-sized (cft-table-size (:storage self))

    :java-fixed-sized (:indices self)

    (throw (Throwable. "Unknown Q-table type"))))

(defn get-all-actions-quality
  [learner d-state]
  (let [q-table (deref (:q-table learner))]
    (case (:q-table-type q-table)
      :clojure-fixed-sized
      (map deref (apply fixed-sized-q-value (:storage q-table) d-state))

      :java-fixed-sized
      (apply aget (:storage q-table) d-state)

      (throw (Throwable. "Unknown Q-table type")))))

(defn get-action-quality
  "Return the atom representing the Q value for the given action in the given discretized state."
  [learner d-state action]
  (let [q-table (deref (:q-table learner))]
    (case (:q-table-type q-table)
      :clojure-fixed-sized
      (deref (fixed-sized-q-value (apply fixed-sized-q-value (:storage q-table) d-state) action))

      :java-fixed-sized
      (aget (apply aget (:storage q-table) d-state) action)

      (throw (Throwable. "Unknown Q-table type")))))

(defn set-action-quality!
  [learner d-state action new-q]
  (let [q-table (deref (:q-table learner))]
    (case (:q-table-type q-table)
      :clojure-fixed-sized
      (fixed-sized-q-set! (:storage q-table) d-state action new-q)

      :java-fixed-sized
      (apply aset-float (:storage q-table) (concat d-state [action new-q]))

      (throw (Throwable. "Unknown Q-table type")))))

(defn save-q-table
  "Save the supplied Q-Table using a filename that includes the episode number."
  [q-table episode name]
  (let [fn (str name episode "-q-table.edn")
        {q-table-type :q-table-type
         obslow :obslow
         disc-os-win-size :disc-os-win-size
         indices :indices} (deref q-table)]
    (case q-table-type
      :clojure-fixed-sized
      (with-open [w (io/writer fn)]
        (binding [*out* w]
          (println ";;; Readable EDN Q-Table after episode " episode)
          (pr {:q-table-type q-table-type
               :storage (:storage (deatomize-q-table-storage q-table))
               :obslow obslow
               :disc-os-win-size disc-os-win-size
               :episode episode
               :indices indices})))

      :java-fixed-sized
      (with-open [w (io/writer fn)]
        (binding [*out* w]
          (println ";;; Readable EDN Q-Table after episode " episode)
          (pprint {:q-table-type q-table-type
                   :storage (:storage (deref q-table))
                   :obslow obslow
                   :disc-os-win-size disc-os-win-size
                   :episode episode
                   :indices indices})))

      (throw (Throwable. "Unknown Q-table type")))
    fn))

(defn read-q-table
  "Read a Q-Table previously written out in EDN format;"
  [filename]
  (with-open [r (java.io.PushbackReader. (io/reader filename))]
    (let [qtable (edn/read r)
          {q-table-type :q-table-type
           obslow :obslow
           storage :storage
           episode :episode
           disc-os-win-size :disc-os-win-size
           indices :indices} qtable]
      (case q-table-type
        :clojure-fixed-sized
        (reatomize-q-table qtable)

        :java-fixed-sized
        (let [indices (cft-table-size storage)]
          (make-java-fixed-sized-q-table-loaded-values
           (- (count indices) 1) (nth indices 0) (last indices) storage obslow disc-os-win-size episode))

        (throw (Throwable. "Unknown Q-table type"))))))


;;; Fin
