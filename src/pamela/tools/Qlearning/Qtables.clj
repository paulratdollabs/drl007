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

(defn make-fixed-sized-q-table-uniform-random-aux
  "Q-table constructor that fills the q-table with random values."
  [num-state-variables discretization num-actions low high]
  (if (= num-state-variables 0)
    (vec (doall (repeatedly num-actions
                            (fn [] (atom
                                    (+ low (* (- high low)(rand))))))))
    (vec (doall (repeatedly discretization
                            (fn [] (make-fixed-sized-q-table-uniform-random-aux
                                    (- num-state-variables 1) discretization num-actions low high)))))))

(defn make-fixed-sized-q-table-uniform-random
  "Q-table constructor that fills the q-table with random values."
  [numobs discretization num-actions low high obslow disc-os-win-size]
  ;; (println "MakeQ: nomobs=" numobs "disc=" discretization "numacts=" num-actions "low="low "high=" high)
  {:storage (make-fixed-sized-q-table-uniform-random-aux numobs discretization num-actions low high)
   :obslow obslow
   :disc-os-win-size disc-os-win-size})

(defn table-size-aux
  [storage size]
  (if (not (coll? storage))
    size
    (table-size-aux (first storage) (conj size (count storage)))))

(defn table-size
  [storage]
  (table-size-aux storage []))

(defn q-table-size
  [qt]
  (table-size (:storage qt)))

(defn fixed-sized-q-value
  "Helper function for q-table indexing."
  [q-table & args]
  ;;(println "In fixed-sized-q-value, args=" args "qt dimensions=" (q-table-size q-table))
  ;;;(try
  (reduce nth q-table args))
  ;;;   (catch Exception e ( "caught exception: " (.getMessage e)))
  ;;;   (finally (println "In fixed-sized-q-value (DMQL.clj), q-table size=" (q-table-size q-table) "indices=" args)
  ;;;            (System/exit 0))))

(defn get-all-actions-quality
  [learner d-state]
  ;;(println "In get-all-actions-quality, d-state=" d-state "qt dimensions=" (q-table-size (deref (:q-table learner))))
  (apply fixed-sized-q-value (:storage (deref (:q-table learner))) d-state))

(defn get-action-quality
  "Return the atom representing the Q value for the given action in the given discretized state."
  [learner d-state action]
  (fixed-sized-q-value (apply fixed-sized-q-value (:storage (deref (:q-table learner))) d-state) action))

(defn deatomize-q-table
  [qtable-storage]
  ;;(println "In deatomize-q-table with: " qtable)
  (cond
    (map? qtable-storage)
    (into {} (map (fn [[a b]] {a (deatomize-q-table b)}) qtable-storage))

    (coll? qtable-storage)
        (vec (doall (map deatomize-q-table qtable-storage)))

        (= (type qtable-storage) (type (atom 42)))
        (deref qtable-storage)

        :otherwise qtable-storage))

(defn save-q-table
  "Save the supplied Q-Table using a filename that includes the episode number."
  [q-table episode name]
  (let [fn (str name episode "-q-table.edn")
        {storage :storage obslow :obslow disc-os-win-size :disc-os-win-size} (deref q-table)]
    (with-open [w (io/writer fn)]
      (binding [*out* w]
        (println ";;; Readable EDN Q-Table after episode " episode)
        (pr {:obslow obslow :disc-os-win-size disc-os-win-size :storage (deatomize-q-table storage)})))
    fn))

(defn reatomize-q-table
  [qtable-storage]
  ;;(println "In deatomize-q-table with: " qtable)
  (cond
    (map? qtable-storage)
    (let [{obslow :obslow
           disc-os-win-size :disc-os-win-size
           storage :storage} qtable-storage]
      {:obslow obslow
       :disc-os-win-size disc-os-win-size
       :storage (reatomize-q-table storage)})
    ;; (into {} (map (fn [[a b]] {a (reatomize-q-table b)}) qtable-storage))

    (coll? qtable-storage)
    (vec (doall (map reatomize-q-table qtable-storage)))

    (number? qtable-storage)
    (atom qtable-storage)

    :otherwise qtable-storage))

(defn read-q-table
  "Read a Q-Table previously written out in EDN format;"
  [filename]
  (with-open [r (java.io.PushbackReader. (io/reader filename))]
    (reatomize-q-table (edn/read r)))) ;(clojure.java.io.PushbackReader. r)


(defn max-atom
  "Given a list of Q-table entries representing, in order, the available actions, select the greatest quality."
  [list-of-atoms]
  ;;(println "max-atom called with " list-of-atoms)
  (apply max (map deref list-of-atoms)))



;;; Fin
