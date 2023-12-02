;; Copyright © 2020 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning Statistics and Analytics

(ns pamela.tools.Qlearning.advisor
  "Advisor"
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
            [pamela.tools.Qlearning.DPLinterface :as dpli :refer [v1, v2, v3, v4, v5]]
            [clojure.math.numeric-tower :as math]
            [environ.core :refer [env]]
            [pamela.tools.Qlearning.Qtables :as qtbl])
  (:gen-class))

;;; (in-ns 'pamela.tools.Qlearning.advisor)

(def all-advice [])                     ; Collacted advice

(defn register-advice
  [advice]
  (if (V3) (println "New advice " (:name advice) "registered"))
  (def all-advice (conj all-advice advice)))

(defn make-advice
  [name precondition action-proposed]
  {:name name,
   :precondition precondition,
   :action-proposed action-proposed})

(defn applicable
  [advice env]
  (when ((:precondition advice) env)
    (if (V5) (println "Advice " (:name advice) "precondition satisfied"))
    advice))

(defn find-relevant-advice
  [env]
  (let [candidates (into [] (remove nil? (map (fn [advice] (applicable advice env)) all-advice)))]
    candidates))

(defn select-action-from-votes
  [action-proposed votes]
  (if (and (V4) (not (== (first votes) action-proposed)))
    (println "Following advice for action " (first votes) " proposed was " action-proposed))
  (first votes)) ;+++ think about this +++

(defn consider-advice-about-action
  [action-proposed previous-action current-d-state]
  (if (nil? previous-action)
    action-proposed
    (let [env {:proposed action-proposed :previous-action previous-action :d-state current-d-state}
          actionable-advice (find-relevant-advice env)]
      (if (empty? actionable-advice)
        action-proposed
        (let [votes (into [] (remove nil? (map (fn [advisor] ((:action-proposed advisor) env)) actionable-advice)))]
            (if (V4) (println (count actionable-advice) " pieces of relevant advice found, and these votes resulted " votes))
            (if (empty? votes)
              action-proposed
              (select-action-from-votes action-proposed votes)))))))

;;; Hardwired advisor for now

(defn advisor1-precondition
  [env]
  (let [d-state (:d-state env)
        ds1 (second d-state)]
    (or (<= ds1 6) (>= ds1 12))))       ;+++ constants

(defn advisor1-action
  [env]
  (:previous-action env))

(defn setup-advisors
  []
  (let [advisor1 (make-advice "limit-changes-at-speed", advisor1-precondition advisor1-action)]
    (register-advice advisor1)))
