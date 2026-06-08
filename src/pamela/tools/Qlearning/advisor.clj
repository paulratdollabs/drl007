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
            [pamela.tools.Qlearning.Qtables :as qtbl]
            ;; [wkok.openai-clojure.api :as api]
            )
  (:gen-class))

;;; (in-ns 'pamela.tools.Qlearning.advisor)

(def all-advice [])                     ; Collacted advice

(defn register-advice
  [advice]
  (if (v3) (println "New advice " (:name advice) "registered"))
  (def all-advice (conj all-advice advice)))

(defn make-advice
  [name json precondition action-proposed]
  {:name name,
   :precondition precondition,
   :action-proposed action-proposed})

(defn applicable
  [advice env]
  (when ((:precondition advice) env)
    (if (v5) (println "Advice " (:name advice) "precondition satisfied"))
    advice))

(defn find-relevant-advice
  [env]
  (let [candidates (into [] (remove nil? (map (fn [advice] (applicable advice env)) all-advice)))]
    candidates))

(defn select-action-from-votes
  [action-proposed votes]
  (if (and (v4) (not (== (first votes) action-proposed)))
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
            (if (v4) (println (count actionable-advice) " pieces of relevant advice found, and these votes resulted " votes))
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
  [advice gpt-response]
  (println "advice=" advice "gpt-response=" gpt-response)
  (if (and advice gpt-response)
    (let [advisor1 (make-advice advice gpt-response, advisor1-precondition advisor1-action)]
      (when (v1) (println "Installing advice: " advice))
      (register-advice advisor1))
    (println "No advice installed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GPT interface

;;; Commented out because it doesn't build at the moment.

#_(defn test1
  []
  (api/create-chat-completion {:model "gpt-3.5-turbo"
                             :messages [{:role "system" :content "You are a helpful assistant."}
                                        {:role "user" :content "Who won the world series in 2020?"}
                                        {:role "assistant" :content "The Los Angeles Dodgers won the World Series in 2020."}
                                        {:role "user" :content "Where was it played?"}]}))

;;; (test1)
