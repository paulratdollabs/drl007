;; Copyright © 2020 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning

(ns pamela.tools.Qlearning.GYMinterface
  "GYM Interface"
  (:require [clojure.string :as string]
            [clojure.repl :refer [pst]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [mbroker.rabbitmq :as mq]
            [langohr.core :as rmq]
            [langohr.exchange :as le]
            [langohr.queue :as lq]
            [langohr.consumers :as lc]
            [langohr.channel :as lch]
            [tpn.fromjson :as fromjson]
             pamela.tools.Qlearning.DPLinterface)
  (:import [pamela.tools.Qlearning.DPLinterface dplinterface])
  (:gen-class))

;(in-ns 'pamela.tools.Qlearning.GYMinterface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plant values

(def ^:dynamic *fields* {})
(def ^:dynamic *debug-fields* false)

(defn print-field-values
  []
  (pprint *fields*))

(defn get-field-value
  [field]
  (let [value (get *fields* (keyword field))]
    (if value
      (deref value)
      (do (println "field " field "not found in " *fields*)
          :not-found))))

(def fv get-field-value)

(defn updatefieldvalue
  [field value]
  (let [kfield (keyword field)
        known-field (get *fields* kfield)]
    (if known-field
      (reset! known-field value)
      (def ^:dynamic *fields* (merge *fields* {kfield (atom value)}))))
  (if *debug-fields* (println "***Set field " field "=" (get *fields* (keyword field)))))

(defn initialize-simulator
  [gym-world routing channel exchange]
  (mq/publish-object
   {:id "qlearner" :plant-id "gym" :exchange "dmrl" :function-name "make_env" :args [gym-world]}
   routing                              ; routing-key
   channel                              ; channel
   exchange))                           ; exchange

(defn perform                           ; Invoke the action
  [action routing channel exchange]
  (mq/publish-object
   {:id "qlearner" :plant-id "gym" :exchange "dmrl" :function-name "perform-action" :args [action]}
   routing                              ; routing-key
   channel                              ; channel
   exchange))

(defn reset                             ; reset the simulator for the next episode
  [routing channel exchange]
  (mq/publish-object
   {:id "qlearner" :plant-id "gym" :exchange "dmrl" :function-name "reset" :args []}
   routing                              ; routing-key
   channel                              ; channel
   exchange))

(defn render                            ; reset the simulator for the next episode
  [routing channel exchange]
  (mq/publish-object
   {:id "qlearner" :plant-id "gym" :exchange "dmrl" :function-name "render" :args []}
   routing                              ; routing-key
   channel                              ; channel
   exchange))

(defn shutdown                          ; shutdown the simulator.
  [routing channel exchange]
  ;; NYI
  #_(mq/publish-object
   {:id "qlearner" :plant-id "gym" :exchange "dmrl" :function-name "shutdown" :args []}
   routing                              ; routing-key
   channel                              ; channel
   exchange)
  nil)

(defn get-obs-high
  [numobs]
  (case numobs
    1 [(get-field-value :high0)]
    2 [(get-field-value :high0) (get-field-value :high1)]
    3 [(get-field-value :high0) (get-field-value :high1) (get-field-value :high2)]
    4 [(get-field-value :high0) (get-field-value :high1) (get-field-value :high2) (get-field-value :high3)]
    (do (println (format "Wrong number of observations (%d), must be between 1 and 4." numobs))
        (System/exit 0))))

(defn get-obs-low
  [numobs]
  (case numobs
    1 [(get-field-value :low0)]
    2 [(get-field-value :low0) (get-field-value :low1)]
    3 [(get-field-value :low0) (get-field-value :low1) (get-field-value :low2)]
    4 [(get-field-value :low0) (get-field-value :low1) (get-field-value :low2) (get-field-value :low3)]
    (do (println (format "Wrong number of observations (%d), must be between 1 and 4." numobs))
        (System/exit 0))))

(defn get-current-state
  [numobs]
  (if (not (number? numobs))
    (do (println "numobs (" numobs ")is not a number.  This usually means that you need to restart the plant.")
        (System/exit 0)))
  (case numobs
    1 [(get-field-value :state0)]
    2 [(get-field-value :state0) (get-field-value :state1)]
    3 [(get-field-value :state0) (get-field-value :state1) (get-field-value :state2)]
    4 [(get-field-value :state0) (get-field-value :state1) (get-field-value :state2) (get-field-value :state3)]
    (do (println (format "Wrong number of observations (%d), must be between 1 and 4." numobs))
        (System/exit 0))))

(defn goal-achieved                     ; Open to decide differently
  [state done]
  (> (first state) (get-field-value :goal_position)))

(defn make-gym-interface
  [world-name routing channel exchange]
  (let [interface (dplinterface.        ; dpl/make-dpl-interface
                   world-name           ; :world-parameters
                   routing              ; :routing
                   channel              ; :channel
                   exchange             ; :exchange
                   (fn [self]           ; :initialize-world
                     (initialize-simulator
                      (first (:world-parameters self)) (:routing self) (:channel self) (:exchange self)))
                   (fn [self]           ; :shutdown
                     (shutdown (:routing self) (:channel self) (:exchange self)))
                   (fn [self action]    ; :perform
                     (perform action (:routing self) (:channel self) (:exchange self)))
                   (fn [self]           ; :reset
                     (reset (:routing self) (:channel self) (:exchange self)))
                   (fn [self]           ; :render
                     (render (:routing self) (:channel self) (:exchange self)))
                   (fn [self state done]     ; :goal-achieved
                     (goal-achieved state done))
                   (fn [self field]     ; :get-field-value
                     (get-field-value field))
                   (fn [self field val] ; :set-field-value
                     (updatefieldvalue field val))
                   (fn [self numobs]    ; :get-current-state
                     (get-current-state numobs)))]
    interface))

;;; Fin
