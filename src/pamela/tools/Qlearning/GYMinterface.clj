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
            [pamela.tools.Qlearning.DPLinterface :as DPL])
  (:import [pamela.tools.Qlearning.DPLinterface dplinterface])
  (:gen-class))


;(in-ns 'pamela.tools.Qlearning.GYMinterface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plant values


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
    1 [(DPL/get-field-value :gml :high0)]
    2 [(DPL/get-field-value :gml :high0) (DPL/get-field-value :gml :high1)]
    3 [(DPL/get-field-value :gml :high0) (DPL/get-field-value :gml :high1) (DPL/get-field-value :gml :high2)]
    4 [(DPL/get-field-value :gml :high0) (DPL/get-field-value :gml :high1) (DPL/get-field-value :gml :high2) (DPL/get-field-value :gml :high3)]
    (do (println (format "Wrong number of observations (%d), must be between 1 and 4." numobs))
        (System/exit 0))))

(defn get-obs-low
  [numobs]
  (case numobs
    1 [(DPL/get-field-value :gml :low0)]
    2 [(DPL/get-field-value :gml :low0) (DPL/get-field-value :gml :low1)]
    3 [(DPL/get-field-value :gml :low0) (DPL/get-field-value :gml :low1) (DPL/get-field-value :gml :low2)]
    4 [(DPL/get-field-value :gml :low0) (DPL/get-field-value :gml :low1) (DPL/get-field-value :gml :low2) (DPL/get-field-value :gml :low3)]
    (do (println (format "Wrong number of observations (%d), must be between 1 and 4." numobs))
        (System/exit 0))))

(defn get-current-state
  [numobs]
  (if (not (number? numobs))
    (do (println "numobs (" numobs ")is not a number.  This usually means that you need to restart the plant.")
        (System/exit 0)))
  (case numobs
    1 [(DPL/get-field-value :gml :state0)]
    2 [(DPL/get-field-value :gml :state0) (DPL/get-field-value :gml :state1)]
    3 [(DPL/get-field-value :gml :state0) (DPL/get-field-value :gml :state1) (DPL/get-field-value :gml :state2)]
    4 [(DPL/get-field-value :gml :state0) (DPL/get-field-value :gml :state1) (DPL/get-field-value :gml :state2) (DPL/get-field-value :gml :state3)]
    (do (println (format "Wrong number of observations (%d), must be between 1 and 4." numobs))
        (System/exit 0))))

(defn goal-achieved                     ; Open to decide differently
  [state done]
  (> (first state) (DPL/get-field-value :gml :goal_position)))

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
                     (DPL/get-field-value :gml field))
                   (fn [self field val] ; :set-field-value
                     (DPL/updatefieldvalue :gml field val))
                   (fn [self numobs]    ; :get-current-state
                     (get-current-state numobs)))]
    interface))

;;; Fin
