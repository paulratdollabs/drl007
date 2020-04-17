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

(defn get-field-value
  [field]
  (let [value (get *fields* field)]
    (and value (deref value))))

(defn updatefieldvalue
  [field value]
  (let [known-field (get *fields* field)]
    (if known-field
      (reset! known-field value)
      (def ^:dynamic *fields* (merge *fields* {field (atom value)}))))
  (if *debug-fields* (println "***Set field " field "=" (get *fields* field))))

(defn actionselector
  [list-of-atoms]
  (deref (first list-of-atoms))) ;+++

(defn reset-state                       ; Invoke the reset +++
  []
  nil)

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

(defn get-discrete-state                ; Discretize the state +++
  [raw-state]
  nil)

(defn goal-achieved                     ; Open to decide differently
  [state done]
  done)

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
                   (fn [self state]     ; :get-discrete-state
                     (get-discrete-state state))
                   (fn [self state]     ; :goal-achieved
                     (get-discrete-state state)))]
    interface))

;;; Fin
