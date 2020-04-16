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
            [pamela.tools.Qlearning.DPLinterface :as dpl]
            )
  ;; ??? Why don't these two ways of accessing the record work ???
  ;; (:use [pamela.tools.Qlearning.DPL-interface])
  ;; (:import [pamela.tools.Qlearning.DPL-interface dlpinterface])
  (:gen-class))

;(in-ns 'pamela.tools.Qlearning.GYMinterface)

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
  (let [interface (dpl/make-dpl-interface
                   ;; :world-parameters
                   (list world-name)

                   ;; :routing
                   routing

                   ;; :channel
                   channel

                   ;; :exchange
                   exchange

                   ;; :initialize-world
                   (fn [self] (initialize-simulator (first (:world-parameters self)) (:routing self) (:channel self) (:exchange self)))

                   ;; :shutdown
                   (fn [self] (shutdown (:routing self) (:channel self) (:exchange self)))

                   ;; :perform
                   (fn [self action] (perform action (:routing self) (:channel self) (:exchange self)))

                   ;; :reset
                   (fn [self] (reset (:routing self) (:channel self) (:exchange self)))

                   ;; :render
                   (fn [self] (render (:routing self) (:channel self) (:exchange self)))

                   ;; :get-discrete-state
                   (fn [self state] (get-discrete-state state))

                   ;; :goal-achieved
                   (fn [self state] (get-discrete-state state)))]
    ((:initialize-world interface) interface)
    interface))

;;; Fin
