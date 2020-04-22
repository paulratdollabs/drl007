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


;;; Initialize the plant (robot or simulator)
(defn initialize-simulator
  [self gym-world]
  (DPL/bp-call self "gym" "make_env" [gym-world]))

;;; Invoke the action
(defn perform
  [self action]
  (DPL/bp-call self "gym" "perform-action" [action]))

;; reset the simulator for the next episode
(defn reset
  [self]
  (DPL/bp-call self "gym" "reset" []))

;; reset the simulator for the next episode
(defn render
  [self]
  (DPL/bp-call self "gym" "render" []))

;;; shutdown the simulator - NYI
(defn shutdown
  [self]
  ;;(DPL/bp-call self "gym" "shutdown" [])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State space discretization

(defn win-size
  "Compute the vector of window sizes for each state variable according to the discretization factor."
  [numobs ssdi]
  (vec (map (fn [high low] (/ (- high low) ssdi)) (get-obs-high numobs) (get-obs-low numobs))))

(defn get-discrete-state
  [learner state]
  (let [{obslow :obslow
         disc-os-win-size :disc-os-win-size} (deref (:q-table learner))]
    ;;(println "state=" state "low=" obslow "win=" disc-os-win-size)
    (let [discstate (vec
                     (doall
                      (map (fn [state low winsize] (int (/ (- state low) winsize)))
                           state obslow disc-os-win-size)))]
       ;;(println "discstate=" discstate)
      discstate)))

(defn make-gym-interface
  [world-name routing channel exchange]
  (let [interface (dplinterface.        ; dpl/make-dpl-interface
                   world-name           ; :world-parameters
                   routing              ; :routing
                   channel              ; :channel
                   exchange             ; :exchange
                   (fn [self]           ; :initialize-world
                     (initialize-simulator
                      self (first (:world-parameters self))))
                   (fn [self] (shutdown self))              ; :shutdown
                   (fn [self action] (perform self action)) ; :perform
                   (fn [self] (reset self))                 ; :reset
                   (fn [self] (render self))                ; :render
                   (fn [self state done]; :goal-achieved
                     (goal-achieved state done))
                   (fn [learner state]     ; :get-discrete-state
                     (get-discrete-state learner state))
                   (fn [self field]     ; :get-field-value
                     (DPL/get-field-value :gml field))
                   (fn [self field val] ; :set-field-value
                     (DPL/updatefieldvalue :gml field val))
                   (fn [self numobs]    ; :get-current-state
                     (get-current-state numobs)))]
    interface))

;;; Fin
