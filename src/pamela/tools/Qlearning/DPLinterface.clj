;; Copyright © 2020 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning

(ns pamela.tools.Qlearning.DPLinterface
  "DOLL Plant Learning Interface"
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
            [tpn.fromjson :as fromjson])
  (:gen-class)
  )

;;;(in-ns 'pamela.tools.Qlearning.DPL-interface)

;;; DOLL Plant Learning Interface

(defrecord dplinterface [;; World-specific parameters
                         world-parameters ; a list of arguments
                         ;; RabbitMQ information
                         routing channel exchange
                         ;; Control of the simulator/plant
                         initialize-world shutdown
                         ;; Actions to be performed on the simulator
                         perform reset render
                         ;; Functions that return values
                         goal-achieved
                         ;; Access to filed values
                         get-field-value set-field-value get-current-state
                         ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plant values

(def ^:dynamic *objects* {})
(def ^:dynamic *debug-objects* false)

(defn print-field-values
  []
  (pprint *objects*))

(defn get-field-value
  [obj field]
  (let [source (get *objects* (keyword obj))]
    (if source
      (let [value (get (deref source) (keyword field))]
        (if value
          (deref value)
          (do (println "field " obj "."  field "not found in " *objects*)
              :field-not-found)))
      (do (println "object " obj "not found in " *objects*)
          :object-not-found))))

(defn updatefieldvalue
  [obj field value]
  (let [kobj (keyword obj)
        kfield (keyword field)
        known-source (get *objects* kobj)] ; nil or an atom
    (if known-source
      (let [known-field (get (deref known-source) kfield)] ; The source is known, but what about the field?
        (if known-field
          (reset! known-field value)                ; Source and field known so just set the value.
          (reset! known-source (merge (deref known-source) { kfield (atom value) })))) ; add new field/value
      ;; If the source is not known, the object the field and its value must be set
      (def ^:dynamic *objects* (merge  *objects* { kobj (atom { kfield (atom value) }) })))
    (if *debug-objects* (println "***Set object" obj "field" field "=" value))))


;;; Fin
