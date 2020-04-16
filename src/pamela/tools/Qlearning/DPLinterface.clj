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
                          get-discrete-state goal-achieved])

(defn make-dpl-interface
  [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11]
  (dplinterface. a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11))

;;; Fin
