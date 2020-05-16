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
            [clojure.data.json :as json]
            [clojure.repl :refer [pst]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.set :as set]
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
                         routing channel exchange plantid
                         ;; Control of the simulator/plant
                         initialize-world shutdown
                         ;; Actions to be performed on the simulator
                         perform reset render
                         ;; Functions that return values
                         goal-achieved get-discrete-state
                         ;; Access to filed values
                         get-field-value set-field-value get-current-state
                         ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plant values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Verbosity for debugging

(def ^:dynamic *verbose* 0)

(defn set-verbosity
  [verbosity]
  (def ^:dynamic *verbose* verbosity)
  (println "Verbosity level set to " *verbose*))

(defn v1 [] (>= *verbose* 1))
(defn v2 [] (>= *verbose* 2))
(defn v3 [] (>= *verbose* 3))
(defn v4 [] (>= *verbose* 4))
(defn v5 [] (>= *verbose* 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Blocking remote calls to the plant

(def call-lock (Object.))
(def running-functions {})

(defn get-call-lock []
  call-lock)

(defn get-running-functions
  []
  running-functions)

(defn function-waiting?
  [id]
  (locking call-lock
    (get running-functions (keyword id))))

(defn function-finished
  [id val]
  ;;(println "In Function-call with id=" id "finished with result=" val)
  (let [apromise (locking call-lock
                   (let [apromise (get running-functions id)]
                     (def running-functions (dissoc running-functions id))
                     apromise))]
    (if apromise
      (deliver apromise val)
      (println "Can't find the function " id "to deliver."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (Temporary object implementztion - to be replaced by BSP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monitors

(def ^:dynamic *monitors* (atom #{}))
(def ^:dynamic *monitor-on* (atom true))

(defn set-monitor-on
  [trueorfalse]
  (reset! *monitor-on* trueorfalse))

(defn monitor-field
  [obj field]
  (reset! *monitors* (clojure.set/union #{[obj field]} (deref *monitors*))))

(defn monitoring-field?
  [obj field]
  (not (empty? (clojure.set/intersection (deref *monitors*) #{[obj field]}))))

(defn check-monitor
  [obj field value]
  (if (and (deref *monitor-on*) (v1) (monitoring-field? obj field))
    (println "Establishing " (format "%s.%s=%s" (name obj) (name field) (str value)))))

(defn check-monitor-update
  [kobj kfield value obj]
  (let [val (deref obj)]
    (if (and  (deref *monitor-on*) (v1) (monitoring-field? kobj kfield))
      (if (or (not (= value val)) (v2))
        (println "Updating " (format "%s.%s=%s" (name kobj) (name kfield) value))))))

(defn get-monitors
  []
  (let [mons (vec (doall (map (fn [memb] memb) (deref *monitors*))))]
    mons))

(defn print-monitors
  []
  (doseq [[obj field] (get-monitors)] (println (format "Monitoring %s.%s" (name obj) (name field)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object references

(def ^:dynamic *objects* {})

(def field-lock (Object.))


(defn print-field-values
  []
  (pprint *objects*))

(defn get-field-value
  [obj field]
  #_(println "Looking for " obj "." field)
  (locking field-lock
    (let [source (get *objects* (keyword obj))]
      (if source
        (let [value (get (deref source) (keyword field))]
          (if value
            (deref value)
            (do (println "field " obj "."  field "not found in " *objects*)
                :field-not-found)))
        (do (println "object " obj "not found in " *objects* "while looking for field " field)
            :object-not-found)))))

(defn updatefieldvalue
  [obj field value]
  (locking field-lock
    (let [kobj (keyword obj)
          kfield (keyword field)
          known-source (get *objects* kobj)] ; nil or an atom
      #_(println (format "Setting %s.%s=%s" (name kobj) (name kfield) (str value)))
      (if known-source
        (let [known-field (get (deref known-source) kfield)] ; The source is known, but what about the field?
          (if known-field
            (do
              (check-monitor-update kobj kfield value known-field)
              (reset! known-field value))                ; Source and field known so just set the value.
            (do
              (check-monitor kobj kfield value)
              (reset! known-source (merge (deref known-source) {kfield (atom value) }))))) ; add new field/value
        (do ; If the source is not known, the object the field and its value must be set
          (check-monitor kobj kfield value)
          (def ^:dynamic *objects* (merge  *objects* { kobj (atom { kfield (atom value) }) })))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RabbitMQ connectivity

(defonce last-ctag nil)
(def ctag nil)
(def received-count 0)
(def plantifid nil)
(def running-activities nil)
(def timeoutmilliseconds 20000) ; 30 second timeout
(def rmq-channel nil)

(defn set-plantifid
  [pifid]
  (def plantifid pifid))

(defn maybe-cancel-subscription
  [ctag]
  (when last-ctag
    (mq/cancel-subscription (first last-ctag) (second last-ctag)))
  ;; conj for list pushes to the front, so we push channel then ctag.
  ;; So, we get ctag = (first last-ctag), and channel = (second last-ctag)
  (def last-ctag (conj last-ctag rmq-channel ctag)))

(defn test-for-termination
  [act]
  (let [now (System/currentTimeMillis)
        [plid partid fname ts fn test] act]
    (if (> (- now ts) timeoutmilliseconds)
      (do (println "Timed out: " plid partid fname) true)
      (test))))

(defn finish-activity
  [act]
  (let [[plid partid fname ts fn test] act]
    (println "Finishing " plid partid fname)
    (fn)))

(defn check-for-satisfied-activities
  []
  (let [acts running-activities]
    (def running-activities nil)
    (doseq [act acts]
      (if (test-for-termination act)
        (finish-activity act)
        (def running-activities (concat running-activities (list act)))))
    (if (not (nil? running-activities)) (println "Running-activities = " running-activities))))

(defn add-running-activity
  [act]
  (def running-activities (concat running-activities (list act)))
  (println "Running-activities = " running-activities)
  (check-for-satisfied-activities))

(def fresh-data-lock (Object.))
(def fresh-data-promises {})

(defn await-plant-message
  [plant-id]
  (let [apromise (promise)]
    (locking fresh-data-lock
      (def fresh-data-promises (merge fresh-data-promises {plant-id apromise})))
    (deref apromise)))                     ; Block until the data is ready

(defn incoming-msgs [_ metadata ^bytes payload]
  (def received-count (inc received-count))
  #_(when (zero? (mod received-count 1000))
      (println "Messages received so far" received-count)
      )
  (let [st (String. payload "UTF-8")
        m (json/read-str st)] ; was (json/map-from-json-str st)
    #_(println st)
    #_(println "Meta")
    #_(clojure.pprint/pprint metadata)
    #_(println "--- from exchange:" (:exchange metadata) ",routing-key:" (:routing-key metadata))
    #_(clojure.pprint/pprint m)
    #_(println "raw-data," (System/currentTimeMillis) "," st)
    (let [rk (:routing-key metadata)
          command (get m "function-name")
          observations (get m "observations")
          plantid (keyword (get m "plant-id"))
          id (str (get m "id"))
          state (keyword (get m "state"))]
      #_(println "rk=" rk "state=" state "plantid=" plantid "plantifid=" plantifid "observations=" observations)
      (cond
        ;; Handle commands from the dispatcher to DMCP directly
        #_(and (= rk dmcpid))     #_(condp = command
                                      :get-field-value (get-field-value :gym m)
                                      ;; :set-field-value (set-field-value m)
                                      (println "Unknown command received: " command m))

        ;; Handle observations from plant
        (= rk "observations")
        (if true #_(= plantid plantifid)
            (do
              (if (= state :finished)
                ;; A previously called function has returned
                (do #_(println "Activity " id "finished with: " (get m "reason"))
                    (function-finished id (get m "reason"))))
              (doseq [anobs observations]
                #_(println "Look at obs=" anobs)
                (let [field (get anobs "field")
                      value (get anobs "value")]
                  (cond  field
                         (do #_(println "Received " plantid "." field "=" value)
                             (updatefieldvalue plantid field value))
                         :else
                         (do
                           (println "Received observation: " anobs)))))
              ;; After processing the update, see if anyone was waiting for them
              (let [apromise (locking fresh-data-lock
                               (let [oldpromise (get fresh-data-promises plantid)]
                                 (def fresh-data-promises (dissoc fresh-data-promises plantid))
                                 oldpromise))]
                (if apromise (deliver apromise plantid))))))
      ;; :else (println "plantid=" plantid "plantifid="  plantifid (if (= plantid plantifid) "same" "different") "observations=" (if observations (tpn.fromjson/map-from-json-str observations)))
      )
    (check-for-satisfied-activities)))

(defn rabbitMQ-connect
  [host port ch-name plantifid]
  (set-plantifid plantifid)
  (let [connection (rmq/connect {:host host :port port})
        channel (lch/open connection)
        _ (le/declare channel ch-name "topic")
        queue (lq/declare channel)
        qname (.getQueue queue)
        _ (lq/bind channel qname ch-name {:routing-key "#"})
        lctag (lc/subscribe channel qname incoming-msgs {:auto-ack true})]
    (maybe-cancel-subscription lctag)
    (def ctag lctag)
    (def rmq-channem channel)
    channel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Blocking plant call

(def call-counter 10)

(defn bp-call
  [self pid function args]
  (let [id (do (locking call-lock
                 (def call-counter (+ call-counter 1))
                 (str "dmrl" (str call-counter))))
        {exchange :exchange routing :routing channel :channel} self
        finished (promise)]
    (locking call-lock
      (def running-functions (merge running-functions {id finished})))
    (mq/publish-object
     {:id id
      :plant-id pid
      :exchange exchange
      :function-name function
      :args args} routing channel exchange)
    (deref finished)))

;;; Fin
