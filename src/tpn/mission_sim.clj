;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.mission-sim
  ^{:doc "An application that simulates exection of activities."}
  (:gen-class)
  (:require [tpn.fromjson]
            [mbroker.rabbitmq :as rmq]
            [tpn.util :refer :all]
            [clojure.pprint :refer :all]
            [clojure.tools.cli :as cli]
            [ruiyun.tools.timer :as timer]
            [clojure.string :as string]
            [clojure.data.csv :as csv]))

(def repl true)
(defonce connection-info nil)
(defonce networks (atom {}))
(defonce timer (timer/deamon-timer "Activity Sim Timer Daemon"))
; Time for which simulator will pretend to execute activity
(def activity-time 5)                                       ; unit is seconds
; When true, activity temporal bounds will be used to execute activity
; Temporal bounds will override activity-time.
; However in case of, infinity, acticity-time will be used.
(def use-temporal-bounds false)

(def mission-mode true)
(def plant-mode false)

(def cli-options [["-h" "--host rmqhost" "RMQ Host" :default "localhost"]
                  ["-p" "--port rmqport" "RMQ Port" :default 5672 :parse-fn #(Integer/parseInt %)]
                  ["-e" "--exchange name" "RMQ Exchange Name" :default "tpn-updates"]
                  ["-r" "--routing-key" "Routing Key for receiving commands from dispatcher" :default "tpn.activity.negotiation"]
                  [nil "--plant-ids plant-ids,as-csv" "Comma separated list of plant ids" :default "plant"]
                  ["-d" "--activity-time activity-time" "Time for which activity will run (in seconds)" :default 5 :parse-fn #(Integer/parseInt %)]
                  ["-t" "--use-temporal-bounds" "Activity execution time will be randomly chosen based on bounds(in seconds)" :default false]
                  ["-m" "--in-mission-mode true/false" "Mission Sim will respond to activity start/negotiation messages"
                   :default true :parse-fn #(Boolean/parseBoolean %)]
                  ["-n" "--in-plant-mode true/false" "Mission Sim will respond to plant start command messages.
                                                      If you enable this mode, I suggest you set -m to false"
                   :default false :parse-fn #(Boolean/parseBoolean %)]
                  ["-?" "--help"]
                  ])

(defn publish [id state net-id routing-key]
  (println "publish " id state net-id routing-key)
  (rmq/publish-object {:network-id net-id
                       id          {:uid              id
                                    :tpn-object-state state}}
                      routing-key (:channel connection-info) (:exchange connection-info)))

(defn get-activity-execution-time [act-id netid]
  (let [network (get @networks netid)
        constraints (filter (fn [constraint]
                              (= :temporal-constraint (:tpn-type constraint))
                              ) (get-constraints act-id network))
        bounds (if (or (empty? constraints) (false? use-temporal-bounds))
                 [0 activity-time]
                 (:value (first constraints)))]
    (* 1000 (second bounds))))

(defn rmq-data-to-clj [data]
  (tpn.fromjson/map-from-json-str (String. data "UTF-8")))

(defn incoming-msgs [_ _ data]
  (let [st (String. data "UTF-8")
        m (tpn.fromjson/map-from-json-str st)]
    (doseq [[k v] m]
      (when (instance? clojure.lang.IPersistentMap v)
        (println "Got activity start" k (:network-id m))
        (timer/run-task! #(publish k :active (:network-id m) "tpn.activity.active") :delay 100 :by timer)
        (timer/run-task! #(publish k :finished (:network-id m) "tpn.activity.finished") :delay (get-activity-execution-time k (:network-id m)) :by timer)
        ))))

(defn new-tpn [_ _ data]
  (let [strng (String. data "UTF-8")
        m (tpn.fromjson/map-from-json-str strng)]
    (println "Got new tpn" (:network-id m))

    (swap! networks assoc (:network-id m) m)
    (println)
    ))

(defn publish-command-state [id plant-id state timestamp reason]
  (rmq/publish-object (merge {:plant-id plant-id :id id :state state :timestamp timestamp} reason) "observations"
                      (:channel connection-info) (:exchange connection-info)))

(defn plant-commands [_ metadata data]
  (println "Got plant command")
  #_(println "metadata" metadata)
  (let [msg (rmq-data-to-clj data)]
    (pprint msg)

    (cond (= :start (:state msg))
          (do
            (println "Plant command: " (:function-name msg) (:args msg) "for plant-id" (:routing-key metadata))
            (timer/run-task! #(publish-command-state (:id msg) (:routing-key metadata) :started (System/currentTimeMillis) nil) :delay 500 :by timer)
            ; The plant does not know anything about the bounds. So activity sim time is activity-time
            (timer/run-task! #(publish-command-state (:id msg) (:routing-key metadata) :finished (System/currentTimeMillis)
                                                     {:reason {:finish-state :success
                                                               :str          (str "Finished simulating "
                                                                                  "command: " (:pclass msg) "." (:pmethod msg)
                                                                                  " " (:args msg)
                                                                                  )}}) :delay (* 1000 activity-time) :by timer)
            )
          :else
          (println "I cannot handle :state" (:state msg) "for id" (:id msg))
          )))

(defn usage [options-summary]
  (->> ["Mission Simulation Engine"
        ""
        "Usage: java -jar mission-sim-XXX-standalone.jar [options]"
        ""
        "Options:"
        options-summary]
       (string/join \newline)))

(defn print-help [summary-options]
  (println (usage summary-options)))

(defn exit [code]
  (when-not repl
    (System/exit code)))

(defn -main [& args]
  (let [parsed (cli/parse-opts args cli-options)
        opts (:options parsed)
        ;_ (pprint opts)
        help (:help opts)
        plant-ids (:plant-ids opts)
        plant-ids (first (csv/read-csv plant-ids))
        in-mission-mode (:in-mission-mode opts)
        in-plant-mode (:in-plant-mode opts)

        conn-info (rmq/make-channel (:exchange opts) opts)

        a-time (:activity-time opts)
        temporal-bounds (:use-temporal-bounds opts)
        ]

    ;(def repl false)
    (when (:errors parsed)
      (print-help (:summary parsed))
      (println (string/join \newline (:errors parsed)))
      (exit 0))

    (when help
      (print-help (:summary parsed))
      (exit 0))

    (def mission-mode in-mission-mode)
    (def plant-mode in-plant-mode)
    (println "Mission mode:" mission-mode)
    (println "Plant   mode:" plant-mode)

    (when (and mission-mode plant-mode)
      (println "Warning: Operating in mission mode and plant mode."))

    (when mission-mode
      (println "Setting up mission mode subscriptions")
      (rmq/make-subscription (:routing-key opts) incoming-msgs (:channel conn-info) (:exchange opts))
      (rmq/make-subscription "network.new" new-tpn (:channel conn-info) (:exchange opts)))

    (when plant-mode
      (println "plant-ids:" plant-ids)
      (doseq [plant-id plant-ids]
        (rmq/make-subscription plant-id plant-commands (:channel conn-info) (:exchange opts))))


    (def activity-time a-time)
    (println "Each activity / plant-command will run for:" activity-time "seconds")

    (def use-temporal-bounds temporal-bounds)
    (println "Using temporal bounds:" use-temporal-bounds)

    ; Cleanup
    (when connection-info
      (rmq/close-connection (:connection connection-info)))

    (def connection-info conn-info)
    (println "App State")
    (println "----------")
    (clojure.pprint/pprint connection-info)
    (println "----------")
    ))