;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.tail-rmq
  (:gen-class)
  (:require [clojure.tools.cli :as cli]
            [mbroker.rabbitmq :as mq]
            [langohr.core :as rmq]
            [langohr.exchange :as le]
            [langohr.queue :as lq]
            [langohr.consumers :as lc]
            [langohr.channel :as lch]
            [clojure.string :as string]
            [tpn.fromjson]
            [mongo.db])

  (:import (java.text SimpleDateFormat)
           (java.util Date)))

(defonce sdf (SimpleDateFormat. "yyyy-MMdd-HHmm"))
(def received-count 0)

(def repl true)
(defonce last-ctag nil)

(def cli-options [["-h" "--host rmqhost" "RMQ Host" :default "localhost"]
                  ["-p" "--port rmqport" "RMQ Port" :default 5672 :parse-fn #(Integer/parseInt %)]
                  ["-e" "--exchange name" "RMQ Exchange Name" :default "tpn-updates"]
                  [nil "--dbhost dbhost" "Mongo DB Host"]
                  [nil "--dbport dbport" "Mongo DB Port" :parse-fn #(Integer/parseInt %)]
                  ["-n" "--name dbname" "Mongo DB Name" :default (str "rmq-log-" (.format sdf (Date.)))]
                  ["-?" "--help"]
                  ])

(defn incoming-msgs [_ metadata ^bytes payload]
  (def received-count (inc received-count))
  (when (zero? (mod received-count 10000))
    (println "Messages received so far" received-count)
    )
  (let [st (String. payload "UTF-8")
        m (tpn.fromjson/map-from-json-str st)]
    ;(println "Meta")
    ;(clojure.pprint/pprint metadata)
    (println "--- from exchange:" (:exchange metadata) ",routing-key:" (:routing-key metadata))
    (mongo.db/to-db m)
    (clojure.pprint/pprint m)
    (println "raw-data," (System/currentTimeMillis) "," st))
  )

(defn usage [options-summary]
  (->> ["Program to listen for all messages on a RMQ exchange"
        ""
        "Usage: java -jar tpn-tailer-XXX-standalone.jar [options]"
        ""
        "Options:"
        options-summary
        ""
        ]
       (string/join \newline)))

; Print messages as they are flowing through the channel
(defn -main
  "Tail messages through exchange"
  [& args]
  #_(println "args:" args)
  #_(println "args type:" (type args))
  (let [parsed (cli/parse-opts args cli-options)
        ;_ (println parsed)
        ch-name (get-in parsed [:options :exchange])
        host (get-in parsed [:options :host])
        port (get-in parsed [:options :port])
        help (get-in parsed [:options :help])
        connection (rmq/connect {:host host :port port})
        channel (lch/open connection)
        _ (le/declare channel ch-name "topic")
        queue (lq/declare channel)
        qname (.getQueue queue)
        _ (lq/bind channel qname ch-name {:routing-key "#"})
        ctag (lc/subscribe channel qname incoming-msgs {:auto-ack true})
        dbhost (get-in parsed [:options :dbhost])
        dbport (get-in parsed [:options :dbport])
        _ (when dbhost
            (mongo.db/connect! :host dbhost :port dbport)
            (mongo.db/get-db (get-in parsed [:options :name])))
        ]

    ;(clojure.pprint/pprint  last-ctag)
    (def repl false)
    (when help
      (println (usage (:summary parsed)))
      (when-not repl
        (System/exit 0)))

    (println "Tail Config" (:options parsed))
    (when last-ctag
      (mq/cancel-subscription (first last-ctag) (second last-ctag)))
    ; conj for list pushes to the front, so we push channel then ctag.
    ; So, we get ctag = (first last-ctag), and channel = (second last-ctag)
    (def last-ctag (conj last-ctag channel ctag))
    ctag))
