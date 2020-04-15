;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;; DISTRIBUTION STATEMENT C: U.S. Government agencies and their contractors.
;; Other requests shall be referred to DARPA’s Public Release Center via email at prc@darpa.mil.


(ns mbroker.rabbitmq
  (:require [langohr.core :as rmq]
            [langohr.channel :as lch]
            [langohr.queue :as lq]
            [langohr.exchange :as le]
            [langohr.consumers :as lc]
            [langohr.basic :as lb]
            [clojure.data.json :as json]))

(def debug false)
(def default-exchange (str "test-" (System/getProperty "user.name")))
(def default-host "localhost")
(def default-port 5672)

(defn message-handler
  [_ metadata ^bytes payload]
  (println "Received message metadata" metadata)
  (clojure.pprint/pprint (String. payload "UTF-8"))
  ;[ch {:keys [content-type delivery-tag type] :as meta} ^bytes payload]
  #_(println (format "[consumer] Received a message: %s, delivery tag: %d, content type: %s, type: %s"
                     (String. payload "UTF-8") metadata)))

(defn close-connection [conn]
  (when (and conn (rmq/open? conn))
    (rmq/close conn)))

(defn close-channel [ch & [which-ch]]
  (when (and ch (rmq/open? ch))
    ;(:from-channel state)
    (println which-ch "channel is open. Closing")
    (lch/close ch)))

; Creates and returns a "topic" exchange with the given RMQ config.
(defn make-channel [exch-name rmq-config]
  #_(println "make-producer-channel")
  (let [config (or rmq-config {})
        conn (rmq/connect config)
        ch (lch/open conn)
        exch-name (or exch-name default-exchange)]
    (le/declare ch exch-name "topic")
    {:channel ch :config config :exchange exch-name :connection conn}))

(defn close-all [m]
  (close-connection (:connection m))
  (close-channel (:channel m)))

; TODO make publish fn thread safe as channels are not thread safe and could lead bogus data being written to the channel
(defn publish [data routing-key to-ch exch-name]
  ;(println "publishing data" data)
  (let [r-key (or routing-key "tpn.dispatcher.default")]
    (if (and to-ch exch-name)
      (lb/publish to-ch exch-name r-key data)
      (when debug (println "incomplete info for publishing data. channel-object and name" to-ch exch-name)))))

(defn make-subscription [sub-key msg-handler channel exch-name]
  (let [key (or sub-key "#")
        handler (or msg-handler message-handler)
        aq (if channel (lq/declare channel))
        qname (if aq (.getQueue aq)
                     (println "Subscription Q is nil. Channel must be nil."))
        _ (if qname (lq/bind channel qname exch-name {:routing-key key})
                    (println "Q binding failed ch qname key" channel qname key))
        c-tag (if qname (lc/subscribe channel qname handler {:auto-ack true}))
        ]
    {:routing-key key :consumer-tag c-tag :queue-name qname :message-handler handler}))

(defn cancel-subscription [^String consumer-tag channel]
  (when channel
    (langohr.basic/cancel channel consumer-tag)))

(defn publish-object [obj routing-key to-ch exch-name]
  "publish given object as json"
  ;(println "publishing" routing-key exch-name)
  ;(clojure.pprint/pprint obj)
  (publish (json/write-str obj) routing-key to-ch exch-name))

; (mbroker.rabbitmq/publish (slurp "/Users/prakash/projects/pac2man/doll/tpn/test/data/create-parallel-ex.test.json"))
