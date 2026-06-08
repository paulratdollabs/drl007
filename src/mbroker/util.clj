;; DISTRIBUTION STATEMENT C: U.S. Government agencies and their contractors.
;; Other requests shall be referred to DARPA’s Public Release Center via email at prc@darpa.mil.

(ns mbroker.util
  (:require [mbroker.rabbitmq :as q]
            [tpn.fromjson]
            [langohr.basic :as lb]
            [clojure.pprint :refer :all]
            [clojure.data.json :as json]))

(defonce state (atom {}))

(defn merge-state! [m]
  (swap! state merge m))


(defn setup [exch-name config]
  (if (@state exch-name)
    (println "Already have connection for " exch-name)
    (merge-state! {exch-name (q/make-channel exch-name config)})))

(defn send-msg-str [msg & exch-name]
  (let [exch (if (first exch-name)
               (first exch-name)
               "tpn-updates")
        ex-info (@state exch)
        routing-key "network_visualization"
        channel (:channel ex-info)]

    (println "to exchange" exch)
    (pprint ex-info)
    (lb/publish channel exch routing-key msg)))


(defn send-msg [clj-data & exch]
  (send-msg-str (json/write-str clj-data) exch))

(defn payload-to-json [payload]
  (let [data (String. payload "UTF-8")
        js (tpn.fromjson/map-from-json-str data)]
    js))
