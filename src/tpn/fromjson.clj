;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.fromjson
  "Functions to read TPN JSON data into clojure data structures.
  Note: For conversion to defrecords, see import.clj"
  (:require [clojure.data.json :as json]))

(def debug nil)

(defn to-number-or-keyword [str]
  "Fn to convert stringified number to value or keyword"
  (try
    (Double/parseDouble str)
    (catch NumberFormatException _
      #_(println "converting to str" str)
      (keyword (.toLowerCase str)))))

(defn val-converter [k v]
  "Fn to convert values to appropriate types"
  (cond (contains? #{:nodeid :edgeid :fromid :toid :tpn-type :network-id :uid :end-node :begin-node :tpn-object-state
                     :sequence-end :state :id :finish-state :plant-id} k)
        (if v (keyword (.toLowerCase v))
              (when debug (println "val-converter: for key" k "value is nil")))

        (contains? #{:activities :incidence-set :constraints} k) (into #{} (map keyword v))

        (= :tc-lb k) (if (= String (type v))
                       (to-number-or-keyword v)
                       v)
        (= :tc-ub k) (if (= String (type v))
                       (to-number-or-keyword v)
                       v)

        (= :cost k) (if (= String (type v))
                      (to-number-or-keyword v)
                      v)

        (= :reward k) (if (= String (type v))
                        (to-number-or-keyword v)
                        v)

        ; Convert other keys as needed. For now they will be string.
        :otherwise v))

(defn map-from-json-str [content]
  (json/read-str content
                 :key-fn #(keyword %)
                 :value-fn val-converter)
  )

(defn from-file [filename]
  "Read json from a file."
  (with-open [rdr (clojure.java.io/reader filename)]
    (json/read rdr :key-fn #(keyword %)
               :value-fn val-converter)))

(defn to-file [m fname]
  "Write json to the file"
  (spit fname (json/write-str m))
  )

