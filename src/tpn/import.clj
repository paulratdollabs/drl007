;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.import
  "Functions to convert TPN JSON to TPN defrecords"
  (:require [tpn.tpnrecords :as tpn]
            [tpn.fromjson :as json]
            [clojure.pprint :refer :all]))                  ;:rename {pprint mpp} ;sugar

(def debug true)

(defn make-object [obj create-fn]
  (if create-fn
    (create-fn obj :uid (:uid obj))
    (do
      (when debug (println "tpn.import/make-object create-fn is nil for" (:tpn-type obj)))
      obj)))

(defn from-map [objs]
  (with-local-vars [m {}]
    (doseq [[k v] objs]
      (if (:tpn-type v)
        (var-set m (assoc @m k (make-object v (tpn/tpn-type-2-create (:tpn-type v)))))
        (when debug (println "import tpn. No constructor for key" k "value" v))))
    (merge @m {:network-id (:network-id objs)})))

;; Call this when importing flat json
(defn from-file [filename]
  (from-map (json/from-file filename)))



;(import-tpn "./test/data/create-parallel-ex.json")