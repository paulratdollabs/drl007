;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.app-state
  (:require [clojure.pprint :refer :all]
            [tpn.util :refer :all]))

; Keys are keywords, values are defrecord objects
(def objects (atom {}))

(defn reset []
  (reset! objects {}))

(defn update-networks! [netid]
  "Updates the :networks value with given netid. Expects netid to be not nil"
  (when-not netid (debug-object "netid is nil" nil update-networks!))
  (when netid (if-not (:networks @objects)
                (add-kv-to-objects! :networks #{netid} objects)
                (add-kv-to-objects! :networks (conj (:networks @objects) netid) objects))))

(defn update-objects! [objs]
  "Expects a map containing tpn objects indexed by their uid.
  If there is a :network-id non-nil-value, it will be added to :networks #{}."
  ; :network-id will be added to :networks #{}
  ; objs with uid will be added to objects with uid as key and obj as value.
  ;   new objects will be added
  ;   updates to objects should be displayed, new keys with values and existing keys with old and new values.
  ; Other objects without uid should be reported as error
  (let [rest (dissoc objs :network-id)]
    (doseq [obj (vals rest)]
      (update-object! obj objects)))
  (when (:network-id objs)
    (update-networks! (:network-id objs)))
  objs)

(defn get-networks []
  "Returns the set containing network uids"
  (:networks @objects))

(defn get-network [uid]
  "Returns the network object associated with the uid"
  (if uid (uid @objects)
          (debug-object "netid is nil" nil get-network)))

#_(defn print-objects []
    (println "{")
    (doseq [kv @objects]
      (println (first kv) @(second kv)))
    (println "}")
    )

#_(defn print-networks []
    (remove nil? (map (fn [[_ v]]
                        (if (= (type v) network)
                          v)) @objects)))

#_(defn print-objects-by-type [typ]
    (doseq [[_ v] @objects]
      (if (= (type v) typ)
        (println (type v) (:uid v) v))))

#_(defn print-acts []
    (print-objects-by-type activity))
