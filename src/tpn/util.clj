;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.util
  (:import (java.net Socket)
           (java.io OutputStreamWriter))
  (:require [tpn.type]
            [tpn.fromjson]
            [clojure.pprint :refer :all]
            [clojure.string :as str]
            [clojure.data :refer :all]
            [clojure.set :as set]
            [clojure.java.io :refer :all]))

(def debug nil)

;;; To show nested json in viewer
(defn send-to [host port content]
  (let [client (Socket. host port)
        wtr (new OutputStreamWriter (. client (getOutputStream)))]
    (binding [*out* wtr]
      (print (format "%08x%s" (count content) content))
      (flush))
    (. client (close))))

(defn as-str-fn [fn]
  "Returns clojure function object as string. Ex: Return value will be tpn.dispatch/dispatch-network"
  (str/replace (first (str/split (str fn) #"@")) #"\$" "/"))

(defn err-println
  "Prints msg and pretty prints obj to stderr."
  ([msg obj]
   (binding [*out* *err*]
     (println msg)
     (when obj (pprint obj))))
  ([msg]
   (err-println msg nil)))

(defn debug-object [msg obj fn]
  "Helper function is used to print where the issue was caught along with message
  and the object that caused the issue"
  (err-println (str (as-str-fn fn) " " msg) obj))

(defn show-updates [old-m new-m]
  "Helper function to show the changes between old and new map.
  Shows new keys and value changes for shared keys"
  (let [[in-old in-new _] (diff old-m new-m)
        updates (set/intersection (set (keys in-old)) (set (keys in-new)))
        new (set/difference (set (keys in-new)) (set (keys in-old)))]

    (when-not (empty? new)
      (println "New keys for uid" (:uid old-m))
      (doseq [k new]
        (println k "=" (k new-m))
        ))
    (when-not (empty? updates)
      (println "Updates for uid" (:uid old-m) " ## new-val <- old-val")
      (doseq [k updates]
        (println k "=" (k new-m) "<-" (k old-m))))))

; State update helper fns
(defn add-kv-to-objects! [k v objects]
  "Updates objects with k v. Ensures key is not nil and assumes objects is gaurded with atom"
  (if-not k (debug-object "nil key. Not adding to map." v add-kv-to-objects!)
            (swap! objects assoc k v)))

(defn remove-keys [k-set objects]
  (swap! objects (fn [derfd]
                   (apply dissoc derfd k-set))))

(defn get-updated-object [m objects]
  "Returns the object(m) if it does not exists, otherwise the merged object"
  (if-not ((:uid m) objects)
    m
    (merge ((:uid m) objects) m)))

(defn update-object! [m objects]
  "Updates the object with new map. expects (:uid m) is not nil"
  (when-not (:uid m) (debug-object "uid is nill" m update-object!))
  (let [derfd @objects]
    (when (:uid m)
      (when (and debug ((:uid m) derfd))
        (show-updates ((:uid m) derfd) m))
      (add-kv-to-objects! (:uid m) (get-updated-object m derfd) objects))))

(defn remove-load-ns [ns file]
  "Remove namespace ns and load file"
  (remove-ns ns)
  (load-file file))

(defn make-uid [prefix]
  (keyword (gensym prefix)))

(defn get-object [uid m]
  "Given a valid uid return the object"
  (when-not uid
    (err-println (str "uid is nil " (as-str-fn get-object)))
    #_(clojure.inspector/inspect m)
    )                                                       ;; TODO add fn call trace here
  (when uid (uid m)))

(defn get-network-object [m]
  (get-object (:network-id m) m))

(defn get-activities [nid m]
  (:activities (get-object nid m))
  )

#_(defn get-end-node-object [act-id m]
    (get-object (:end-node (get-object act-id m)) m)
    )

(defn get-end-node-activity [act m]
  (get-object (:end-node act) m))

(defn get-end-node [obj m]
  "Get end node for the object or return nil"
  (let [has-end-node (:end-node obj)]
    #_(println (:uid obj) has-end-node)
    ; state nodes and c/p-end nodes do not have :end-node ;TODO Fix warnings for state and *-end nodes.
    (cond has-end-node
          (get-object (:end-node obj) m)

          (contains? tpn.type/edgetypes (:tpn-type obj))
          (get-end-node-activity (get-object (first (:activities obj)) m) m)
          :otherwise
          (binding [*out* *err*]
            (println "Warning get-end-node for obj is nil" obj)
            nil))))

(defn print-persistent-queue [q]
  (print "[")
  (doseq [x q]
    (print x " "))
  (print "]"))

(defn get-constraints [act-id m]
  (let [act (get-object act-id m)
        constraints (map (fn [cid]
                           (get-object cid m)
                           ) (:constraints act))
        ]
    constraints))

;;; Function to read tailer generated CSV line containing timestamp and json message
;;; Return time-stamp as ? and json message as clj-map
(defn parse-tailer-json-line [line]
  (let [time-begin (inc (str/index-of line ","))
        time (.substring line time-begin)
        time-end (+ time-begin (str/index-of time ","))

        ts (str/trim (.substring line time-begin time-end))
        data (str/trim (.substring line (inc time-end)))
        ]

    {:recv-ts (read-string ts)
     :data    (tpn.fromjson/map-from-json-str data)}
    ))

(defn read-lines [fname]
  "Read lines from a file"
  (with-open [r (reader fname)]
    (doall (line-seq r))))
; (tpn.util/send-to "localhost" 34170 (slurp "test/data/tpn.json"))