;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.tpn-walk
  (:require [tpn.tpnrecords :as tpn]))

(defmulti walk-tpn
          (fn [obj _ _ _]
            (type obj)))

(defn set-visited! [obj visited]
  (var-set visited (conj @visited (:uid obj)))
  #_(println "set-visited!" @visited "\n"))

(defn visited? [obj visited]
  (contains? @visited (:uid obj)))

(defn print-walk [obj _]
  (println "Visited" (:uid obj) (type obj)))

(defn begin-walk [obj func objects]
  (if-not obj
    (println "begin-walk obj is nil"))
  (if-not objects
    (println "begin-walk objects is nil"))
  (if-not func
    (println "walk fun is nil. Assuming print-walk"))

  (with-local-vars [visited #{}]
    (walk-tpn obj (if func func print-walk) objects visited)))

(defmethod walk-tpn :default [obj func _ _]
  (binding [*out* *err*]
    (println "walk-tpn handle \ntype:" (type obj) "\nfunc:" func "\nobj" obj)))

(defn walk-tpn-begin [obj func objects visited]
  (if-not (visited? obj visited)
    (do (func obj objects)
        (set-visited! obj visited)
        (doseq [act (:activities obj)]
          #_(println "walking" act " of " (:activities obj))
          (walk-tpn (act objects) func objects visited)))
    #_(println "already visited" (:uid obj) (:tpn-type obj)))
  )

(defmethod walk-tpn tpn.tpnrecords.p-begin [obj func objects visited]
  #_(println "\nwalk p-begin" (:uid obj) (:end-node obj))
  (walk-tpn-begin obj func objects visited))

(defmethod walk-tpn tpn.tpnrecords.c-begin [obj func objects visited]
  #_(println "\nwalk c-begin" (:uid obj) (:end-node obj))
  (walk-tpn-begin obj func objects visited))

(defn walk-tpn-state [obj func objects visited]
  (if-not (visited? obj visited)
    (do (func obj objects)
        (set-visited! obj visited)
        (when (first (:activities obj))
          (walk-tpn ((first (:activities obj)) objects) func objects visited)))
    #_(println "already visited" (:uid obj) (:tpn-type obj))))

(defmethod walk-tpn tpn.tpnrecords.state [obj func objects visited]
  (walk-tpn-state obj func objects visited))

(defmethod walk-tpn tpn.tpnrecords.p-end [obj func objects visited]
  (if-not (visited? obj visited)
    (walk-tpn-state obj func objects visited)
    #_(println "already visited" (:uid obj) (:tpn-type obj))))

(defmethod walk-tpn tpn.tpnrecords.c-end [obj func objects visited]
  (if-not (visited? obj visited)
    (walk-tpn-state obj func objects visited)
    #_(println "already visited" (:uid obj) (:tpn-type obj))))


(defn walk-tpn-activity [obj func objects visited]
  (if-not (visited? obj visited)
    (do (func obj objects)
        (set-visited! obj visited)
        (walk-tpn ((:end-node obj) objects) func objects visited))
    #_(println "already visited" (:uid obj) (:tpn-type obj))))

(defmethod walk-tpn tpn.tpnrecords.null-activity [obj func objects visited]
  (walk-tpn-activity obj func objects visited))

(defmethod walk-tpn tpn.tpnrecords.activity [obj func objects visited]
  (walk-tpn-activity obj func objects visited))

(defmethod walk-tpn tpn.tpnrecords.delay-activity [obj func objects visited]
  (walk-tpn-activity obj func objects visited))

; Collect tpn objects
; --------------------------------------------------------------------------

(defn collect-tpn [netid objs]
  "Walk the tpn and returns a map of nodes and edges including constraints but not network-id"
  #_(println "objs")
  #_(clojure.pprint/pprint objs)
  (with-local-vars [m {}]
    (let [network (netid objs)
          bnode (:begin-node network)
          ]
      (var-set m (assoc @m netid network))
      #_(println "collect-tpn" objs)
      #_(clojure.pprint/pprint @m)
      (begin-walk (bnode objs) (fn [obj _]
                                 ;(println "got " (:uid obj) (type obj))
                                 (var-set m (assoc @m (:uid obj) obj #_(merge obj {:checkme (.getSimpleName (type obj))})))
                                 #_(clojure.pprint/pprint @m)
                                 (when-not (empty? (:constraints obj))
                                   #_(println "constraints" (:constraints obj))
                                   (doseq [c (:constraints obj)]
                                     (var-set m (assoc @m c (c objs)))))
                                 ) objs)
      @m
      )))

(defn collect-tpn-with-netid [netid objs]
  "Walk the tpn and returns a map containing nodes, edges, constraints and network-id"
  (merge (collect-tpn netid objs) {:network-id netid}))

(defn collect-tpn-ids [netid objs]
  "Walks the TPN and returns a vector containing ids of nodes and edges along with netid"
  (with-local-vars [ids [netid]]
    (begin-walk (tpn/get-begin-node netid objs) (fn [obj _]
                                                  (var-set ids (conj @ids (:uid obj)))
                                                  ) objs)
    @ids))