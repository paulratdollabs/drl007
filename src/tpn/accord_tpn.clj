;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

;; Use tpn.nested2flat to convert nested accord objects to flat objects and then import it.
;;; @ Deprecated this namespace.
(ns tpn.accord-tpn
  (:require [tpn.tpnrecords :refer :all]
            [tpn.app-state]
            [tpn.fromjson]))

(def objects (atom {}))

(defn add-kv-to-objects [k v]
  (swap! objects assoc k v)
  )
;;;; Fns for working with @objects
(defn add-to-objects [obj]
  "Assumes that given obj has :uid value and uses the value to store the obj in 'objects' map"
  (add-kv-to-objects (:uid obj) obj)
  obj)

(defn update-object [uid m]
  "Updates the object with new map"
  #_(println "update-object " uid m)
  (add-to-objects (merge (uid @objects) m)))

(defn get-object [uid]
  (when uid (uid @objects)))

(defn find-or-make-object [uid m creator-fn]
  "Finds the object or creates and adds it."
  (if uid (if-not (@objects uid)
            (add-to-objects (creator-fn m :uid uid))
            (uid @objects))
          (add-to-objects (creator-fn m))))

; Multi methods for fom node
; Mutates objects
; ------------------------------------------------------------------------------------------------------------------
(defmulti find-or-make-node
          (fn [_ m]
            ;; Dispatch function
            (:tpn-type m)))

(defmethod find-or-make-node nil [uid m]
  (binding [*out* *err*]
    (println "Node : " uid " has no :tpn-type -> " m)))

(defmethod find-or-make-node :tpn-state [uid m]
  (find-or-make-object uid m make-state))

(defmethod find-or-make-node :tpn-parallel-begin [uid m]
  (find-or-make-object uid m make-p-begin))

(defmethod find-or-make-node :tpn-parallel-end [uid m]
  (find-or-make-object uid m make-p-end))

(defmethod find-or-make-node :tpn-choice-begin [uid m]
  (find-or-make-object uid m make-c-begin))

(defmethod find-or-make-node :tpn-choice-end [uid m]
  (find-or-make-object uid m make-c-end))

; Mutates objects
(defn find-or-make-activity [uid m]
  (if (and (= (:tc-lb m) 0)
           (= (:tc-ub m) 0))
    (find-or-make-object uid m make-null-activity)
    (find-or-make-object uid m make-activity)))

(defn find-or-make-temporal-constraint [uid m]
  (find-or-make-object uid m make-temporal-constraint))

; Mutates objects
(defn find-or-make-network [uid m]
  (find-or-make-object uid m make-network))

; Mutates objects
(defn update-node [from act to]
  "Updates from node's activity set and to node's incidence set with the given activity."
  (update-object from {:activities (conj (:activities (get-object from)) act)}) ; updates outgoing activity
  (update-object to {:incidence-set (conj (:incidence-set (get-object to)) act)}) ;  updates incoming activity
  )

(defn find-begin-node [nodes]
  "Find the list of nodes that have empty incidence set"
  (remove nil? (map (fn [node]
                      (when (= 0 (count (:incidence-set (get-object (:nodeid node)))))
                        node)
                      ) nodes)))

; Multi method for finding end-node
; ------------------------------------------------------------------------------------------------------------------
(defmulti find-end-node (fn [object _]
                          (type object)))

(defmethod find-end-node :default [object _]
  (binding [*out* *err*]
    (println "find-end-node handle type ->" (type object))))

(defn- find-end-node-state-helper [object stacklist]
  (when (first (:activities object))
    ;;;find-end-node of the only outgoing activity of the  object
    (find-end-node (get-object
                     (:end-node (get-object
                                  (first (:activities object)))))
                   stacklist)))

(defn- find-end-node-begin-helper [object stacklist]
  (let [m (transient {})]
    (doseq [activity (:activities object)]
      (conj! m (find-end-node (get-object (:end-node (get-object activity)))
                              (conj stacklist object))))
    (persistent! m)))

(defmethod find-end-node tpn.tpnrecords.state [object stacklist]
  (find-end-node-state-helper object stacklist))

(defmethod find-end-node tpn.tpnrecords.p-end [object stacklist]
  (merge {(:uid (first stacklist)) (:uid object)}
         (find-end-node-state-helper object (rest stacklist))))

(defmethod find-end-node tpn.tpnrecords.c-end [object stacklist]
  (merge {(:uid (first stacklist)) (:uid object)}
         (find-end-node-state-helper object (rest stacklist))))

(defmethod find-end-node tpn.tpnrecords.p-begin [object stacklist]
  (find-end-node-begin-helper object stacklist))

(defmethod find-end-node tpn.tpnrecords.c-begin [object stacklist]
  (find-end-node-begin-helper object stacklist))

(defn update-end-nodes [nodes]
  (doseq [[k v] nodes]
    #_(if (and (:end-node (get-object k))
               (not= v (:end-node (get-object k))))
        (println "Warning updating end-node for" k "old-val" (:end-node (get-object k)) "new value" v))
    (update-object k {:end-node v})))

(defn has-temporal-constraints [m]
  (and (or (:tc-lb m)
           (:tc-ub m))
       (not (and (= 0 (:tc-lb m))
                 (= 0 (:tc-ub m))))))

(defn constraint-exist-p [new-constraint]
  (fn [old-constraint]
    (= (:value old-constraint) (:value new-constraint))
    #_(and (= (:tc-lb (get-object old-constraint))
            (:tc-lb new-constraint))
         (= (:tc-ub (get-object old-constraint))
            (:tc-ub new-constraint))
         )))

(defn update-temporal-constraint [obj constraint]
  (if (empty? (:constraints obj))
    (update-object (:uid obj) (update-in obj [:constraints] (fn [previous]
                                                              (conj previous (:uid constraint))
                                                              )))
    ;; Make sure we are not adding duplicates
    (when-not (some (constraint-exist-p constraint) (:constraints (get-object (:uid obj))))
      (update-object (:uid obj) (update-in (get-object (:uid obj)) [:constraints] (fn [previous]
                                                                                    (conj previous (:uid constraint))
                                                                                    ))))
    ))

; Network import. Accord TPN Json representation
; ------------------------------------------------------------------------------------------------------------------
(defn import-accord-network [network]
  (let [{nodes :nodes edges :edges netid :network-id} network]
    (when (> (count edges) 0)
      ;; Create all nodes first so that edges can use them
      (doseq [n nodes]
        (find-or-make-node (:nodeid n) n)

        (when (has-temporal-constraints n)
          (update-temporal-constraint (get-object (:nodeid n))
                                      (find-or-make-temporal-constraint nil (merge n {:value [(:tc-lb n) (:tc-ub n)]}))))

        (when (:network n)
          (println "Found subnetwork for node: " (:nodeid n))
          (import-accord-network (:network n))))

      ; Edges containt either activities or constraints.
      ; Constraints are by default attached to the 'from' node.
      (doseq [e edges]
        (when (or (= (:tpn-type e) nil)                     ; null-activity does not has :tpn-type value
                  (= (:tpn-type e) :tpn-activity))

          (find-or-make-activity (:edgeid e) (merge e {:end-node (:toid e)}))

          (when (has-temporal-constraints e)
            (update-temporal-constraint (get-object (:edgeid e))
                                        (find-or-make-temporal-constraint nil (merge e {:value [(:tc-lb e) (:tc-ub e)]}))))

          (update-node (:fromid e) (:edgeid e) (:toid e)))

        ; Node constraints are represented as edges.
        (when (= (:tpn-type e) :temporal-constraint)
          (update-temporal-constraint (get-object (:fromid e))
                                      (find-or-make-temporal-constraint (:edgeid e)
                                                                        (merge e {:end-node (:toid e) :value [(:tc-lb e) (:tc-ub e)]})))
          )

        (when (:network e)
          (println "Found subnetwork for edge: " (:edgeid e))
          (import-accord-network (:network e))))

      (let [bnodes (find-begin-node nodes)
            net (find-or-make-network netid {:begin-node (:nodeid (first bnodes))})
            end-nodes (find-end-node (get-object (:nodeid (first bnodes))) '())]
        (update-end-nodes end-nodes)
        (add-kv-to-objects :network-id (:uid net))
        (:nodeid (first bnodes))))))

;; Call this when importing nested/accord json
(defn import-accord-tpn [filename]
  (let [{net :network} (tpn.fromjson/from-file filename)]
    (import-accord-network net)
    @objects))
