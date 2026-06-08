;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.nested2flat
  (:require [clojure.pprint :refer :all]
            [tpn.util :refer :all]
            [tpn.fromjson]
            [tpn.type :refer :all]))

; Global state
(def objects (atom {}))

; Keys that have renamed in flat model.
(def key-replacement {:tpn-state        :state :tpn-parallel-begin :p-begin :tpn-parallel-end :p-end
                      :tpn-choice-begin :c-begin :tpn-choice-end :c-end})


(defn get-value [uid key]
  (key (uid @objects)))

(defn update-value [uid key value]
  #_(println "update value" uid key value)
  (let [obj (uid @objects)
        obj (merge obj {key value})]

    #_(println "update obj value" obj key value)
    (tpn.util/update-object! obj objects)))


(defn node-converter [node]
  "To convert nested node object to flat node object"
  #_(println "node-converter" node)
  (let [node (merge node {:uid (:nodeid node) :incidence-set #{} :activities #{} :constraints #{}})
        node (dissoc node :nodeid :type)
        old-tpn-type (:tpn-type node)
        new-tpn-type (old-tpn-type key-replacement)
        node (merge node {:tpn-type new-tpn-type})]

    #_(println old-tpn-type new-tpn-type "-> node" node)
    {(:uid node) node}))

(defn fix-type-of-activity [edge]
  "Detect null activity. Change :tpn-type to correct type"
  (let [atype (cond (and (not (:tpn-type edge))
                         (== 0.0 (:tc-lb edge) (:tc-ub edge)))
                    :null-activity
                    (= :tpn-activity (:tpn-type edge))
                    :activity
                    :else
                    (:tpn-type edge))]


    (if (= :null-activity atype)
      (dissoc (merge edge {:tpn-type atype}) :tc-lb :tc-ub :constraints :cost :network-flows)
      (merge edge {:tpn-type atype}))))

(defn edge-converter [edge]
  "To convert nested edge object to flat edge object"
  (let [edge (merge edge {:uid (:edgeid edge) :end-node (:toid edge)})
        ;_ (println "uid" edge)
        edge (dissoc edge :edgeid :type :toid)
        edge (fix-type-of-activity edge)
        ; Only activity has constraints object. null activities are assumed to be [0 0]
        ; constraint objects shoud not have :constraints
        edge (if (= :activity (:tpn-type edge))
               (merge edge {:constraints #{}})
               edge)]

    #_(println "created edge:" edge)
    {(:uid edge) edge}))


(defn as-map [list conv-fn]
  "Helper function to convert list of nested objects to flat objects"
  (into {} (map (fn [obj]
                  (conv-fn obj))
                list)))

;;; m is flat network with all nodes and edges
;;; nodes and edges are arrays where each object must exist in m.

;;; For each node, update its activities and incidence set.

;;; for each activity, detect if it is null-activity and change its type to null-activity
;;; For nested objects, null-activities in edges do not have key :tpn-type and bounds should be both 0.

;;; for each node and activity, if any constraints then add
;;;   constraint object to m and
;;;   uid to node or activity

(defn update-set-value-for-object [obj key value]
  "For the given flat object and key of the object, conj the value to the list"
  (let [vals (key obj)
        conjed (conj vals value)
        updated (merge obj {key conjed})]

    #_(println "updated")
    #_(pprint updated)
    (tpn.util/update-object! updated objects)
    updated))


(defn update-constraints [cnst]
  "To convert nested temporal constraint to flat temporal constraint"
  ; accord json has only temporal constraint
  #_(println "updating cnst" cnst)
  (let [fromnode ((:fromid cnst) @objects)
        lb (or (:tc-lb cnst) 0)
        ub (or (:tc-ub cnst) :infinity)
        cnst (merge cnst {:tpn-type :temporal-constraint :value [lb ub]})
        cnst (dissoc cnst :fromid :tc-lb :tc-ub)]


    (tpn.util/add-kv-to-objects! (:uid cnst) cnst objects)
    (update-set-value-for-object fromnode :constraints (:uid cnst))
    #_(println "after cnst dissoc" cnst)
    cnst))

(defn update-activity [edge]
  "Creates constraint object for the activity. Update nested object to conform to flat object schema. "
  #_(println "update-activity" edge)
  ; if an activity has valid constraints, (not= 0 :infinity) then create a constraint object and add to map
  ; update edge attributes
  (let [lb (or (:tc-lb edge) 0)
        ub (or (:tc-ub edge) :infinity)
        has-constraint (or (> lb 0) (not= :infinity ub))]

    (if has-constraint
      (let [uid (tpn.util/make-uid "cnstr-")
            value [lb ub]
            type :temporal-constraint
            enode (:end-node edge)
            cobj {:uid uid :value value :tpn-type type :end-node enode}
            edge (update-set-value-for-object edge :constraints uid)]

        #_(println "has constraint" cobj)
        (tpn.util/add-kv-to-objects! uid cobj objects)
        (tpn.util/add-kv-to-objects! (:uid edge) edge objects))))



  (let [edge (get-object (:uid edge) @objects)
        reward (or (:reward edge) 0)
        edge (if (= :activity (:tpn-type edge))
               (merge edge {:reward reward})
               edge)
        edge (dissoc edge :tc-lb :tc-ub :fromid)]

    #_(println "Final act updates" edge)
    (tpn.util/add-kv-to-objects! (:uid edge) edge objects)))


(defn update-activites-and-incidence-set [edge]
  (let [from ((:fromid edge) @objects)
        to ((:end-node edge) @objects)
        edgeid (:uid edge)
        edge (dissoc edge :fromid)]
    #_(println "edge" edge)
    #_(println "from" from)
    #_(println "to" to)
    (update-set-value-for-object from :activities edgeid)
    (update-set-value-for-object to :incidence-set edgeid)
    (tpn.util/add-kv-to-objects! (:uid edge) edge objects)))


(defn update-begin-node [node]
  "Find and update begin node for the TPN."
  (when (empty? (:incidence-set node))
    #_(println "Found begin node:" node)
    ; Check to ensure we have not already assigned begin node
    (let [net ((:network-id @objects) @objects)]
      (if (:begin-node net)
        (println "begin-node is already assigned" net)
        (tpn.util/update-object! (merge net {:begin-node (:uid node)}) objects)))))



(defn update-end-node [beginid endid stack]
  "Helper to update end-node of flat object."
  #_(println "update end node" beginid endid @stack)
  (update-value beginid :end-node endid)
  #_(println "before pop")
  (swap! stack pop)
  #_(println "after pop stack"))

; Walk the TPN and update end nodes
;; end-node is not required for state node but is required for parallel and choice begin
(defn fix-end-node [obj visited stack]
  #_(println "Visiting: " obj)
  #_(println "Stack" @stack)
  #_(println "Visted" @visited)

  ; for nodes dispatch activities
  ; for edge,  dispatch end node
  ; keep track of visited objects. Visit if not visited
  ; keep a stack of begin nodes
  ; for begin nodes ensure they do not have a end-node
  (let [type (:tpn-type obj)]
    #_(println "type" type)
    #_(when (contains? @visited (:uid obj))
        (println "Already visited" (:uid obj)))

    (when-not (contains? @visited (:uid obj))
      (swap! visited conj (:uid obj))
      (cond
        (contains? nodetypes type)
        (do
          ; we have a node
          #_(println "we have node" (:uid obj))
          (if (contains? begin-nodes type)
            ; we have a begin node
            (do
              #_(println "we have begin node" (:uid obj))
              (swap! stack conj (:uid obj))
              #_(println "stack updated?" @stack)))


          (if (contains? end-nodes type)
            ; we have a end node
            (do
              #_(println "we have end node" (:uid obj))
              (cond (and (= :p-end type)
                         (= :p-begin (get-value (peek @stack) :tpn-type)))
                    ;we have found match for p-begin node
                    (update-end-node (peek @stack) (:uid obj) stack)

                    (and (= :c-end type)
                         (= :c-begin (get-value (peek @stack) :tpn-type)))
                    ;we have found match for c-begin node
                    (update-end-node (peek @stack) (:uid obj) stack)

                    :else
                    ())))
                      ;non matching node




          #_(println "stack calling activities for node" @stack)
          (doseq [act (:activities obj)]
            (fix-end-node (get-object act @objects) visited stack)))

        (contains? edgetypes type)
        ; we have a known activity
        (fix-end-node (get-object (:end-node obj) @objects) visited stack)

        #_(let [enode (get-object (:end-node obj))]
            (doseq [act (:activities enode)]
              (fix-end-node (get-object act) visited stack)))


        :else
        (println "fix-end-node unkown object" obj)))))
; cond ends


(defn update-tpn [nodes edges]
  "top level fn to convert nested objects to flat objects"
  (doseq [tmpedge edges]
    (let [edge ((:edgeid tmpedge) @objects)]

      #_(println "edge type" (:tpn-type edge))

      (when (or (= :activity (:tpn-type edge))
                (= :null-activity (:tpn-type edge))
                (= :delay-activity (:tpn-type edge)))
        (update-activites-and-incidence-set edge))

      (when (or (= :activity (:tpn-type edge))
                (= :delay-activity (:tpn-type edge)))
        (update-activity edge))

      (when (= :tpn-constraints (:tpn-type edge))
        (update-constraints edge)
        #_(println "updated cnst" ((:uid edge) @objects)))))




  (doseq [node nodes]
    (update-begin-node ((:nodeid node) @objects)))

  (println "network-id" (:network-id @objects))
  (fix-end-node (get-object (:begin-node ((:network-id @objects) @objects)) @objects)
                (atom #{}) (atom [])))

;;; Nested structure is:
;;; network -> nodes [{}, {}]
;;;         -> edges [{},{}]
;;;         -> network-flows {{},{}}
;;;         -> Label ""
;;;         -> layout ignore
;;;         -> etc ignore
(defn toflat
  ([from-file]
   (toflat from-file nil))
  ([from-file to-file]
    ;;; Nested structure
   (let [nested (tpn.fromjson/from-file from-file)
         network (:network nested)
         nodes (:nodes network)
         edges (:edges network)
         flowsmap (:network-flows network)                  ; optional

         nodesmap (as-map nodes node-converter)
         edgesmap (as-map edges edge-converter)
         netobj {:uid (tpn.util/make-uid "net-") :tpn-type :network}
         netmap (merge {(:uid netobj) netobj} nodesmap edgesmap flowsmap)
         netmap (merge netmap {:network-id (:uid netobj)})]

     #_(println "edges map")
     #_(pprint edgesmap)

     #_(println "nodes map")
     #_(pprint nodesmap)

     (reset! objects netmap)
     (update-tpn nodes edges)
     #_(pp/pprint @objects)
     #_(doseq [[k v] nested]
         (prn k v)
         (pp/pprint v))


     (if to-file
       (tpn.fromjson/to-file @objects to-file)
       #_(pp/pprint @objects)
       #_(do
           (pp/pprint nodesmap)
           (pp/pprint edgesmap)
           (pp/pprint flowsmap)
           (pp/pprint netmap)))

     @objects)))

; Usage Example (tpn.nested2flat/batch-flat "/Users/prakash/projects/pam-git-bb/mission-models-lisp/scale-tests/" files)
; where files is ["tpn-2-3-4.nested.json" "tpn-2-3-5.nested.json" "tpn-2-4-4.nested.json" "tpn-3-3-2.nested.json"
; "tpn-3-3-3.nested.json" "tpn-3-3-4.nested.json" "tpn-3-4-3.nested.json" "tpn-3-4-4.nested.json"]

(defn batch-flat [dir files]
  "Assume input files have 'nested' somewhere. Ex: tpn-2-3-4.nested.json. This fn will then create a flat
  file with nested replaced by flat. tpn-2-3-4.flat.json in the 'dir' directory. Note 'dir' must have a trailing slash
  and it must be a fully qualified path"
  (let [fnames (map (fn [if]
                      ;(println "input file" if)
                      ; (println "outfile" (clojure.string/replace if "nested" "flat"))
                      (list if (clojure.string/replace if "nested" "flat")))
                    files)
        full-paths (map (fn [pair]
                          (list (str dir (first pair)) (str dir (second pair))))
                        fnames)]

    (println full-paths)
    (doseq [[if of] full-paths]
      (println if of)
      (toflat if of))))


