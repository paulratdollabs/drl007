;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.dispatch
  "Keeps track of dispatched state of TPN"
  (:require
    [tpn.util :refer :all]
    ))

; Forward declarations.
(declare activity-finished?)
(declare dispatch-object)

(def state (atom {}))

(defn reset-state []
  (reset! state {}))

(defn reset-state-network [ids]
  "Walk the TPN for the given network and remove all objects from state"
  (remove-keys ids state))                                  ;(walk/collect-tpn-ids netid objects)

(defn update-dispatch-state! [uid key value]
  "Updates the runtime state of the object identified by uid"
  #_(println "update-state" uid key value)
  (if-not uid (debug-object (str "Cannot update run time state for nil uid " key " " value) nil update-dispatch-state!)
              (swap! state assoc-in [uid key] value)))

(defn simple-activity-dispatcher [act _ _]                  ;objs and m
  (println "simple-activity-dispatcher" (:uid act) "type" (:tpn-type act) (:name act)))

(defn first-choice [activities _]
  "Return the first activity"
  (if (empty? activities)
    (debug-object "Activity list is empty" activities first-choice)
    (first activities)))

(defn node-reached-helper [node state objs]
  "A node is reached if all the activities in the incidence set are finished."
  #_(println "\nnode-reached-helper" (:uid node))
  (with-local-vars [pending-completion #{}
                    finished #{}]
    (every? (fn [id]
              (let [finished? (activity-finished? (id objs) state)]
                (if-not finished?
                  (var-set pending-completion (conj @pending-completion id))
                  (var-set finished (conj @finished id)))))
            (:incidence-set node))
    ;(println "Choice end node? " (= :c-end (:tpn-type node)))
    (if (= :c-end (:tpn-type node))
      (do
        (if (> (count @finished) 1)
          (println "For choice node" (:uid node) "finished count is greater than 1." @finished))
        (println "Choice end node.pending and finished" @pending-completion @finished)
        [(>= (count @finished) 1) @pending-completion @finished]
        )
      [(empty? @pending-completion) @pending-completion @finished])
    ))

(defn check-node-state [node state objs]
  "Returns a [reached? #{activities pending completion} start-time (as-long or nil)"
  (let [nid (:uid node)
        start-time (get-in state [nid :start-time])
        [reached pending-completion finished] (node-reached-helper node state objs)]
    #_(println "node-reached-helper returned reached? pending-completion? finished?" reached pending-completion finished)
    [reached pending-completion start-time finished]))

(defn check-activity-state [act state]
  "Returns one of false, :dispatched, :finished and :error.
  false if not found in state
  :dispatched if there is :start-time associated with the activity and not :end-time
  :finished if there is both :start-time and :end-time associated with the activity
  :error otherwise"
  (let [id (:uid act)
        start-time (get-in state [id :start-time])
        end-time (get-in state [id :end-time])]
    #_(println "act start end " id start-time end-time)
    (cond (= nil (id state))
          (do
            ;(println "unknown activity" id)
            false)

          (and start-time (not end-time))
          :dispatched

          (= (and end-time (>= end-time start-time)))
          :finished

          :otherwise
          (do (debug-object "Error in Activity state" act check-activity-state)
              :error))))

(defn node-dispatched?
  "A node is dispatched if it has start-time"
  ([node objs]
   (node-dispatched? node @state objs))
  ([node state objs] #_(println "node dispatched?" (:uid node))
   (nth (check-node-state node state objs) 2)))

(defn node-reached? [node state objs]
  "A node is reached if all the activities in the incidence set are reached."
  #_(println "node reached?" (:uid node))
  (when node
    (first (check-node-state node state objs))))

(defn activity-started? [act state]
  (= :dispatched (check-activity-state act state)))

(defn activity-finished? [act state]
  (= :finished (check-activity-state act state)))

(defn activity-finished [act objs m]
  "To be called when the activity has finished its processing"
  (update-dispatch-state! (:uid act) :end-time (System/currentTimeMillis))
  #_(println "act finished" (:uid act))
  (let [end-node-id (:end-node act)
        end-node (end-node-id objs)]
    (when (node-reached? end-node @state objs)
      (dispatch-object end-node objs m)
      #_(merge
          ; Note. We are sending activity state update here. But plant/actuator already sends the update.
          ; This is duplicate state update and benign to the viewer but anyone else listening may get confused.
          {(:uid act) {:uid              (:uid act)
                       :tpn-object-state :finished
                       }}
          (dispatch-object end-node objs m)))))

(defn dispatch-activities [act-ids objs m]
  #_(println "Dispatching activities:" act-ids)
  (if (empty? act-ids)
    {}                                                      ;(dispatch-object ((first act-ids) objs) objs m)
    (conj (dispatch-object ((first act-ids) objs) objs m)
          (dispatch-activities (rest act-ids) objs m))))

(defn dispatch-object-state [node objs m]
  "Helper function to dispatch all the activities of the node
  Returns the list of activities dispatched."
  #_(println "dispatch-object-state" (:uid node) (:tpn-type node))
  (if (node-dispatched? node @state objs)
    (debug-object "Already dispatched node. Not dispatching" node dispatch-object)
    (do
      (update-dispatch-state! (:uid node) :start-time (System/currentTimeMillis))
      #_((:dispatch-listener m) node :reached)
      (dispatch-activities (:activities node) objs m))))

; Dispatch methods
(defmulti dispatch-object
          "Generic function to dispatch the obj
          objs is a map of objects in the tpn index by :uid
          m is map to contain additional information such as
          :activity-dispatcher -- The function that actually does something
          :choice-function -- The function to decide which activity should be dispatched for the choice node"
          (fn [obj _ _]
            (:tpn-type obj)))

(defmethod dispatch-object :default [obj _ _]
  (debug-object "dispatch-object :default" obj dispatch-object)
  #_(clojure.pprint/pprint obj)
  {(:uid obj) {:uid (:uid obj) :tpn-object-state :unkown}})

(defmethod dispatch-object :p-begin [obj objs m]
  #_(println "p-begin" (:uid obj) (:tpn-type obj) "-----------")
  (conj {(:uid obj) {:uid (:uid obj) :tpn-object-state :reached}}
        (dispatch-object-state obj objs m)))

(defmethod dispatch-object :p-end [obj objs m]
  #_(println "p-end" (:uid obj) (:tpn-type obj) "-----------")
  (conj {(:uid obj) {:uid (:uid obj) :tpn-object-state :reached}}
        (dispatch-object-state obj objs m)))

(defmethod dispatch-object :c-begin [obj objs m]
  #_(println "c-begin" (:uid obj) (:tpn-type obj) "-----------")
  (let [choice-fn (:choice-fn m)
        choice-act-id (choice-fn (:activities obj) m)
        choice-act (choice-act-id objs)]
    (update-dispatch-state! (:uid obj) :start-time (System/currentTimeMillis))
    (conj {(:uid obj) {:uid (:uid obj) :tpn-object-state :reached}}
          (dispatch-object choice-act objs m))))

(defmethod dispatch-object :c-end [obj objs m]
  #_(println "c-end" (:uid obj) (:tpn-type obj) "-----------")
  (conj {(:uid obj) {:uid (:uid obj) :tpn-object-state :reached}}
        (dispatch-object-state obj objs m)))

(defmethod dispatch-object :state [obj objs m]
  ;(println "dispatch-object state" (:uid obj) (:tpn-type obj) "-----------")
  (conj {(:uid obj) {:uid (:uid obj) :tpn-object-state :reached}}
        (dispatch-object-state obj objs m)))

(defmethod dispatch-object :activity [obj _ _]
  (update-dispatch-state! (:uid obj) :start-time (System/currentTimeMillis))
  {(:uid obj) {:uid (:uid obj) :tpn-object-state :negotiation}})

(defmethod dispatch-object :null-activity [obj objs m]
  #_(println "null-activity" (:uid obj) (:tpn-type obj) "-----------")
  (update-dispatch-state! (:uid obj) :start-time (System/currentTimeMillis))
  #_((:dispatch-listener m) obj :finished)
  (conj {(:uid obj) {:uid (:uid obj) :tpn-object-state :finished}}
        (activity-finished obj objs m)))

(defmethod dispatch-object :delay-activity [obj _ _]
  #_(println "dispatch-object :delay-activity" obj)
  (update-dispatch-state! (:uid obj) :start-time (System/currentTimeMillis))
  {(:uid obj) {:uid (:uid obj) :tpn-object-state :started}})

; Dispatch network / dispatch-object should return state of the object
(defmethod dispatch-object :network [obj objs m]
  "Entry point to dispatching the network"
  (update-dispatch-state! (:uid obj) :state :started)
  (let [;a-dispatcher (or (:activity-dispatcher m) simple-activity-dispatcher)
        choice-fn (or (:choice-fn m) first-choice)
        me (merge m {;:activity-dispatcher a-dispatcher
                     :choice-fn choice-fn})
        begin-obj ((:begin-node obj) objs)]
    (println "dispatching begin node")
    (dispatch-object begin-obj objs me)))

(defn print-node-run-state [val]
  (if (first val) (print "Reached")
                  (print "Not Reached"))
  (println " Pending" (second val) "Start time" (nth val 2))
  )

(defn has-activities? [node]
  (pos? (count (:activities node))))

(defn get-network-end-node [tpn-net current-node]
  ;(println "get-network-end-node for node" (:uid current-node))

  (if-not (has-activities? current-node)
    current-node
    (let [end (if (:end-node current-node)
                (get-object (:end-node current-node) tpn-net))

          end (if-not end
                (let [act-id (first (:activities current-node))
                      ;_ (println "activities" (:activities current-node))
                      act (get-object act-id tpn-net)
                      ]
                  (get-object (:end-node act) tpn-net))
                end)]
      (get-network-end-node tpn-net end)
      )
    ))

(defn network-finished? [tpn-net]
  ;(println "Check netwok finished? -----")
  (let [network (get-object (:network-id tpn-net) tpn-net)
        begin (get-object (:begin-node network) tpn-net)
        ;end (get-object (:end-node begin) tpn-net)
        end (get-network-end-node tpn-net begin)
        ]
    ;(clojure.pprint/pprint network)
    ;(clojure.pprint/pprint begin)
    ;(clojure.pprint/pprint end)

    (node-reached? end @state tpn-net)))

(defn print-activiy-run-state [val]
  (if val (println val)
          (println "Not dispatched")))

; Need object inheritance?
(defn print-state [ids state objects]
  (doseq [id ids]
    (println "Object" id (get-in objects [id :tpn-type]))
    (cond (contains? #{:p-begin :p-end :c-begin :c-end :state} (get-in objects [id :tpn-type]))
          (print-node-run-state (check-node-state (id objects) state objects))

          (contains? #{:activity :null-activity} (get-in objects [id :tpn-type]))
          (print-activiy-run-state (check-activity-state (id objects) state))
          (= :network (get-in objects [id :tpn-type]))
          (println (id state))
          )
    (println)))


