;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.dispatch-app
  (:require [mbroker.rabbitmq :as mq]
            [plant.connection :as plant]
            [plant.interface]
            [plant.util]
            [clojure.tools.cli :as cli]
            [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [tpn.dispatch :as dispatch]
            [tpn.tpn-walk]
            [tpn.import]
            [tpn.fromjson]
            [tpn.util :refer :all]
    ;:reload-all ; Causes problems in repl with clojure multi methods.
            [clojure.string :as string]
            [ruiyun.tools.timer :as timer]
            [clojure.core.async :as async])
  (:gen-class))

(def timeunit :secs)
(defonce state (atom {}))
(defonce blocking-q (async/chan))
(defonce activity-started-q (async/chan))
(defonce observations-q (async/chan))

(def monitor-mode nil)
(def no-tpn-publish false)                                  ;we always publish tpn by default.
(defn exit []
  (when-not (:repl @state) (System/exit 0)))

(defn update-state! [m]
  (swap! state merge @state m))

(defn print-state []
  (println "state -")
  (pprint @state)
  (println "state --"))

(update-state! {:repl      true
                :choice-fn tpn.dispatch/first-choice})
;(print-state)

(defn get-exchange-name []
  (:exchange @state))

(defn get-channel [exch-name]
  (get-in @state [exch-name :channel]))

(def cli-options [["-h" "--host rmqhost" "RMQ Host" :default "localhost"]
                  ["-p" "--port rmqport" "RMQ Port" :default 5672 :parse-fn #(Integer/parseInt %)]
                  ["-e" "--exchange name" "RMQ Exchange Name" :default "tpn-updates"]
                  ["-m" "--monitor-tpn" "Will monitor and update TPN state but not dispatch the begin tpn" :default false]
                  [nil "--no-tpn-publish" "When specified, we will not publish full tpn network" :default no-tpn-publish]
                  ["-?" "--help"]
                  ])

#_(defn parse-summary [args]
    (clojure.string/join " " (map (fn [opt]
                                    (str (:long-opt opt) " " (if (:default opt)
                                                               (str "[" (:default opt) "]")
                                                               (:required opt)))
                                    ) args)))

(defn reset-network
  ([net-objs]
   (let [netid (:network-id net-objs)
         ids   (tpn.tpn-walk/collect-tpn-ids netid net-objs)]
     (reset-network ids netid)))
  ([ids netid]
   (println "Reset network (internal)")
   (dispatch/reset-state-network ids)
    ;; reset state in viewer

   (println "Reset network (publish)")
   (let [st        (apply merge (map (fn [id]
                                       {id {:uid              id
                                            :tpn-object-state :normal}}
                                       ) ids))
         exch-name (get-exchange-name)
         channel   (get-channel exch-name)
         ]
     #_(pprint st)
     ; TODO refactor reset to a method
     (mbroker.rabbitmq/publish-object (merge st {:network-id netid}) "network.reset"
                                      channel exch-name)
     )))

(defn toMsecs [time unit]
  (println "time and unit" time unit)
  (if (= unit :secs)
    (* time 1000)
    time)
  )

(defn is-primitive [activity]
  (let [non-primitive (:non-primitive activity)]
    (if (nil? non-primitive)
      (do (println "activity :non-primitive is nil. Assuming primitive" activity)
          true)
      (not non-primitive))))

(defn get-plant-id [act-obj]
  (let [pl-id (:plantid act-obj)
        pl-id (if (pos? (count pl-id))
                pl-id
                "plant")]
    pl-id))

(defn publish-to-plant [tpn-activities]
  (let [plant (:plant-interface @state)]
    (when plant
      (let [call-ids     (plant.util/make-method-ids (count tpn-activities))
            method-calls (map (fn [id act-id]
                                {id (get (:tpn-map @state) act-id)})
                              call-ids (keys tpn-activities))]

        (doseq [call-info method-calls]
          (let [invocation-id (first (first call-info))
                act-obj       (second (first call-info))
                plant-id      (get-plant-id act-obj)
                command       (:command act-obj)
                args          (or (:args act-obj) [])
                argsmap       (or (:argsmap act-obj) {})
                plant-part    (:plant-part act-obj)]
            (update-state! {invocation-id (:uid act-obj)})
            (plant.interface/start plant plant-id invocation-id command args argsmap plant-part nil)
            ))))))

(defn publish-dispatched [dispatched tpn-net]
  #_(println "publish-dispatched dispatched")
  #_(pprint dispatched)
  (let [netid          (:network-id tpn-net)
        {tpn-activities true tpn-objects false} (group-by (fn [[_ v]]
                                                            ;(println "k=" k "v=" v)
                                                            (if (= :negotiation (:tpn-object-state v))
                                                              true
                                                              false
                                                              )) dispatched)
        exch-name      (get-exchange-name)
        channel        (get-channel exch-name)
        ; group-by returns vectors. we need maps.
        tpn-activities (apply merge (map (fn [[id v]]
                                           {id v}
                                           ) tpn-activities))
        delays         (filter (fn [[id v]]
                                 (if (and (= :delay-activity (:tpn-type (id tpn-net)))
                                          (= :started (:tpn-object-state v)))
                                   true
                                   false))
                               dispatched)]
    #_(println (into {} tpn-objects))
    #_(println (into {} tpn-activities))
    ; TODO Refactor update, negotiation and finished event publish to methods.
    ; FIXME make routing from TPN activity plant info
    (when (pos? (count tpn-objects))
      (mq/publish-object (merge (into {} tpn-objects) {:network-id netid}) "tpn.object.update" channel exch-name))
    (when (pos? (count tpn-activities))
      (mq/publish-object (merge tpn-activities {:network-id netid}) "tpn.activity.negotiation" channel exch-name)
      (publish-to-plant tpn-activities))

    ; if dispatched has any delay activities, then create a timer to finish them.
    (doseq [a-vec delays]
      (let [id      (first a-vec)
            cnst-id (first (get-in tpn-net [id :constraints]))
            cnst    (cnst-id tpn-net)
            lb      (first (:value cnst))
            msec    (toMsecs lb timeunit)
            obj     {:network-id netid (first a-vec) {:uid (first a-vec) :tpn-object-state :finished}}]
        (timer/run-task! #(mq/publish-object obj "tpn.activity.finished" channel exch-name)
                         :delay msec)))

    ; See if end-node is reached
    ;(println "\nChecking network finished?")
    (when (dispatch/network-finished? tpn-net)              ;network-finished? check walks the TPN. Might be inefficient for large TPNS. TODO
      (println "Network end-node reached. TPN Execution finished")
      (exit))
    ))

(defn monitor-mode-publish-dispatched [dispatched tpn-net]
  ;(pprint "Monitor mode publish dispatched before")
  ;(pprint dispatched)
  ;(pprint "after")
  ;(pprint (into {} (remove (fn [[k v]]
  ;                              (= :activity (:tpn-type (get-object k tpn-net))))
  ;                            dispatched)))
  (publish-dispatched (into {} (remove (fn [[k _]]
                                         (= :activity (:tpn-type (get-object k tpn-net))))
                                       dispatched)) tpn-net))

(defn act-finished-handler [act-id act-state netobjs m]
  (println "finished-activity" act-id act-state)
  (cond (or (= :finished act-state) (= :success act-state))
        (if monitor-mode
          (monitor-mode-publish-dispatched (dispatch/activity-finished (act-id netobjs) netobjs m) netobjs)
          (publish-dispatched (dispatch/activity-finished (act-id netobjs) netobjs m) netobjs))
        :else
        (println "act-finished-handler unknown state" act-state)
        ))

; Callback To receive messages from RMQ
(defn act-finished-handler-broker-cb [netobjs m _ _ ^bytes payload]
  #_(println "recvd from rmq: " payload)
  (async/>!! blocking-q {:payload payload
                         :m       m
                         :netobjs netobjs}))

(defn setup-broker-cb-with [netobjs m]
  (mbroker.rabbitmq/make-subscription "tpn.activity.finished"
                                      (partial act-finished-handler-broker-cb netobjs m)
                                      (get-channel (get-exchange-name))
                                      (:exchange @state)))

(defn process-activity-finished-msg [msg]
  "To process the message received from RMQ"
  (let [last-msg (:last-rmq-msg @state)]
    (when last-msg
      (binding [*out* *err*]
        (println "last rmq message. Sync incoming rmq messages")
        (clojure.pprint/pprint (:last-rmq-msg))
        )))

  (let [data (String. (:payload msg) "UTF-8")
        js   (tpn.fromjson/map-from-json-str data)]
    (update-state! {:last-rmq-msg js})
    (doseq [[_ v] js]
      (when (:tpn-object-state v)
        (act-finished-handler (:uid v) (:tpn-object-state v) (:netobjs msg) (:m msg)))
      )
    (update-state! {:last-rmq-msg nil})
    js))

(defn msg-serial-process []
  "To serially process each from RMQ"
  (async/go-loop [msg (async/<! blocking-q)]
    (if-not msg
      (println "Blocking Queue for RMQ incoming messages closed")
      (do
        (process-activity-finished-msg msg)
        (recur (async/<! blocking-q))))))

(defn process-acivity-started [act-msg]
  (pprint act-msg)
  (let [network-id  (get-in @state [:tpn-map :network-id])
        act-network (:network-id act-msg)
        m           (get @state :tpn-map)]

    (when (= act-network network-id)
      (println "Found network " network-id)
      (let [network-obj  (get-object network-id m)
            begin-node   (get-object (:begin-node network-obj) m)
            acts-started (disj (into #{} (keys act-msg)) :network-id)
            ]

        (if (dispatch/network-finished? m)
          (reset-network m))

        (if (dispatch/node-dispatched? begin-node m)
          (println "Begin node is dispatched")
          (do (println "Begin node is not dispatched")
              ; We are not passing choice fn as last parameter because we are in monitor mode and not making any decisions.
              (let [dispatched (dispatch/dispatch-object network-obj m {})]
                (println "Updating state of rest")
                (pprint (apply dissoc dispatched acts-started))

                (monitor-mode-publish-dispatched (apply dissoc dispatched acts-started) m))
              )
          )
        ))
    ))

(defn activity-started-handler [m]
  (when monitor-mode
    (async/>!! activity-started-q m)
    ))

(defn setup-activity-started-listener []
  "Assume the network is stored in app atom (@state)"

  (async/go-loop [msg (async/<! activity-started-q)]
    (if-not msg
      (println "Blocking Queue is closed for receiving messages 'RMQ activity started'")
      (do
        (process-acivity-started msg)
        (recur (async/<! activity-started-q)))))

  (update-state! {:activity-started-listener (mbroker.rabbitmq/make-subscription "tpn.activity.active"
                                                                                 (fn [_ _ ^bytes payload]
                                                                                   (let [data (String. payload "UTF-8")
                                                                                         m    (tpn.fromjson/map-from-json-str data)]
                                                                                     (println "Got activity started message")
                                                                                     (activity-started-handler m)
                                                                                     ))
                                                                                 (get-channel (get-exchange-name))
                                                                                 (:exchange @state)
                                                                                 )}))

(defn dispatch-tpn [tpn-net]
  #_(println "Dispatching TPN from file" file)

  ;; Guard so that we do not subscribe everytime we run from repl.
  ;; We expect to run only once from main.
  (when-not (:broker-subscription @state)
    (println "Setting subscription")
    (msg-serial-process)
    (setup-broker-cb-with tpn-net {:choice-fn (:choice-fn @state)})
    (if monitor-mode
      (setup-activity-started-listener))
    (update-state! {:broker-subscription true}))

  (update-state! {:tpn-map tpn-net})

  (let [netid     (:network-id tpn-net)
        network   (netid tpn-net)
        m         {:choice-fn dispatch/first-choice}
        exch-name (:exchange @state)
        channel   (get-in @state [exch-name :channel])
        ]
    (println "Use Ctrl-C to exit")

    (when-not no-tpn-publish
      (println "Publishing network")
      (mbroker.rabbitmq/publish-object tpn-net "network.new" channel exch-name))



    (reset-network tpn-net)
    (Thread/sleep 2000)

    (println "Dispatching netid" netid)
    (if-not monitor-mode
      (let [dispatched (dispatch/dispatch-object network tpn-net m)]
        (publish-dispatched dispatched tpn-net))
      (println "In Monitor mode. Not dispatching"))
    ))

(defn usage [options-summary]
  (->> ["TPN Dispatch Application"
        ""
        "Usage: java -jar tpn-dispatch-XXX-standalone.jar [options] tpn-File.json"
        ""
        "Options:"
        options-summary
        ""
        ]
       (string/join \newline)))

(defn get-tpn-file [args]                                   ;Note args are (:arguments parsed)

  (if (< (count args) 1)
    [nil "Need tpn-File.json"]
    (do
      (if (and (> (count args) 0) (.exists (io/as-file (first args))))
        [(first args) nil]
        [nil (str "File not found:" " " (first args))]
        ))))

(defn publish-activity-state [id state net-id routing-key]
  "To use when plant observations come through"
  (let [exch (get-exchange-name)
        ch   (get-channel exch)]

    (mq/publish-object {:network-id net-id
                        id          {:uid              id
                                     :tpn-object-state state}}
                       routing-key ch exch)
    )
  )

(defn handle-observation-message [msg]
  (println "Observation message")
  (pprint msg)
  (when (and (:id msg) ((:id msg) @state))
    (let [invocation-id (:id msg)
          act-id        (invocation-id @state)
          network       (:tpn-map @state)
          net-id        (:network-id network)]
      (cond (= :started (:state msg))
            (do (println "plant activity started invocation-id act-id" (:id msg) ((:id msg) @state))
                (publish-activity-state act-id :started net-id "tpn.activity.finished"))

            (= :finished (:state msg))
            (do (println "plant activity finished" (:id msg) ((:id msg) @state))
                (publish-activity-state act-id :finished net-id "tpn.activity.finished"))
            :else
            (println "Unknown observation state")
            ))))

(defn setup-recv-observations-from-q []
  (do (async/go-loop [msg (async/<! observations-q)]
        (if-not msg
          (println "Blocking Queue is closed for receiving #observation messages")
          (do
            (handle-observation-message msg)
            (recur (async/<! observations-q)))))
      (update-state! {:observations-q-setup true})))

(defn put-rmq-message [data q]
  (async/>!! q (tpn.fromjson/map-from-json-str (String. data "UTF-8"))))

(defn setup-plant-interface [exch-name host port]
  (update-state! {:plant-interface (plant/make-plant-connection exch-name {:host host :port port})})
  (if-not (:observations-q-setup @state)
    (do
      (setup-recv-observations-from-q)
      (mq/make-subscription "observations" (fn [_ _ data]
                                             (put-rmq-message data observations-q))
                            (get-in @state [:plant-interface :channel]) (:exchange @state)))
    (println "Observations q is already setup. ")))

(defn go [& [args]]
  (let [parsed      (cli/parse-opts args cli-options)
        help        (get-in parsed [:options :help])
        errors      (:errors parsed)
        [tpn-file message] (get-tpn-file (:arguments parsed)) ;(get-in parsed [:options :tpn])
        tpn-network (when tpn-file (tpn.import/from-file tpn-file))
        exch-name   (get-in parsed [:options :exchange])
        host        (get-in parsed [:options :host])
        port        (get-in parsed [:options :port])
        monitor     (get-in parsed [:options :monitor-tpn])
        no-tpn-pub  (get-in parsed [:options :no-tpn-publish])]

    (if-not monitor-mode
      (def monitor-mode monitor))

    (def no-tpn-publish no-tpn-pub)
    (println "Will publish? tpn" (not no-tpn-publish))
    (when errors
      (println (usage (:summary parsed)))
      (println (string/join \newline errors))
      (exit))

    (when help
      (println (usage (:summary parsed)))
      (exit))

    #_(clojure.pprint/pprint parsed)
    #_(clojure.pprint/pprint tpn-network)
    (update-state! (:options parsed))
    #_(print-state)

    (if tpn-file
      (do
        (println "Connecting to RMQ" host ":" port "topic" exch-name)
        (let [m (mq/make-channel exch-name {:host host :port port})]
          (if (:channel m)
            (do (update-state! {(:exchange m) m})
                (setup-plant-interface exch-name host port)
                (print-state)
                (dispatch-tpn tpn-network))
            (do
              (println "Error creating rmq channel")
              (exit)))
          ))
      (do
        (println (usage (:summary parsed)))
        (println message)
        (exit)
        ))))

; Dispatch TPN, Wait for TPN to finish and Exit.
(defn -main
  "Dispatches TPN via RMQ"
  [& args]
  ;(println "TPN Dispatch args" args)
  (update-state! {:repl false})
  (go args)
  )

; Command line parsing.
; https://github.com/clojure/tools.cli
