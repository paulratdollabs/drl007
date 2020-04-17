;; Copyright © 2018 Dynamic Object Language Labs Inc.
;; DISTRIBUTION STATEMENT C: U.S. Government agencies and their contractors.
;; Other requests shall be referred to DARPA’s Public Release Center via email at prc@darpa.mil.

(ns pamela.tools.Qlearning.cli
  "Q-Learning."
  (:require [clojure.tools.cli :as cli :refer [parse-opts]]
            [clojure.data.json :as json]
            [clojure.data.codec.base64 :as base64]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            ;;[me.raynes.fs :as fs]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [mbroker.rabbitmq :as mq]
            [langohr.core :as rmq]
            [langohr.exchange :as le]
            [langohr.queue :as lq]
            [langohr.consumers :as lc]
            [langohr.channel :as lch]
            [tpn.fromjson :as fromjson]
            [pamela.tools.Qlearning.DMQL :as dmql]
            [pamela.tools.Qlearning.GYMinterface :as gym])
  (:gen-class)) ;; required for uberjar

(def cli-options [;;["-m" "--model pm" "pamela model of system" :default nil]
                  ;; Pamela related options (Which is not yet hooked in)
                  ["-o" "--output file" "output" :default "spamela.txt"]
                  ["-m" "--model ir" "Model IR" :default nil]
                  ["-r" "--root name" "Root pClass" :default "main"]
                  ;; RabbitMQ related options
                  ["-h" "--host rmqhost" "RMQ Host" :default "localhost"]
                  ["-p" "--port rmqport" "RMQ Port" :default 5672 :parse-fn #(Integer/parseInt %)]
                  ["-e" "--exchange name" "RMQ Exchange Name" :default "dmrl"]
                  ;; These are learning related options - with reasonable defaults
                  ["-i" "--if id" "IF ID" :default "gym"] ; The interface ID (plant - robot or simulator)
                  ["-n" "--episodes n" "Number of Episodes" :default 100 :parse-fn #(Integer/parseInt %)]
                  ["-a" "--alpha f" "Learning Rate" :default 0.1 :parse-fn #(Float/parseFloat %)]
                  ["-d" "--discount f" "Discount Rate" :default 0.95 :parse-fn #(Float/parseFloat %)]
                  ["-x" "--explore f" "Portion of episodes to explore" :default 0.5 :parse-fn #(Float/parseFloat %)]
                  ["-c" "--cycletime ms" "Cycle time in milliseconds" :default 200 :parse-fn #(Integer/parseInt %)]

                  ["-g" "--gymworld gw" "Name of the Gym World" :default "MountainCar-v0"]
                  ;; Debugging options
                  ["-w" "--watchedplant id" "WATCHEDPLANT ID" :default nil]
                  ["-t" "--tracefile file" " Trace filename" :default nil]
                  ["-f" "--fromfile val" "Observations from file" :default 0]
                  ["-v" "--verbose level" "Verbose mode" :default "0"]
                  ;; Help
                  ["-?" "--help"]
                  ])

;;;plantid: gym, exchange: dmrl, host: localhost, port: 5672
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce last-ctag nil)

;;; Commands

(def rmq-channel nil)
(def exchange nil)
(def dmcpid nil)
(def watchedplant nil)
(def plantifid nil)
(def running-activities nil)
(def timeoutmilliseconds 20000) ; 30 second timeout
(def tracefilename nil)
(def exitmainprogram false)

(defn test-for-termination
  [act]
  (let [now (System/currentTimeMillis)
        [plid partid fname ts fn test] act]
    (if (> (- now ts) timeoutmilliseconds)
      (do (println "Timed out: " plid partid fname) true)
      (test))))

(defn finish-activity
  [act]
  (let [[plid partid fname ts fn test] act]
    (println "Finishing " plid partid fname)
    (fn)))

(defn check-for-satisfied-activities
  []
  (let [acts running-activities]
    (def running-activities nil)
    (doseq [act acts]
      (if (test-for-termination act)
        (finish-activity act)
        (def running-activities (concat running-activities (list act)))))
    (if (not (nil? running-activities)) (println "Running-activities = " running-activities))))

(defn add-running-activity
  [act]
  (def running-activities (concat running-activities (list act)))
  (println "Running-activities = " running-activities)
  (check-for-satisfied-activities))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def received-count 0)

(defn incoming-msgs [_ metadata ^bytes payload]
  (def received-count (inc received-count))
  (when (zero? (mod received-count 1000))
    (println "Messages received so far" received-count)
    )
  (let [st (String. payload "UTF-8")
        m (tpn.fromjson/map-from-json-str st)]
    #_(println "Meta")
    #_(clojure.pprint/pprint metadata)
    #_(println "--- from exchange:" (:exchange metadata) ",routing-key:" (:routing-key metadata))
    #_(clojure.pprint/pprint m)
    #_(println "raw-data," (System/currentTimeMillis) "," st)
    (let [rk (:routing-key metadata)
          command (keyword (get m :function-name))
          observations (get m :observations)
          plantid (get m :plant-id)]
      (cond
        ;; Handle commands from the dispatcher to DMCP directly
        #_(and (= rk dmcpid))     #_(condp = command
                                 :get-field-value (get-field-value m)
                                 ;; :set-field-value (set-field-value m)
                                 (println "Unknown command received: " command m))

        ;; Handle observations from plant
        (= rk "observations")    (if (= plantid plantifid)
                                   (doseq [anobs observations]
                                     (let [field (get anobs :field)
                                           value (get anobs :value)]
                                       (cond  field
                                              (do #_(println "Received " field "=" value)
                                                  (gym/updatefieldvalue field value))
                                             :else
                                             (do
                                               (println "Received observation: " anobs))))))
        ;; :else (println "plantid=" plantid "plantifid="  plantifid (if (= plantid plantifid) "same" "different") "observations=" observations)
)
      (check-for-satisfied-activities))))


(defn action-function
  [args]
  (println args))

(defn demo-function
  [args]
  nil)

(def #^{:added "0.1.0"}
  actions
  "Valid drql command line actions"
  {"q-learner" (var action-function)
   "demo" (var demo-function)})

(defn usage
  "Print drql command line help."
  {:added "0.1.0"}
  [options-summary]
  (->> (for [a (sort (keys actions))]
         (str "  " a "\t" (:doc (meta (get actions a)))))
    (concat [""
             "drql"
             ""
             "Usage: drql [options] action"
             ""
             "Options:"
             options-summary
             ""
             "Actions:"])
    (string/join \newline)))

(defn q-learner
  "DOLL Reinforcement Q-Learner"
  [& args]
  ;;(println args)
  ;;(println cli-options)
  (let [parsed (cli/parse-opts args cli-options)
        verbosity (read-string (get-in parsed [:options :verbose]))
        _ (if (> verbosity 1) (println parsed))
        model (get-in parsed [:options :model])
        outfile (get-in parsed [:options :output])

        ch-name (get-in parsed [:options :exchange])
        host (get-in parsed [:options :host])
        _ (if (> verbosity 0) (println ["host = " host]))
        exch (get-in parsed [:options :exchange])
        ifid (get-in parsed [:options :if])       ; Interface ID
        neps (get-in parsed [:options :episodes]) ; Number of episodes
        alph (get-in parsed [:options :alpha])    ; Learning rate
        disc (get-in parsed [:options :discount]) ; Discount rate
        expl (get-in parsed [:options :explore])  ; fraction of episodes for which exploration takes place
        cycl (get-in parsed [:options :cycletime]); Cycletime in milliseconds
        gwld (get-in parsed [:options :gymworld]) ; Name of the GYM world to instantiate
        wpid (get-in parsed [:options :watchedplant])
        trfn (get-in parsed [:options :tracefile])
        frfi (get-in parsed [:options :fromfile])
        port (get-in parsed [:options :port])
        _ (if (> verbosity 0) (println ["port = " port]))
        help (get-in parsed [:options :help])
        _ (if (> verbosity 0) (println ["help = " help]))
        root (symbol (get-in parsed [:options :root]))
        ;;_ (if root (println ["root = " root]))
        _ (if (and model (> verbosity 0)) (println ["model = " model]))
        _ (do
            (def repl false)
            (when help
              (println (usage (:summary parsed)))
              (when-not repl
                (System/exit 0))))

        _ (if (> verbosity 0) (println "DOLL Reinforcement Q-Learner" (:options parsed)))
        ]

    (def exchange exch)
    (def plantifid (keyword ifid))
    (def watchedplant wpid)
    (def tracefilename trfn)

    (let [connection (rmq/connect {:host host :port port})
          channel (lch/open connection)
          _ (le/declare channel ch-name "topic")
          queue (lq/declare channel)
          qname (.getQueue queue)
          _ (lq/bind channel qname ch-name {:routing-key "#"})
          ctag (lc/subscribe channel qname incoming-msgs {:auto-ack true})]

      (def rmq-channel channel)

      (println "RabbitMQ connection Established, plantifid=" plantifid)

      ;; This is a test of the interface.  This code doesn't belong here - comment it out.
      (let [gym-if (gym/make-gym-interface (list "MountainCar-v0") "dmrl" channel exchange)]
        ((:initialize-world gym-if) gym-if)
        ((:reset gym-if) gym-if)
        ((:perform gym-if) gym-if 0)
        ((:render gym-if) gym-if))

      ;; Start the learner!
      (println (format "*** Starting the Q learner with %s (%d episodes) ***%n" gwld neps))

      (let [learner (dmql/initialize-learner cycl alph disc expl nil #_initialQ)] ;+++ provide an initial Q
        (dmql/train learner neps (gym/make-gym-interface gwld "dmrl" channel exchange)))

      ;; If no model was specified, we assume that a command will provide the model to load  later.
      (when last-ctag
        (mq/cancel-subscription (first last-ctag) (second last-ctag)))
      ;; conj for list pushes to the front, so we push channel then ctag.
      ;; So, we get ctag = (first last-ctag), and channel = (second last-ctag)
      (def last-ctag (conj last-ctag channel ctag))

      (if-not (nil? tracefilename)
        (with-open [ostrm (clojure.java.io/writer tracefilename)]
          (while (not exitmainprogram)
            (Thread/sleep 1000))))

      ctag)))

(defn  -main
  "drql"
  {:added "0.1.0"}
  [& args]
  (apply q-learner args)
  nil)

;;; Fin
