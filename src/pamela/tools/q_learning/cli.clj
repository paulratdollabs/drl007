;; Copyright © 2018 Dynamic Object Language Labs Inc.
;; DISTRIBUTION STATEMENT C: U.S. Government agencies and their contractors.
;; Other requests shall be referred to DARPA’s Public Release Center via email at prc@darpa.mil.

(ns pamela.tools.q-learning.cli
  "Q-Learning."
  (:require [clojure.tools.cli :as cli :refer [parse-opts]]
            [clojure.data.json :as json]
            [clojure.data.codec.base64 :as base64]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [me.raynes.fs :as fs]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [mbroker.rabbitmq :as rmq])
  (:gen-class)) ;; required for uberjar


(def cli-options [["-m" "--model pm" "pamela model of system" :default nil]
                  ["-o" "--output file" "output" :default "spamela.txt"]
                  ["-h" "--host rmqhost" "RMQ Host" :default "localhost"]
                  ["-p" "--port rmqport" "RMQ Port" :default 5672 :parse-fn #(Integer/parseInt %)]
                  ["-e" "--exchange name" "RMQ Exchange Name" :default "tpn-updates"]
                  ["-m" "--model ir" "Model IR" :default nil]
                  ["-r" "--root name" "Root pClass" :default "main"]
                  ["-b" "--drqlid id" "DRQL ID" :default "drql1"]
                  ["-w" "--watchedplant id" "WATCHEDPLANT ID" :default nil]
                  ["-t" "--tracefile file" " Trace filename" :default nil]
                  ["-f" "--fromfile val" "Observations from file" :default 0]
                  ["-v" "--verbose level" "Verbose mode" :default "0"]
                  ["-?" "--help"]
                  ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commands

(def rmq-channel nil)
(def exchange nil)
(def dmcpid nil)
(def watchedplant nil)
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
        (and (= rk dmcpid))     (condp = command
                                 :get-field-value (get-field-value m)
                                 ;; :set-field-value (set-field-value m)
                                 (println "Unknown command received: " command m))

        ;; Handle observations from everywhere
        (= rk "observations")    (if (= plantid :plant) ;+++ we need a plant id to be unique to the camera +++
                                   nil ;; +++(recobs/process-visual-observation m)
                                   (doseq [anobs observations]
                                     (let [field (get anobs :field)
                                           value (get anobs :value)]
                                       (cond (and field value)
                                             nil ;; do something here***+++(rtm/set-field-value! plantid field value)
                                             :else
                                             (do
                                               (println "Received observation: " anobs))))))
)
      (check-for-satisfied-activities))))

(def #^{:added "0.1.0"}
  actions
  "Valid drql command line actions"
  {"observe" (var observe-plant)})

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
  "DOLL Reinforcement Q-Leqrner"
  [& args]
  (println args)
  (println cli-options)
  (let [parsed (cli/parse-opts args cli-options)
        verbosity (read-string (get-in parsed [:options :verbose]))
        _ (if (> verbosity 1) (println parsed))
        model (get-in parsed [:options :model])
        desired (get-in parsed [:options :desired])
        outfile (get-in parsed [:options :output])

        ch-name (get-in parsed [:options :exchange])
        _ (if (> verbosity 0) (println [ "ch-name = " ch-name]))
        host (get-in parsed [:options :host])
        _ (if (> verbosity 0) (println ["host = " host]))
        exch (get-in parsed [:options :exchange])
        myid (get-in parsed [:options :drqlid])
        wpid (get-in parsed [:options :watchedplant])
        trfn (get-in parsed [:options :tracefile])
        frfi (get-in parsed [:options :fromfile])
        port (get-in parsed [:options :port])
        _ (if (> verbosity 0) (println ["port = " port]))
        help (get-in parsed [:options :help])
        _ (if (> verbosity 0) (println ["help = " help]))
        root (symbol (get-in parsed [:options :root]))
        _ (println ["root = " root])
        ;; importfilestem (if desired (strip-extn model) nil) ; not clear that we need that.
        ;; model (get-in parsed [:options :model])
        _ (if (> verbosity 0) (println ["model = " model]))
        _ (do
            (def repl false)
            (when help
              (println (usage (:summary parsed)))
              (when-not repl
                (System/exit 0))))

        _ (if (> verbosity 0) (println "DOLL Reinforcement Q-Learner" (:options parsed)))
        ]

    (let [connection (rmq/connect {:host host :port port})
          channel (lch/open connection)
          _ (le/declare channel ch-name "topic")
          queue (lq/declare channel)
          qname (.getQueue queue)
          _ (lq/bind channel qname ch-name {:routing-key "#"})
          ctag (lc/subscribe channel qname incoming-msgs {:auto-ack true})]

      (def rmq-channel channel)
      (def exchange exch)
      (def dmcpid myid)
      (def watchedplant wpid)
      (def tracefilename trfn)
      (println "RabbitMQ connection Established")

      ;; If no model was specified, we assume that a command will provide the model to load  later.
      (when last-ctag
        (mq/cancel-subscription (first last-ctag) (second last-ctag)))
      ;; conj for list pushes to the front, so we push channel then ctag.
      ;; So, we get ctag = (first last-ctag), and channel = (second last-ctag)
      (def last-ctag (conj last-ctag channel ctag))

      (if-not (nil? tracefilename)
        (with-open [ostrm (clojure.java.io/writer tracefilename)]
          (while (not exitmainprogram) (Thread/sleep 1000))))

      ctag)))

(defn  -main
  "drql"
  {:added "0.1.0"}
  [& args]
  nil)
