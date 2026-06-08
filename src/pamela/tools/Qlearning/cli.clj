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
            [pamela.tools.Qlearning.Qtables :as qtbl]
            [pamela.tools.Qlearning.DMQL :as dmql]
            [pamela.tools.Qlearning.DPLinterface :as DPL]
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
                  ["-l" "--loadqtable edn" "Start from a prior Q-table" :default nil]
                  ["-i" "--if id" "IF ID" :default "gym"] ; The interface ID (plant - robot or simulator)
                  ["-n" "--episodes n" "Number of Episodes" :default 25000 :parse-fn #(Integer/parseInt %)]
                  ["-S" "--statistics n" "Statistics saved after n Episodes" :default 100 :parse-fn #(Integer/parseInt %)]
                  ["-R" "--render n" "Render after n Episodes" :default 100 :parse-fn #(Integer/parseInt %)]
                  ["-B" "--backup n" "Backup frequency in episodes" :default 100 :parse-fn #(Integer/parseInt %)]
                  ["-a" "--alpha f" "Learning Rate" :default 0.1 :parse-fn #(Float/parseFloat %)]
                  ["-d" "--discount f" "Discount Rate" :default 0.95 :parse-fn #(Float/parseFloat %)]
                  ["-x" "--explore f" "Portion of episodes to explore" :default 0.5 :parse-fn #(Float/parseFloat %)]
                  ["-c" "--cycletime ms" "Cycle time in milliseconds" :default 0 :parse-fn #(Integer/parseInt %)]
                  ["-q" "--min-q n" "Minimum Q value" :default -2.0  :parse-fn #(Float/parseFloat %)]
                  ["-u" "--max-q n" "Maximum Q value" :default 0.0   :parse-fn #(Float/parseFloat %)]
                  ["-s" "--statedivision n" "Discretization of each state dimension" :default 20  :parse-fn #(Integer/parseInt %)]

                  ["-g" "--gymworld gw" "Name of the Gym World" :default "MountainCar-v0"]
                  ["-z" "--epsilon fr" "Starting value for epsilon exploration 1 >= fr >= 0" :default 1.0 :parse-fn #(Float/parseFloat %)]
                  ["-=" "--mode n" "Select a special mode [0=normal, 1=Monte-Carlo, others to come]" :default 1  :parse-fn #(Integer/parseInt %)]

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

;;; Commands

(def rmq-channel nil)
(def exchange nil)
(def dmcpid nil)
(def watchedplant nil)
(def plantifid nil)
(def tracefilename nil)
(def exitmainprogram false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn action-function
  [args]
  (println args))

(defn demo-function
  [args]
  nil)

(def #^{:added "0.1.0"}
  actions
  "Valid dmrl command line actions"
  {"qlearn" (var action-function)
   "test-connection" (var demo-function)})

(defn usage
  "Print dmrl command line help."
  {:added "0.1.0"}
  [options-summary]
  (->> (for [a (sort (keys actions))]
         (str "  " a "\t" (:doc (meta (get actions a)))))
    (concat [""
             "DOLL Monte-Carlo Reinforcement Learner"
             ""
             "Usage: dmrl [options] action"
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
        {:keys [options arguments error summary]} parsed
        {:keys [help version verbose test-connection qlearn] } options
        cmd (first arguments)
        verbosity (read-string (get-in parsed [:options :verbose]))
        _ (DPL/set-verbosity verbosity)
        _ (if (> verbosity 2) (println parsed))
        model (get-in parsed [:options :model])
        outfile (get-in parsed [:options :output])

        ch-name (get-in parsed [:options :exchange])
        host (get-in parsed [:options :host])
        _ (if (> verbosity 0) (println ["host = " host]))
        exch (get-in parsed [:options :exchange])
        ifid (get-in parsed [:options :if])       ; Interface ID
        neps (get-in parsed [:options :episodes]) ; Number of episodes
        rend (get-in parsed [:options :render])   ; Number of episodes before rendering
        stat (get-in parsed [:options :statistics]) ; Number of episodes before statistics
        back (get-in parsed [:options :backup])   ; Number of episodes before saving Q-table
        loaq (get-in parsed [:options :loadqtable]) ; Restart learning from a prior Q table
        alph (get-in parsed [:options :alpha])    ; Learning rate
        disc (get-in parsed [:options :discount]) ; Discount rate
        epsi (get-in parsed [:options :epsilon])  ; Epsilon starting value
        minq (get-in parsed [:options :min-q])    ; Minimum initial Q value
        maxq (get-in parsed [:options :max-q])    ; Maximum initial Q value
        expl (get-in parsed [:options :explore])  ; fraction of episodes for which exploration takes place
        ssdi (get-in parsed [:options :statedivision]) ; State space discretization for each dimension
        cycl (get-in parsed [:options :cycletime]); Cycletime in milliseconds
        gwld (get-in parsed [:options :gymworld]) ; Name of the GYM world to instantiate
        mode (get-in parsed [:options :mode])     ; Mode 0=normal, 1=Monte-Carlo, etc.
        wpid (get-in parsed [:options :watchedplant])
        trfn (get-in parsed [:options :tracefile])
        frfi (get-in parsed [:options :fromfile])
        port (get-in parsed [:options :port])
        _ (if (> verbosity 0) (println ["port = " port]))
        help (get-in parsed [:options :help])
        _ (if (> verbosity 0) (println ["help = " help]))
        root (symbol (get-in parsed [:options :root]))
        _ (if (> verbosity 0) (println "DOLL Reinforcement Q-Learner" (:options parsed)))
        ]

    (if help
      (do
        (println (usage summary))
        (System/exit 0)))

    ;; RabbitQG
    (def exchange exch)
    (def plantifid (keyword ifid))
    (def watchedplant wpid)
    (def tracefilename trfn)

    (def rmq-channel (DPL/rabbitMQ-connect host port ch-name plantifid))

    (println "RabbitMQ connection Established, plantifid=" plantifid)

    (cond (>= (count arguments) 1)
          (case (keyword (first arguments))
            :test-connection
            (let [gym-if (gym/make-gym-interface (list "MountainCar-v0") "dmrl" rmq-channel exchange :gym)]
              ((:initialize-world gym-if) gym-if)
              ((:reset gym-if) gym-if)
              ((:perform gym-if) gym-if 0 0)
              ((:render gym-if) gym-if))

            :qlearn
            ;; Start the learner!
            (let [_ (println (format "*** Starting the Q learner with %s (%d episodes, mode=%d, epsilon=%f explore=%f) ***%n"
                                     gwld neps mode epsi expl))
                  gym-if  (gym/make-gym-interface (list gwld) "dmrl" rmq-channel exchange :gym)]
              ;; Monitors for debugging, put -v 1 in the command line to see them.
              (DPL/monitor-field :gym  :reward)
              (DPL/monitor-field :gym  :done)
              (DPL/print-monitors)

              ((:initialize-world gym-if) gym-if) ; Startup the simulator
              (Thread/sleep 100) ; Wait one second to allow simulator to start up and publish data
              ;; (gym/print-field-values)
              (let [numobs  (DPL/get-field-value :gym :numobs)
                    numacts (DPL/get-field-value :gym :numacts)]
                #_(println (format "*** Observation Dimension=%d Actions=%d" numobs numacts))
                (let [initial-q-table
                      (if loaq ; +++ maybe check (.exists (clojure.java.io/as-file loaq) ?
                        (let [prior-q-table (qtbl/read-q-table loaq)
                              {episodes :episodes} prior-q-table]
                          (println "Restarting from a prior q-table: " loaq "episode=" episodes)
                          ;; (pprint prior-q-table)
                          prior-q-table)
                        (qtbl/make-java-fixed-sized-q-table-constant
                         numobs ssdi numacts (/ (+ minq maxq) 2.0)
                         (gym/get-obs-low numobs) (gym/win-size numobs ssdi) 0)
                        #_(qtbl/make-java-fixed-sized-q-table-uniform-random
                         #_make-fixed-sized-q-table-uniform-random
                         numobs ssdi numacts minq maxq
                         (gym/get-obs-low numobs) (gym/win-size numobs ssdi) 0))
                      learner (dmql/initialize-learner cycl 200 mode rend stat back alph disc
                                                       epsi neps expl ssdi numobs numacts
                                                       initial-q-table gym-if)
                      #_(pprint initial-q-table)]
                  (dmql/train learner)
                  (println "Training completed.")
                  (System/exit 0))))

            (println "Unknown command: " (first arguments) "try: test-connection or qlearn"))

          :else
          (do
            (println "No command specified, try test-connection or qlearn")
            (System/exit 0)))

    (if-not (nil? tracefilename)
      (with-open [ostrm (clojure.java.io/writer tracefilename)]
        (while (not exitmainprogram)
          (Thread/sleep 1000))))))

(defn  -main
  "dmrl"
  {:added "0.1.0"}
  [& args]
  (apply q-learner args)
  nil)

;;; Fin
