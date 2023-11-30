;; Copyright © 2020 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning Statistics and Analytics

(ns pamela.tools.Qlearning.analytics
  "Analytics"
  (:require [clojure.string :as string]
            [clojure.repl :refer [pst]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.tools.reader :as reader]
            [clojure.java.io :as io]
            [clojure.core :as cc]
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [pamela.tools.Qlearning.fromjson :as fromjson]
            [clojure.math.numeric-tower :as math]
            [environ.core :refer [env]]
            [pamela.tools.Qlearning.Qtables :as qtbl])
  (:gen-class))

;(in-ns 'pamela.tools.Qlearning.analytics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statistics

(def json-or-edn :json)

(defn save-statistics
  "Save statistics to a named file."
  [stats episode rid name]
  (let ; +++ [fn (str rid "-" name (if (= json-or-edn :edn) "-learning-statistics.edn" "-learning-statistics.json"))]
      ;; +++ for now let's do both!
      [fn  (str rid "-" name "-learning-statistics.edn")
       fnj (str rid "-" name "-learning-statistics.json")]
    ;; (if  (= json-or-edn :edn)
      (with-open [w (io/writer fn)]
        (binding [*out* w]
          (println ";;; Readable EDN Statistics")
          (pr stats)))
      (fromjson/to-file stats fnj)
      ;  )
    fn))

(def adirpath "/Volumes/paul/DataForNDIST23/")
(def data-example
  ["1701040169369-DMQL-learning-statistics"
   "1701041331030-DMQL-learning-statistics"
   "1701042607046-DMQL-learning-statistics"
   "1701044066481-DMQL-learning-statistics"
   "1701045680027-DMQL-learning-statistics"
   "1701047457959-DMQL-learning-statistics"
   "1701049348483-DMQL-learning-statistics"
   "1701051492815-DMQL-learning-statistics"
   "1701053822395-DMQL-learning-statistics"
   "1701056472200-DMQL-learning-statistics"
   "1701059277798-DMQL-learning-statistics"
   "1701062164895-DMQL-learning-statistics"
   "1701065308166-DMQL-learning-statistics"
   "1701068561086-DMQL-learning-statistics"
   "1701072248086-DMQL-learning-statistics"
   "1701076080202-DMQL-learning-statistics"
   "1701080048926-DMQL-learning-statistics"
   "1701089539439-DMQL-learning-statistics"
   "1701113425387-DMQL-learning-statistics"])

(defn convert-edn-statistics-to-json
  [dir-path list-of-filenames]
  (doseq [afile list-of-filenames]
    (let [in-fn (str dir-path afile ".edn")
          out-fn (str dir-path afile ".json")
          in-data (read-string (slurp in-fn))]
      (println "Processing " in-fn " -> " out-fn)
      (fromjson/to-file in-data out-fn))))

;;; (convert-edn-statistics-to-json adirpath data-example)

;;; CVS state and action data

(def csvfile nil)

(defn open-csv-file
  [runid name episode]
  (let [fn (str (str runid) "-" name "-e-" "ds+a-" (str episode) ".csv")]
    (def csvfile (io/writer fn :append false))))

(defn close-csv-file
  [runid name episode]
  (.close csvfile))

(def once-only true)

(defn comma-separated-values
  [vec-of-vals]
  (loop [vov vec-of-vals
         res ""]
    (if (empty? vov)
      res
      (recur (rest vov) (str res (first vov) ", ")))))

;;; (comma-separated-values ["foo" "bar" "baz"])

(defn write-csv-data [dstate action]
  (let [numdims (count dstate)]
    (when once-only
      ;; (println "dstate = " (str dstate) " numdims = " (str numdims))
      (def once-only false))
    (.write csvfile (str (comma-separated-values dstate)  (str action) "\n"))))
