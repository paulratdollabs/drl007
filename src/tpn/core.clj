;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.core
  (:gen-class)
  #_(:require [tpn.dispatch-app :as d-app])
  #_(:require [tpn.app-state :as state]
            [tpn.tpnrecords :as tpn]
            [tpn.tpn-walk]
            [tpn.dispatch :as dispatch]
            [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [mbroker.rabbitmq]
            [tpn.import]
            [tpn.simulate-local]
            :reload-all
            ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "core args" args)
  #_(d-app/-main args)
  )

;(println "tpn-core loaded")

#_(defn show-tpn [file]
  (let [objs (tpn.import/from-file file)]
    (mbroker.rabbitmq/publish-object objs)
    #_(pp/pprint objs)
    )
  )

;(use 'clojure.tools.trace)
