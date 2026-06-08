;; Copyright © 2018 Dynamic Object Language Labs Inc.
;; DISTRIBUTION STATEMENT C: U.S. Government agencies and their contractors.
;; Other requests shall be referred to DARPA’s Public Release Center via email at prc@darpa.mil.

(ns dcrypps.diagnosis-based-rg.casediagimpl
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.repl :refer [pst]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [clojure.data.xml :as xml]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.java.shell :only [sh]]
            [clojure.set :as set]
            [pamela.unparser :as pup]
            [pamela.tools.belief-state-planner.ir-extraction :as irx]
            [pamela.tools.belief-state-planner.runtimemodel :as rt :refer :all]
            [pamela.tools.belief-state-planner.montecarloplanner :as mcp]
            [dcrypps.diagnosis-based-rg.cachedpamela :as cpam]))

#_(in-ns 'dcrypps.diagnosis-based-rg.casediagimpl)

(def ^:dynamic attack-plan nil)
(def ^:dynamic pamela-model-json nil)
;(def ^:dynamic outfile nil)
(def ^:dynamic desired nil)
;(def ^:dynamic verbosity 0)
(def model-root "root")

(defn NYI "Not Yet Implemented" [name]
  (println "NYI called from: " name)
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros and dynamic variables

(def ^:dynamic rgImportFileStem nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diagnose requirements

;;(defn mapphase [n] (map #(nth (cvPhases %) 1) (range 0 (def informal-new-requirements-1-low-probability
;;   {
;;    ["(requirement
;;        :doc "Authenticate communication between GS and missile."
;;        (authenticated (communication GS missile)))
;;      (requirement
;;        :doc "Ensure communication between GS and missile does not exceed buffer."
;;        (protected-against :buffer-overflow-attack (communication GS missile)))"
;;    ]
;;   )
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Objects in the Pamela model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Actuator
;;; AeroGuidance
;;; AerodynamicsAndEquationsOfMotion
;;; AirframeAutopilot
;;; Atmosphere
;;; Autopilot
;;; Controller
;;; ControllerBoard
;;; FinActuator
;;; FinControl
;;; GPS
;;; GroundStation
;;; Guidance
;;; INS
;;; KalmanFilter
;;; LocalNetwork
;;; MCCellularNetwork
;;; MissileGuidanceBus
;;; MissileGuidanceDiagnosticsServer
;;; MissileGuidanceProgram
;;; MissileGuidanceUnit
;;; NavigationalSensor
;;; NavigationalSensorDependentOnRadio
;;; Program
;;; RangeAndClosingVelocityEstimates
;;; SeekerTracker
;;; SeekerTrackerComponent
;;; Sensor
;;; Sensors
;;; TargetAcquisition
;;; ThreeAxisAccelerometer
;;; TrackerEstimator

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object referred to by the attack plan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; internal to attack plan
;;; accuracy
;;; missileGuidanceProcess
;;; finControlCommand
;;; typicalAttackerMachine
;;; dataIntegrity
;;; gpsPosition
;;; execution
;;; controllerDiagnosticProcess
;;; insGyroPosition
;;; seekerTrackerVector
;;; typicalAttackerMachine
;;; missileGuidanceProcess
;;; insGyro
;;; Controller

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Objects used in requirements and desirable properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GS = GroundStation
;;; missile = AirframeAutopilot
;;; airframecontroller = AirframeAutopilot
;;; guidance = MissileGuidanceUnit
;;; tracker-seeker = SeekerTracker
;;; target = TargetAcquisition

#_(def informal-new-requirements-1-high-probability
  {:requirements
    ["(requirement
       :doc \"Authenticate communication between GroundStation and AirframeAutopilot.\"
       (authenticated (communication GroundStation AirframeAutopilot)))
     (requirement
       :doc \"Ensure communication between GroundStation and AirframeAutopilot does not exceed buffer.\"
       (protected-against :buffer-overflow-attack (communication GroundStation AirframeAutopilot)))
     (requirement
       :doc \"Authenticate communication between all internal units: AirframeAutopilot MissileGuidanceUnit SeekerTracker.\"
       (and (authenticated (communication airframecontroller MissileGuidanceUnit))
            (authenticated (communication controller MissileGuidanceUnit SeekerTracker))
            (authenticated (communication controller MissileGuidanceUnit tracker-seeker))))"
     ]
   }
)

(def desired-properties-placeholder
  ["(desirable
       :doc \"Abort if-and-only-if abort command is received from ground station at least 100ms before impact.\"
       (== (and (abort-mission) (and (time-to-impact) 100ms))(received :from GroundStation :command :abort)))"
     "(desirable
       :doc \"Tracker maintains a trajectory within 10 degrees of trajectory towards the target\"
       (maintain (< (abs (- (current-trajectory) (target-trajectory))) (10 :degrees))))"
     "(desirable
       :doc \"Missile converges on target\"
       (forall (and (> t 0) (> epsilon 0)) (< (distance missile target :at (+ t epsilon)) (distance missile target :at t)))"
    ]
)

#_(def requirements-and-pcc-placeholder-1
  {:requirements
   ["(requirement
        (communications-requiring-pki-encryption-on-network AutoPilotUnit.cellnet)
        :desirable-properties [9,10,14,15,17])"
    "(requirement
        (communications-requiring-pki-encryption-on-network AutoPilotUnit.localnet)
        :desirable-properties [1,2,4,5,7,8,9,10,14,17])"
    "(requirement
        (filter-required-for-excess-deviation-detection
        [AutoPilotUnit.vor AutoPilotUnit.gps AutoPilotUnit.ins])
        :desirable-properties [11,12,13])"
    "(requirement
        (add-backup-network-link-using-pki-encryption
          :substituting-for AutoPilotUnit.groundstation
          :connecting-to AutoPilotUnit.controller
          :allowed-methods [AutoPilotProgram.set-waypoint-list])
          :desirable-properties [9,10,14,15,17])"
    "(requirement
        :STL-Formula
          (eventually
            :op \"<=\"
            :timed-variable-value \"MissileGuidanceUnit.guidancemodel.range\"
            :constant 10)
        :Description
          \"fuze-distance will eventually be less than or equal to 10\")"]
     :pcc
     ["Placeholder for PCC demonstrating Target Confidence Level < 95%"]})

#_(def requirements-and-pcc-placeholder-2
  {:requirements
   ["(requirement
       (communications-requiring-pki-encryption-on-network MissileGuidanceUnit.cellnet)
       :desirable-properties [9,10,14,15,17])"
    "(requirement
       (communications-requiring-pki-encryption-on-network MissileGuidanceUnit.localnet)
       :desirable-properties [1,2,4,5,7,8,9,10,14,17])"
    "(requirement
       (filter-required-for-excess-deviation-detection
       [MissileGuidanceUnit.gps MissileGuidanceUnit.ins])
       :desirable-properties [11,12,13])"
    "(requirement
       (add-backup-network-link-using-pki-encryption
       :substituting-for MissileGuidanceUnit.groundstation
       :connecting-to MissileGuidanceUnit.controller
       :allowed-methods [ControllerBoard.initialize ControllerBoard.navigation-test])
       :desirable-properties [9,10,14,15,17])"
    "(requirement
       (add-module-to-specifed-input-functional-design new-nav-sensor
       :pclass NavigationalSensorNotDependentOnRadio
       :where (independent MissileGuidanceUnit.ins new-nav-sensor))
       :desirable-properties [11,12,13])"]
   :pcc
    ["Placeholder for PCC demonstrating Target Confidence Level >= 95%"]})


;;; MOVED to dcrypps.pamela-utilities.cli
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiling pamela on the fly

#_(defn compile-pamela-string-to-ir-json
  [pamela-string]
  (let [tofile (apply str (concat (str (gensym "pamela-source")) ".pamela"))
        tifile (apply str (concat (str (gensym "pamela-json")) ".json"))
        command (apply str (concat "pamela -i " tofile " -o " tifile " --json-ir build"))]
    ;;(println "Writing Pamela source to: " tofile)
    ;; Write the pamela text to a file
    (with-open [ostrm (java.io.OutputStreamWriter.(java.io.FileOutputStream. tofile) "UTF-8")]
      (.write ostrm pamela-string)
      (.write ostrm "\n"))
    ;; Compile the pamela file
    ;;(println "Compiling Pamela source: " command)
    (clojure.java.shell/sh "pamela" "-i" tofile "-o" tifile "--json-ir" "build")
    ;; Read the compiled json
    ;;(println "Reading the compiled pamela as json from: " tifile)
    (json/read-str (slurp tifile))))

#_(def pamela-test-data "(defpclass Sensor [] :modes [:on :off :fail])")

;;; (compile-pamela-string-to-ir-json pamela-test-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here are the auxilliary functions

(defn make-field-ref
  [obj fld]
  (with-out-str (printf "%s.%s" obj fld)))

(defn find-class-methods [myclass all-methods]
  (let [matches (remove nil? (map (fn [pcls] (if (= myclass
                                                    (get pcls :pclass))
                                               pcls
                                               nil))
                                  all-methods))]
    ;;(println "fcm: myclass=" (str myclass)  "matches=" matches)
    (if (>= (count matches) 1)
      (do
        (if (> (count matches) 1) (println "Strange, more than one match: " matches))
        ;; (first matches) is our class
        (let [cls (first matches)
              methods (get cls :methods)]
          ;;(println "Found-methods=" (map first methods))
          (map first methods))))))

(defn get-methods [object fld component-type-map all-methods]
  (let [mytypemap (get component-type-map object)
        relevant-class (get mytypemap fld)]
    ;;(println "get-methods: mytypemap=" mytypemap "relevant-class=" relevant-class)
    (find-class-methods relevant-class all-methods)))

(defn get-objects-of-type ;+++ transfor cdict earlier to avoid recomputing it every time +++
  [key cdict]
  (map (fn [x] (symbol nil x)) (cdict key)))

(defn external-connections
  "Returns a list of external connections in all classes."
  [componenttypemap alltypes conmap cdict]
  (let [types (get-objects-of-type :ExternalConnections cdict)]
    (apply concat
           (map (fn [external]
                  (irx/components-of-type componenttypemap alltypes conmap external))
                types)))) ; was 'CellularNetwork

(defn internal-connections
  "Returns a list of internal connections in all classes."
  [componenttypemap alltypes conmap cdict]
  (let [types (get-objects-of-type :InternalConnections cdict)]
    (apply concat
           (map (fn [internal]
                  (irx/components-of-type componenttypemap alltypes conmap internal))
                types)))) ; was 'LocalNetwork

(defn all-navigational-sensors
  "Returns a list of navigational sensors associated with 'object'."
  [componenttypemap alltypes conmap cdict]
  (let [types (seq (set (concat (get-objects-of-type :NavigationalSensor cdict)
                                (get-objects-of-type :NavigationalSensorNotDependentOnRadio cdict))))]
    #_(println "all-navigational-sensors: types =" types)
    (let [ans (apply concat
                     (map (fn [navsensor]
                            (irx/components-of-type componenttypemap alltypes conmap navsensor))
                          types))]
      #_(println "All nav sensors = " ans)
      ans))) ; was 'NavigationalSensor

(defn non-radio-navigational-sensors
  "Returns a list of non-radio navigational sensors associated with 'object'."
  [componenttypemap alltypes conmap cdict]
  (let [types (get-objects-of-type :NavigationalSensorNotDependentOnRadio cdict)]
    (apply concat
           (map (fn [nrnavsensor]
                  (irx/components-of-type componenttypemap alltypes conmap nrnavsensor))
                types)))) ; was 'NavigationalSensorNotDependentOnRadio

(defn requirement
  "Package up a requirement as expected by the consumer."
  [req & {:keys [desirable-properties capecs] :or {desirable-properties [] capecs []}}]
  ;;(list 'requirement req :desirable-properties desirable-properties))
  {:requirement req :desirable-properties desirable-properties :capecs capecs})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here are the requirement generators
;;;
;;; Each emit produces a single requirement

(defn emit-communication-requirement
  [dps fromtyp totyp]
  (requirement
   (list 'communications-should-guard-against-lateral-attacks-between (str fromtyp) (str totyp))
   :desirable-properties dps
   :capecs [{
                :capec-id 94
                :capec-short-description "Man in the Middle Attack"
                :capec-long-description "This type of attack targets the communication between two components (typically client and server). The attacker places himself in the communication channel between the two components. Whenever one component attempts to communicate with the other (data flow, authentication challenges, etc.), the data first goes to the attacker, who has the opportunity to observe or alter it, and it is then passed on to the other component as if it was never observed. This interposition is transparent leaving the two compromised components unaware of the potential corruption or leakage of their communications. The potential for Man-in-the-Middle attacks yields an implicit lack of trust in communication or identify between two components. MITM attacks differ from sniffing attacks since they often modify the communications prior to delivering it to the intended recipient. These attacks also differ from interception attacks since they may forward the sender's original unmodified data, after copying it, instead of keeping it for themselves."
                :capec-category-id 512
                :capec-template-parameters {
                    :component-a "FinControl_Impl"
                    :component-b "MissileGuidanceBus_Impl"
                }
            } {
                :capec-id 151
                :capec-short-description "Identity Spoofing"
                :capec-long-description "Identity Spoofing refers to the action of assuming (i.e., taking on) the identity of some other entity (human or non-human) and then using that identity to accomplish a goal. An adversary may craft messages that appear to come from a different principle or use stolen / spoofed authentication credentials. Alternatively, an adversary may intercept a message from a legitimate sender and attempt to make it look like the message comes from them without changing its content. The latter form of this attack can be used to hijack credentials from legitimate users. Identity Spoofing attacks need not be limited to transmitted messages - any resource that is associated with an identity (for example, a file with a signature) can be the target of an attack where the adversary attempts to change the apparent identity. This attack differs from Content Spoofing attacks where the adversary does not wish to change the apparent identity of the message but instead wishes to change what the message says. In an Identity Spoofing attack, the adversary is attempting to change the identity of the content."
                :capec-category-id 512
                :capec-template-parameters {
                    :component-identity-spoofed "FinControl_Impl"
                    :component-under-attack "MissileGuidanceBus_Impl"
                }
            } {
                :capec-id 148
                :capec-short-description "Content Spoofing"
                :capec-long-description "An adversary modifies content to make it contain something other than what the original content producer intended while keeping the apparent source of the content unchanged. The term content spoofing is most often used to describe modification of web pages hosted by a target to display the adversary's content instead of the owner's content. However, any content can be spoofed, including the content of email messages, file transfers, or the content of other network communication protocols. Content can be modified at the source (e.g. modifying the source file for a web page) or in transit (e.g. intercepting and modifying a message between the sender and recipient). Usually, the adversary will attempt to hide the fact that the content has been modified, but in some cases, such as with web site defacement, this is not necessary. Content Spoofing can lead to malware exposure, financial fraud (if the content governs financial transactions), privacy violations, and other unwanted outcomes."
                :capec-category-id 512
                :capec-template-parameters {
                    :content-source "FinControl_Impl"
                    :content-dest "MissileGuidanceBus_Impl"
                }
            } {
                :capec-id 216
                :capec-short-description "Communication Channel Manipulation"
                :capec-long-description "An adversary manipulates a setting or parameter on communications channel in order to compromise its security. This can result in information exposure, insertion/removal of information from the communications stream, and/or potentially system compromise."
                :capec-category-id 512
                :capec-template-parameters {
                    :channel-endpoint-a "FinControl_Impl"
                    :channel-endpoint-b "MissileGuidanceBus_Impl"
                }
            } {
                :capec-id 594
                :capec-short-description "Traffic Injection"
                :capec-long-description "An adversary injects traffic into the target's network connection. The adversary is therefore able to degrade or disrupt the connection, and potentially modify the content. This is not a flooding attack, as the adversary is not focusing on exhausting resources. Instead, the adversary is crafting a specific input to affect the system in a particular way."
                :capec-category-id 512
                :capec-template-parameters {
                    :channel-endpoint-a "FinControl_Impl"
                    :channel-endpoint-b "MissileGuidanceBus_Impl"
                }
            }
        ]))

(defn emit-pki-communication-requirement
  "The requirement that a connection use PKI encryption."
  [dps object field]
  (requirement
   (list 'communications-requiring-pki-encryption-on-network (make-field-ref object field))
   :desirable-properties dps
   :capecs [{
                :capec-id 94
                :capec-short-description "Man in the Middle Attack"
                :capec-long-description "This type of attack targets the communication between two components (typically client and server). The attacker places himself in the communication channel between the two components. Whenever one component attempts to communicate with the other (data flow, authentication challenges, etc.), the data first goes to the attacker, who has the opportunity to observe or alter it, and it is then passed on to the other component as if it was never observed. This interposition is transparent leaving the two compromised components unaware of the potential corruption or leakage of their communications. The potential for Man-in-the-Middle attacks yields an implicit lack of trust in communication or identify between two components. MITM attacks differ from sniffing attacks since they often modify the communications prior to delivering it to the intended recipient. These attacks also differ from interception attacks since they may forward the sender's original unmodified data, after copying it, instead of keeping it for themselves."
                :capec-category-id 512
                :capec-template-parameters {
                    :component-a "ControllerBoard_Impl.TCPA"
                    :component-b "all_connected_components"
                }
            } {
                :capec-id 151
                :capec-short-description "Identity Spoofing"
                :capec-long-description "Identity Spoofing refers to the action of assuming (i.e., taking on) the identity of some other entity (human or non-human) and then using that identity to accomplish a goal. An adversary may craft messages that appear to come from a different principle or use stolen / spoofed authentication credentials. Alternatively, an adversary may intercept a message from a legitimate sender and attempt to make it look like the message comes from them without changing its content. The latter form of this attack can be used to hijack credentials from legitimate users. Identity Spoofing attacks need not be limited to transmitted messages - any resource that is associated with an identity (for example, a file with a signature) can be the target of an attack where the adversary attempts to change the apparent identity. This attack differs from Content Spoofing attacks where the adversary does not wish to change the apparent identity of the message but instead wishes to change what the message says. In an Identity Spoofing attack, the adversary is attempting to change the identity of the content."
                :capec-category-id 512
                :capec-template-parameters {
                    :component-identity-spoofed "ControllerBoard_Impl.TCPA"
                    :component-under-attack "all_connected_components"
                }
            } {
                :capec-id 148
                :capec-short-description "Content Spoofing"
                :capec-long-description "An adversary modifies content to make it contain something other than what the original content producer intended while keeping the apparent source of the content unchanged. The term content spoofing is most often used to describe modification of web pages hosted by a target to display the adversary's content instead of the owner's content. However, any content can be spoofed, including the content of email messages, file transfers, or the content of other network communication protocols. Content can be modified at the source (e.g. modifying the source file for a web page) or in transit (e.g. intercepting and modifying a message between the sender and recipient). Usually, the adversary will attempt to hide the fact that the content has been modified, but in some cases, such as with web site defacement, this is not necessary. Content Spoofing can lead to malware exposure, financial fraud (if the content governs financial transactions), privacy violations, and other unwanted outcomes."
                :capec-category-id 512
                :capec-template-parameters {
                    :content-source "ControllerBoard_Impl.TCPA"
                    :content-dest "all_connected_components"
                }
            } {
                :capec-id 216
                :capec-short-description "Communication Channel Manipulation"
                :capec-long-description "An adversary manipulates a setting or parameter on communications channel in order to compromise its security. This can result in information exposure, insertion/removal of information from the communications stream, and/or potentially system compromise."
                :capec-category-id 512
                :capec-template-parameters {
                    :channel-endpoint-a "ControllerBoard_Impl.TCPA"
                    :channel-endpoint-b "all_connected_components"
                }
            } {
                :capec-id 594
                :capec-short-description "Traffic Injection"
                :capec-long-description "An adversary injects traffic into the target's network connection. The adversary is therefore able to degrade or disrupt the connection, and potentially modify the content. This is not a flooding attack, as the adversary is not focusing on exhausting resources. Instead, the adversary is crafting a specific input to affect the system in a particular way."
                :capec-category-id 512
                :capec-template-parameters {
                    :channel-endpoint-a "ControllerBoard_Impl.TCPA"
                    :channel-endpoint-b "all_connected_components"
                }
            }
        ]))

(defn emit-redundant-sensor-deviation-monitoring-requirement
  "The requirement that if redundant sensors exist that their deviation should be monitored."
  [dps navigational-sensors]
  (requirement (concat (list 'filter-required-for-excess-deviation-detection)
                             (seq (map (fn [[object field users]]
                                         (make-field-ref object field))
                                       navigational-sensors)))
               :desirable-properties dps
               :capecs [{
                :capec-id 624
                :capec-short-description "Fault Injection"
                :capec-long-description "The adversary uses disruptive signals or events (e.g. electromagnetic pulses, laser pulses, clock glitches, etc.) to cause faulty behavior in electronic devices. When performed in a controlled manner on devices performing cryptographic operations, this faulty behavior can be exploited to derive secret key information."
                :capec-category-id 512
                }]))


(defn emit-backup-network-requirement
  "The requirement that if communications with outside, redundant PKI connection for specific methods be provided."
  [dps from to methods]
  (requirement
   (list 'add-backup-network-link-using-pki-encryption
         :substituting-for from
         :connecting-to to
         :allowed-methods methods)
   :desirable-properties dps
   :capecs [{:capec-id 125
             :capec-short-description "Flooding"
             :capec-long-description "An adversary consumes the resources of a target by rapidly engaging in a large number of interactions with the target. This type of attack generally exposes a weakness in rate limiting or flow. When successful this attack prevents legitimate users from accessing the service and can cause the target to crash. This attack differs from resource depletion through leaks or allocations in that the latter attacks do not rely on the volume of requests made to the target but instead focus on manipulation of the target's operations. The key factor in a flooding attack is the number of requests the adversary can make in a given period of time. The greater this number, the more likely an attack is to succeed against a given target."
             :capec-category-id 512
             }
            {:capec-id 148
             :capec-short-description "Content Spoofing"
             :capec-long-description "An adversary modifies content to make it contain something other than what the original content producer intended while keeping the apparent source of the content unchanged. The term content spoofing is most often used to describe modification of web pages hosted by a target to display the adversary's content instead of the owner's content. However, any content can be spoofed, including the content of email messages, file transfers, or the content of other network communication protocols. Content can be modified at the source (e.g. modifying the source file for a web page) or in transit (e.g. intercepting and modifying a message between the sender and recipient). Usually, the adversary will attempt to hide the fact that the content has been modified, but in some cases, such as with web site defacement, this is not necessary. Content Spoofing can lead to malware exposure, financial fraud (if the content governs financial transactions), privacy violations, and other unwanted outcomes."
             :capec-category-id 512
             }
            {:capec-id 216
             :capec-short-description "Communication Channel Manipulation"
             :capec-long-description "An adversary manipulates a setting or parameter on communications channel in order to compromise its security. This can result in information exposure, insertion/removal of information from the communications stream, and/or potentially system compromise."
             :capec-category-id 512
             }
            {:capec-id 272
             :capec-short-description "Protocol Manipulation"
             :capec-long-description "An adversary subverts a communications protocol to perform an attack. This type of attack can allow an adversary to impersonate others, discover sensitive information, control the outcome of a session, or perform other attacks. This type of attack targets invalid assumptions that may be inherent in implementers of the protocol, incorrect implementations of the protocol, or vulnerabilities in the protocol itself."
             :capec-category-id 512
             }
            {:capec-id 594
             :capec-short-description "Traffic Injection"
             :capec-long-description "An adversary injects traffic into the target's network connection. The adversary is therefore able to degrade or disrupt the connection, and potentially modify the content. This is not a flooding attack, as the adversary is not focusing on exhausting resources. Instead, the adversary is crafting a specific input to affect the system in a particular way."
             :capec-category-id 512
             }
            {:capec-id 624
             :capec-short-description "Fault Injection"
             :capec-long-description "The adversary uses disruptive signals or events (e.g. electromagnetic pulses, laser pulses, clock glitches, etc.) to cause faulty behavior in electronic devices. When performed in a controlled manner on devices performing cryptographic operations, this faulty behavior can be exploited to derive secret key information."
             :capec-category-id 512
             }]))

(defn emit-redundant-non-radio-sensors-requirement
  "emit requirement that if > 95% probability, redundant navigational sensors not requiring radio be supplied."
  [dps ns]
  (let [new-sensor-name (str (gensym 'new-nav-sensor))
        [object field connections] ns
        req (requirement
             (list 'add-module-to-specifed-input-functional-design new-sensor-name
                   :pclass 'NavigationalSensorNotDependentOnRadio
                   :where (list 'independent (make-field-ref object field) new-sensor-name))
             :desirable-properties dps
             :capecs [{:capec-id 125
                       :capec-short-description "Flooding"
                       :capec-long-description "An adversary consumes the resources of a target by rapidly engaging in a large number of interactions with the target. This type of attack generally exposes a weakness in rate limiting or flow. When successful this attack prevents legitimate users from accessing the service and can cause the target to crash. This attack differs from resource depletion through leaks or allocations in that the latter attacks do not rely on the volume of requests made to the target but instead focus on manipulation of the target's operations. The key factor in a flooding attack is the number of requests the adversary can make in a given period of time. The greater this number, the more likely an attack is to succeed against a given target."
                       :capec-category-id 512
                       }
                      {:capec-id 148
                       :capec-short-description "Content Spoofing"
                       :capec-long-description "An adversary modifies content to make it contain something other than what the original content producer intended while keeping the apparent source of the content unchanged. The term content spoofing is most often used to describe modification of web pages hosted by a target to display the adversary's content instead of the owner's content. However, any content can be spoofed, including the content of email messages, file transfers, or the content of other network communication protocols. Content can be modified at the source (e.g. modifying the source file for a web page) or in transit (e.g. intercepting and modifying a message between the sender and recipient). Usually, the adversary will attempt to hide the fact that the content has been modified, but in some cases, such as with web site defacement, this is not necessary. Content Spoofing can lead to malware exposure, financial fraud (if the content governs financial transactions), privacy violations, and other unwanted outcomes."
                       :capec-category-id 512
                       }
                      {:capec-id 272
                       :capec-short-description "Protocol Manipulation"
                       :capec-long-description "An adversary subverts a communications protocol to perform an attack. This type of attack can allow an adversary to impersonate others, discover sensitive information, control the outcome of a session, or perform other attacks. This type of attack targets invalid assumptions that may be inherent in implementers of the protocol, incorrect implementations of the protocol, or vulnerabilities in the protocol itself."
                       :capec-category-id 512
                       }
                      {:capec-id 216
                       :capec-short-description "Communication Channel Manipulation"
                       :capec-long-description "An adversary manipulates a setting or parameter on communications channel in order to compromise its security. This can result in information exposure, insertion/removal of information from the communications stream, and/or potentially system compromise."
                       :capec-category-id 512
                       }
                      {:capec-id 594
                       :capec-short-description "Traffic Injection"
                       :capec-long-description "An adversary injects traffic into the target's network connection. The adversary is therefore able to degrade or disrupt the connection, and potentially modify the content. This is not a flooding attack, as the adversary is not focusing on exhausting resources. Instead, the adversary is crafting a specific input to affect the system in a particular way."
                       :capec-category-id 512
                       }
                      {:capec-id 624
                       :capec-short-description "Fault Injection"
                       :capec-long-description "The adversary uses disruptive signals or events (e.g. electromagnetic pulses, laser pulses, clock glitches, etc.) to cause faulty behavior in electronic devices. When performed in a controlled manner on devices performing cryptographic operations, this faulty behavior can be exploited to derive secret key information."
                       :capec-category-id 512
                       }
                      ])]
    req))

(defn get-object-type
  [obj]
  ;(println "****** In get-object-type obj=" obj " var=" (mcp/find-variable (str obj)))
  obj)

(defn emit-backdoor-safe-requirement
  [dsps object field]
  (requirement
   (list 'Should-be-guaranteed-to-be-absent-of-backdoors (str (get-object-type object)))
   :desirable-properties dsps
   :capecs [{:capec-id 443
             :capec-short-description "Malicious Logic Inserted Into Product Software by Authorized Developer"
             :capec-long-description "An adversary uses their privileged position within an authorized software development organization to inject malicious logic into a codebase or product. Supply chain attacks from approved or trusted developers are extremely difficult to detect as it is generally assumed the quality control and internal security measures of these organizations conform to best practices. In some cases the malicious logic is intentional, embedded by a disgruntled employee, programmer, or individual with an otherwise hidden agenda. In other cases, the integrity of the product is compromised by accident (e.g. by lapse in the internal security of the organization that results in a product becoming contaminated). In other cases, the developer embeds a backdoor into a product to serve some purpose, such as product support, but discovery of the backdoor results in its malicious use by adversaries."
             :capec-category-id 512
             }]))

(defn emit-phishing-safe-requirement
  [dsps object field]
  (requirement
   (list 'Should-be-guaranteed-to-resiliant-to-phishing (str (get-object-type object)))
   :desirable-properties dsps
   :capecs [{:capec-id 98
             :capec-short-description "Phishing"
             :capec-long-description "Phishing is a social engineering technique where an attacker masquerades as a legitimate entity with which the victim might do business in order to prompt the user to reveal some confidential information (very frequently authentication credentials) that can later be used by an attacker. Phishing is essentially a form of information gathering or \"fishing\" for information."
             :capec-category-id 512
             }
            {:capec-id 163
             :capec-short-description "Spear Phishing"
             :capec-long-description "An adversary targets a specific user or group with a Phishing (CAPEC-98) attack tailored to a category of users in order to have maximum relevance and deceptive capability. Spear Phishing is an enhanced version of the Phishing attack targeted to a specific user or group. The quality of the targeted email is usually enhanced by appearing to come from a known or trusted entity. If the email account of some trusted entity has been compromised the message may be digitally signed. The message will contain information specific to the targeted users that will enhance the probability that they will follow the URL to the compromised site. For example, the message may indicate knowledge of the targets employment, residence, interests, or other information that suggests familiarity. As soon as the user follows the instructions in the message, the attack proceeds as a standard Phishing attack."
             :capec-category-id 512
             }
            {:capec-id 164
             :capec-short-description "Mobile Phishing"
             :capec-long-description "An attacker targets mobile phone users with a phishing attack for the purpose of soliciting account passwords or sensitive information from the user. Mobile Phishing is a variation on the Phishing social engineering technique where the attack is initiated via mobile texting rather than email. The user is enticed to provide information or go to a compromised web site via a text message. Apart from the manner in which the attack is initiated, the attack proceeds as a standard Phishing attack."
             :capec-category-id 512
             }]))

(defn emit-pw-guessing-safe-requirement
  [dsps object field]
  (requirement
   (list 'Should-be-guaranteed-to-resiliant-to-pw-guessing (str (get-object-type object)))
   :desirable-properties dsps
   :capecs [{:capec-id 70
             :capec-short-description "Try Common or Default Usernames and Passwords"
             :capec-long-description "An adversary may try certain common or default usernames and passwords to gain access into the system and perform unauthorized actions. An adversary may try an intelligent brute force using empty passwords, known vendor default credentials, as well as a dictionary of common usernames and passwords. Many vendor products come preconfigured with default (and thus well-known) usernames and passwords that should be deleted prior to usage in a production environment. It is a common mistake to forget to remove these default login credentials. Another problem is that users would pick very simple (common) passwords (e.g. \"secret\" or \"password\") that make it easier for the attacker to gain access to the system compared to using a brute force attack or even a dictionary attack using a full dictionary."
             :capec-category-id 512
             }]))

(defn emit-supply-chain-attack-safe-requirement
  [dsps object field]
  (requirement
   (list 'Should-be-guaranteed-to-absent-of-supply-chain-vulnerabilities (str (get-object-type object)))
   :desirable-properties dsps
   :capecs [{:capec-id 176
             :capec-short-description "Configuration/Environment Manipulation"
             :capec-long-description "An attacker modifies a technology, product, or component during a stage in its manufacture for the purpose of carrying out an attack against some entity involved in the supply chain lifecycle. There are an almost limitless number of ways an attacker can modify a technology when they are involved in its manufacture, as the attacker has potential inroads to the software composition, hardware design and assembly, firmware, or basic design mechanics. Additionally, manufacturing of key components is often outsourced with the final product assembled by the primary manufacturer. The greatest risk, however, is deliberate manipulation of design specifications to produce malicious hardware or devices. There are billions of transistors in a single integrated circuit and studies have shown that fewer than 10 transistors are required to create malicious functionality."
             :capec-category-id 437
             }
            {:capec-id 438
             :capec-short-description "Modification During Manufacture"
             :capec-long-description "An attacker modifies a technology, product, or component during a stage in its manufacture for the purpose of carrying out an attack against some entity involved in the supply chain lifecycle. There are an almost limitless number of ways an attacker can modify a technology when they are involved in its manufacture, as the attacker has potential inroads to the software composition, hardware design and assembly, firmware, or basic design mechanics. Additionally, manufacturing of key components is often outsourced with the final product assembled by the primary manufacturer. The greatest risk, however, is deliberate manipulation of design specifications to produce malicious hardware or devices. There are billions of transistors in a single integrated circuit and studies have shown that fewer than 10 transistors are required to create malicious functionality."
             :capec-category-id 437
             }
            {:capec-id 439
             :capec-short-description "Manipulation During Distribution"
             :capec-long-description "An attacker undermines the integrity of a product, software, or technology at some stage of the distribution channel. The core threat of modification or manipulation during distribution arise from the many stages of distribution, as a product may traverse multiple suppliers and integrators as the final asset is delivered. Components and services provided from a manufacturer to a supplier may be tampered with during integration or packaging."
             :capec-category-id 437
             }
            {:capec-id 440
             :capec-short-description "Hardware Integrity Attack"
             :capec-long-description "An adversary exploits a weakness in the system maintenance process and causes a change to be made to a technology, product, component, or sub-component or a new one installed during its deployed use at the victim location for the purpose of carrying out an attack."
             :capec-category-id 437
             }
            {:capec-id 441
             :capec-short-description "Malicious Logic Insertion"
             :capec-long-description "An adversary installs or adds malicious logic (also known as malware) into a seemingly benign component of a fielded system. This logic is often hidden from the user of the system and works behind the scenes to achieve negative impacts. With the proliferation of mass digital storage and inexpensive multimedia devices, Bluetooth and 802.11 support, new attack vectors for spreading malware are emerging for things we once thought of as innocuous greeting cards, picture frames, or digital projectors. This pattern of attack focuses on systems already fielded and used in operation as opposed to systems and their components that are still under development and part of the supply chain."
             :capec-category-id 437
             }]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; here are the rules:

;;; a rule returns either nil or a list of requirements.  it is applied to each object in the model.
;;; all non nil rules are concatenated to produce a final list of requirements.

;;; rule 1: if connection to outside require connection to be pki encrypted.
;;; (communications-requiring-pki-encryption-on-network name-of-object-concerned)


; MissileGuidanceUnit > cellnet = GroundStation

(def translations {"Main.ins"  "SeekerTrackerComponent_Impl",
                   "Main.gps"  "SeekerTrackerComponent_Impl",
                   "Main.cell_net"  "GroundStation_Impl",
                   "MissileGuidanceUnit.cellnet" "GroundStation",
                   "MissileGuidanceNetwork.outside-network" "GroundStation",
                   "MissileGuidanceUnit.localnet" "MissileGuidanceBus",
                   "MissileGuidanceUnit.gps" "SeekerTrackerComponent",
                   "MissileGuidanceUnit.ins" "SeekerTrackerComponent",
                   "router.sent-command-abort" "MissileGuidanceBus"})

(def dpmap-cache nil)

;;; (rt/find-type-of-field object-type field)

(defn lookup-dps
  [dpmap object field]
  (def dpmap-cache dpmap)
  (let [index (with-out-str (printf "%s.%s" object field))
        ;;- (println "index=" index)
        fieldval (rt/find-type-of-field object field)
        ;;- (println "field-value = " fieldval)
        translate (get translations index)
        value (if translate (get dpmap translate) (get dpmap (str fieldval)))]
    ;;(println "value = " value " dpmap = " (keys dpmap))
    (if value (into [] @value))))

(defn rule-1
  "rule 1: If connection to outside require connection to be pki encrypted."
  [component-type-map model-types connectivity-map all-methods prob-reqd dpmap laterals om cdict]
  (let [external-connection-list
        (external-connections component-type-map model-types connectivity-map cdict)]
    (println "Rule-1: " external-connection-list)
    (map (fn [[object field users]]
           (let [dsps (lookup-dps dpmap object field)]
             (if (or dsps (> prob-reqd 0.99)) (emit-pki-communication-requirement dsps object field))))
         external-connection-list)))

;;; rule 2: IF connection via internal network require connection to be pki encrypted.
;;; (communications-requiring-pki-encryption-on-network name-of-object-concered)

(defn rule-2a
  "rule 2: IF connection to internal network, require connection to be pki encrypted."
  [component-type-map model-types connectivity-map all-methods prob-reqd dpmap laterals om cdict]
  (let [internal-connection-list
        (internal-connections component-type-map model-types connectivity-map cdict)]
    (println "Rule-2a: " internal-connection-list)
    (map (fn [[object field users]]
           (let [dsps (lookup-dps dpmap object field)]
             (if (or dsps (> prob-reqd 0.99)) (emit-pki-communication-requirement dsps object field))))
         internal-connection-list)))

;; dpmap= (DeliveryDrone_Radio DeliveryDrone_DeliveryPlanner DeliveryDrone_Camera)
;; laterals= [[(DeliveryDrone_Radio DeliveryDrone_DeliveryPlanner DeliveryDrone_Camera) 1]]
;;

(defn rule-2b
  "rule 2: If connection to internal network, require connection to be pki encrypted."
  [component-type-map model-types connectivity-map all-methods prob-reqd dpmap laterals om cdict]
  (let [connections (mcp/find-binary-propositions-matching nil nil #{:connects-with} nil nil nil)
        isa (mcp/find-binary-propositions-matching nil nil #{:is-a} nil nil nil)
        dpmapset (set (keys dpmap))
        ;;- (println "dpmapset = " dpmapset "model-root = " model-root "(type model-root) = "(type model-root))
        objtyp (into {} (map (fn [{obj :subject, typ :object}] {obj typ}) isa))
        ;;- (pprint objtyp)
        ;;- (println "first entry of objtyp = " (first objtyp) "type of second part = " (type (second (first objtyp))))
        - (println "Rule-2b: ")
        res (map (fn [{from :subject, to :object}]
                   (let [fromtyp (get objtyp from)
                         totyp (get objtyp to)
                         dps  (seq (set (concat (maybe-deref (get dpmap (str fromtyp)) :normal)
                                                (maybe-deref (get dpmap (str totyp)) :normal))))
                         fromdp (set/intersection dpmapset (set [(str fromtyp)]))
                         todp (set/intersection dpmapset (set [(str totyp)]))]
                     ;; If prob >= 0.99 all of them
                     (if (and
                          (not (string/includes? (str fromtyp) "TypicalAttacker"))
                          (not (string/includes? (str totyp) "TypicalAttacker"))
                          (not (= model-root (str fromtyp)))
                          (not (= model-root (str totyp)))
                          (not (= (str fromtyp) (str totyp))))
                       ;; If prob >= 0.8 only the ones with both dps
                       ;; If prob > 0.95 the ones with one or both dps
                       (cond (>= prob-reqd 0.99)
                             (emit-communication-requirement dps fromtyp totyp)

                             (and (> prob-reqd 0.95)
                                  (or (not (empty? todp)) (not (empty? fromdp))))
                             (emit-communication-requirement dps fromtyp totyp)

                             (and (>= prob-reqd 0.80) (not (empty? todp)) (not (empty? fromdp)))
                             (emit-communication-requirement dps fromtyp totyp)))))
                 connections)]
    res))

;;; rule 3: IF there are redundant navigational sensors they must be monitored for excess deviation
;;; (filter-required-for-excess-deviation-detection [list-of-concerned-redundant-sensors])

(defn rule-3 ;+++ should be updated to handle multiple requirements
  "rule 3: IF there are redundant navigational sensors they must be monitored for excess deviation."
  [component-type-map model-types connectivity-map all-methods prob-reqd dpmap laterals om cdict]
  ;[object fields connectiontypes all-lvs allclasses prob-reqd]
  (let [navigational-sensors (all-navigational-sensors component-type-map model-types connectivity-map cdict)]
    (println "Rule-3: " navigational-sensors)

    (when (> (count navigational-sensors) 1)
      (let [[object field user] (first navigational-sensors)
            dsps  (lookup-dps dpmap object field)]
        (if (or dsps (> prob-reqd 0.99))
          (list (emit-redundant-sensor-deviation-monitoring-requirement
                 dsps navigational-sensors)))))))

;;; rule 4: IF communications with outside, require redundant PKI connection for specific methods.
;;; (add-backup-network-link-using-pki-encryption
;;;     :substituting-for from
;;;     :connecting-to to
;;;     :allowed-methods [list-of-methods-concerned])

(defn rule-4
  "IF communications with outside, require redundant PKI connection for specific methods."
  [component-type-map model-types connectivity-map all-methods prob-reqd dpmap laterals om cdict]
  (let [external-connection-list (external-connections component-type-map model-types connectivity-map cdict)]
    (println "Rule-4: " external-connection-list)
    (map (fn [[object field users]]
           (let [methods (apply concat
                                (remove nil?
                                        (map (fn [fld]
                                               (get-methods object fld
                                                            component-type-map all-methods))
                                             users)))]
             (if (and users (not (empty? methods)))
               (let [dsps (lookup-dps dpmap (second users) (first methods))]
                 #_(println "users=" users " methods=" methods)
                 (if (or dsps (> prob-reqd 0.99))
                   (emit-backup-network-requirement dsps (first users) (second users) methods))))))
         external-connection-list)))

;;; rule 5: IF requirement > 95% probability, insist on redundant navigational sensors not requiring radio.
;;; (add-module-to-specifed-input-functional-design new-nav-sensor
;;;     :pclass NavigationalSensorNotDependentOnRadio
;;;     :where (independent existing-non-dependant-sensor new-nav-sensor))

(defn rule-5 ;+++ should be extended to support multiple requirements
  "IF requirement >= 95% probability, insist on redundant navigational sensors not requiring radio."
  [component-type-map model-types connectivity-map all-methods prob-reqd dpmap laterals om cdict]
  (let [navigational-sensors (non-radio-navigational-sensors component-type-map model-types connectivity-map cdict)]
    (println "rule-5: navigational-sensors=" navigational-sensors "count=" (count navigational-sensors) "rprob=" prob-reqd)
    (if (and (>= prob-reqd 0.95)
             (== (count navigational-sensors) 1))
      (let [[object field connections] (first navigational-sensors)
            dsps (lookup-dps dpmap object field)]
        (println "In Rule-4: object = " object "field = " field "dsps = " dsps)
        (if (or dsps (> prob-reqd 0.99))
          (list (emit-redundant-non-radio-sensors-requirement
                 dsps (first navigational-sensors))))))))

#_(defn rule-6
  "Communication along attack paths should require PKI security to minimize lateral moves."
  [component-type-map model-types connectivity-map all-methods prob-reqd dpmap laterals om cdict]
  (let [navigational-sensors (non-radio-navigational-sensors component-type-map model-types connectivity-map cdict)]
    ;;(println "rule-6: navigational-sensors=" navigational-sensors "count=" (count navigational-sensors) "rprob=" prob-reqd)
    (if (and (>= prob-reqd 0.95)
             (== (count navigational-sensors) 1))
      (list (emit-redundant-non-radio-sensors-requirement (first navigational-sensors))))))

(defn filter-other-attacks
  [type om]
  (remove nil?
          (apply concat
                 (map (fn [[attackseq reqs]]
                        (map (fn [[method target]]
                               (if (= type (str method))
                                 [target reqs]))
                             attackseq))
                      om))))

;;; (filter-other-attacks "launch-backdoor-attack" '[[((launch-backdoor-attack TypicalAttacker_Impl55)(launch-supply-chain-attack Router_Impl67)(launch-backdoor-attack foo)) 1]])

(defn is-dcrypps-category
  [object category cdict]
  (let [otypes (get cdict category)
        isp (if otypes (some #{(str object)} otypes))]
    (println "**** is-dcrypps-category cdict=" (pr-str cdict) " otypes=" (pr-str otypes) " and " object (if isp "is" "is not") " in dcrypps category " category)
    isp))

(defn rule-7a
  "If attack-type=launch-backdoor-attack on x, x should be resiliant to back-door-attacks"
  [component-type-map model-types connectivity-map all-methods prob-reqd dpmap laterals om cdict]
  (let [backdoor-attacks (filter-other-attacks "launch-backdoor-attack" om)
        comps (remove empty?
                      (concat (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :Workstation cdict))
                              (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :Router cdict))
                              (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :SmartCamers cdict))                              (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :SmartRadio cdict))                              (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :SmartController cdict))))]
   (println "Rule-7a: " backdoor-attacks "comps=" comps)
   (map (fn [[object dps]]
          (if (and (or dps (> prob-reqd 0.99))
                   (or
                    (is-dcrypps-category object :Workstation cdict)
                    (is-dcrypps-category object :Router cdict)
                    (is-dcrypps-category object :SmartController cdict)
                    (is-dcrypps-category object :SmartCamera cdict)
                    (is-dcrypps-category object :SmartRadio cdict)))
            (emit-backdoor-safe-requirement dps object nil)))
        backdoor-attacks)))

(defn rule-7b
  "If attack-type=launch-phishing-attack on x, x should be resiliant to phishing-attacks"
  [component-type-map model-types connectivity-map all-methods prob-reqd dpmap laterals om cdict]
  (let [phishing-attacks (filter-other-attacks "launch-phishing-attack" om)
        comps (remove empty?
                      (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                           (get-objects-of-type :Workstation cdict)))]
   (println "Rule-7b: " phishing-attacks "comps=" comps)
   (map (fn [[object dps]]
          (if  (and (or dps (> prob-reqd 0.99))
                   (or
                    (is-dcrypps-category object :Workstation cdict)))
            (emit-phishing-safe-requirement dps object nil)))
        phishing-attacks)))

(defn rule-7c
  "If attack-type=launch-PW-guessing on x, x should be resiliant to PW-guessing"
  [component-type-map model-types connectivity-map all-methods prob-reqd dpmap laterals om cdict]
  (let [pw-guessing (filter-other-attacks "launch-PW-guessing" om)
        comps (remove empty?
                      (concat (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :Workstation cdict))
                              (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :Router cdict))
                              (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :SmartCamera cdict))))]
   (println "Rule-7c: " pw-guessing "comps=" comps)
   (map (fn [[object dps]]
          (if (and (or dps (> prob-reqd 0.99))
                   (or
                    (is-dcrypps-category object :Workstation cdict)
                    (is-dcrypps-category object :Router cdict)
                    (is-dcrypps-category object :SmartCamera cdict)))
            (emit-pw-guessing-safe-requirement dps object nil)))
        pw-guessing)))

(defn rule-7d
  "If attack-type=launch-supply-chain-attack on x, x should be resiliant to supply-chain-attacks"
  [component-type-map model-types connectivity-map all-methods prob-reqd dpmap laterals om cdict]
  (let [supply-chain-attacks (filter-other-attacks "launch-supply-chain-attack" om)
        comps (remove empty?
                      (concat (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :Router cdict))
                              (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :SmartController cdict))
                              (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :SmartCamera cdict))
                              (map (fn [x] (irx/components-of-type component-type-map model-types connectivity-map x))
                                   (get-objects-of-type :SmartRadio cdict))))]
   (println "Rule-7d: " supply-chain-attacks "comps=" comps)
   (map (fn [[object dps]]
          (if (and (or dps (> prob-reqd 0.99))
                   (or
                    (is-dcrypps-category object :Router cdict)
                    (is-dcrypps-category object :SmartController cdict)
                    (is-dcrypps-category object :SmartCamera cdict)
                    (is-dcrypps-category object :SmartRadio cdict)))
            (emit-supply-chain-attack-safe-requirement dps object nil)))
        supply-chain-attacks)))

;;; End of rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gen-reqs
  [model-types component-type-map wired-constructors connectivity-map all-methods rprob  dpmap laterals om cdict]
  ;; (println "model-types=" model-types)
  ;; (println "component-type-map=" component-type-map)
  ;; (println "wired-constructors=" wired-constructors)
  ;; (println "connectivity-map=" connectivity-map)
  ;; (println "all-methods=" all-methods)
  ;; (println "rprob=" rprob)
  (println "dpmap=" (keys dpmap))
  (println "laterals=" laterals)

  (remove nil?
          (if (> rprob 0.1)
            [;; Rules
             (rule-1 component-type-map model-types connectivity-map all-methods rprob dpmap laterals om cdict)
             (rule-2a component-type-map model-types connectivity-map all-methods rprob dpmap laterals om cdict)
             (rule-2b component-type-map model-types connectivity-map all-methods rprob dpmap laterals om cdict)
             (rule-3 component-type-map model-types connectivity-map all-methods rprob dpmap laterals om cdict)
             (rule-4 component-type-map model-types connectivity-map all-methods rprob dpmap laterals om cdict)
             (rule-5 component-type-map model-types connectivity-map all-methods rprob dpmap laterals om cdict)
             ;;   (rule-6 component-type-map model-types connectivity-map all-methods rprob dpmap laterals om cdict)

             ;; the following requirement does not follow from the model since noone uses localnet - fix the pamela!
             #_(let [dsps (lookup-dps dpmap "MissileGuidanceUnit" "localnet")]
                 (if dsps (list (emit-pki-communication-requirement dsps "MissileGuidanceUnit" "localnet"))))
             (rule-7a component-type-map model-types connectivity-map all-methods rprob dpmap laterals om cdict)
             (rule-7b component-type-map model-types connectivity-map all-methods rprob dpmap laterals om cdict)
             (rule-7c component-type-map model-types connectivity-map all-methods rprob dpmap laterals om cdict)
             (rule-7d component-type-map model-types connectivity-map all-methods rprob dpmap laterals om cdict)
             ]
            [])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filter requirements
;;;
;;; Remove duplicates

(defn add-requirement
  [reqset req]
  (reset! reqset (conj @reqset req)))

(defn contains-reverse-direction?
  [aset from to]
  (some (fn [x]
          ;; (println x)
          (let [[- [reqtype & args] & args2] x]
            ;; (println reqtype)
            (if (= reqtype 'communications-should-guard-against-lateral-attacks-between)
              (let [[fr au] args]
                ;; (println "*** " from to fr au)
                (and (= from au) (= to fr))))))
        (seq aset)))

(defn filter-requirements
  [requirements]
  (let [origcard (count requirements)
        reqset (set requirements) ; strip out existing simple duplication
        newset (atom #{})
        newcard (count reqset)]         ; new set with custom treatment based on message type.
    (doseq [req (seq reqset)]
      (let [{[reqtype & args] :requirements
             dps :desirable-properties
             capecs :capecs} req]
        ;; (println "reqtype = " reqtype " args = " args " dps = " dps)
        (case reqtype
          communications-should-guard-against-lateral-attacks-between
          (let [[from to] args]
            ;; (println "checking on lateral guard" req #_"newset = " #_@newset " from = " from " to = " to)
            (if (not (contains-reverse-direction? @newset from to))
              (add-requirement newset req)  ; keep it
              #_(println "********* Discarding superfluous requirement: " req)))

          communications-requiring-pki-encryption-on-network
          (add-requirement newset req)  ; keep it

          filter-required-for-excess-deviation-detection
          (add-requirement newset req)  ; keep it

          add-backup-network-link-using-pki-encryption
          (add-requirement newset req)  ; keep it

          add-module-to-specifed-input-functional-design
          (add-requirement newset req)  ; keep it

          (add-requirement newset req)))) ; Default - just keep it
    (println "Original # reqs = " origcard " minus duplicates = " newcard " finally " (count @newset))
    (seq reqset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Requirements generator

;;; From IR extract all classes.
;;; For each field of each object, if assigned an object list [fieldname type]
;;; For each type, make list of inheritance tree.
;;; Make a list of LV's and for each LV which components were passed the LV
;;; For each object compute which objects share LV's and propagate the types to the two objects

(declare interpret-attack-plan)

(defn diagnose-requirements
  [attack model #_desired rprob cdict]
  ;;(if (> irx/verbosity 0) (println "In diagnose-requirements attack=" attack "model=" model))
  ;;(if (> irx/verbosity 0) (println "In diagnose-requirements cdict=" cdict "rprob=" rprob))
  (if (empty? attack) (println "Attack plan missing"))
  (if (empty? model) (println "Pamela model missing"))
  (if (empty? cdict) (println "Dictionary missing"))
  ;; (if (empty? desired) (println "Desired properties missing"))
  (if (not (number? rprob)) (println "required probability missing"))

  (if model
    ;; Both the attack plan and the model are necessary for the diagnosis
    ;; desired and required probability (rprob should be required too. We can,
    ;; however, generate some requirements without the attack plan, so we don't require it.
    (let [[dpmap laterals om] (interpret-attack-plan attack)
          all-class-names (irx/get-pclass-names-from-ir model)
          ;; - (println "all-class-names=" all-class-names)
          all-fields (irx/get-fields-from-ir model)
          all-methods (irx/get-methods-from-ir model)
          all-lvars (irx/get-lvars-from-ir model)
          model-types (irx/get-pclass-types-from-ir model)
          component-type-map (irx/get-component-type-map-from-ir model)
          ;; - (println "component-type-map=" component-type-map)
          wired-constructors (irx/get-wired-constructors model)
          connectivity-map (irx/component-connectivity-map wired-constructors)
          ;; - (println "connectivity-map=" connectivity-map)
          ext-cons (external-connections component-type-map model-types connectivity-map cdict)
          int-cons (internal-connections component-type-map model-types connectivity-map cdict)
          allnavsensors (all-navigational-sensors component-type-map model-types connectivity-map cdict)
          nrnavsensors (non-radio-navigational-sensors component-type-map model-types connectivity-map cdict)

          ;;all-transitions (irx/get-transitions-from-ir model)
          ;;all-modes (irx/get-modes-from-ir model)

          ;; - (println "***dpmap=" (keys dpmap))
          ;; - (println "***laterals=" laterals)

          generated-reqs (into []
                               (remove nil?
                                       (apply concat
                                              (gen-reqs model-types
                                                        component-type-map
                                                        wired-constructors
                                                        connectivity-map
                                                        all-methods
                                                        rprob
                                                        dpmap
                                                        laterals
                                                        om
                                                        cdict))))]

      (if (> irx/verbosity 0) (println "\nall-class-names = " (map #(first %) all-class-names) "\n"))
      ;;(println "all-fields = " all-fields)
      (if (> irx/verbosity 0) (println "all-methods = " all-methods))
      ;;(println "all-transitions = " all-transitions)
      ;;(println "all-lvars = " all-lvars "\n")
      ;;(println "all-modes = " all-modes)
      (if (> irx/verbosity 0) (println "all-types:\n" (with-out-str (pprint model-types))))
      (if (> irx/verbosity 0) (println "component-type-map:\n" (with-out-str (pprint component-type-map))))
      (if (> irx/verbosity 0) (println "wired-constructors: \n" (with-out-str (pprint connectivity-map)) "\n"))
      ;;(println (count generated-reqs) "Requirements generated")
      ;;(pprint generated-reqs)
      (if (not (empty? generated-reqs))
        {;:type-map component-type-map
         ;:model-essential-structure connectivity-map
         :requirements (filter-requirements generated-reqs)
         :pcc ["Not yet installed"]}))
    nil)) ; if no attack plan no requirements

(defn generate-requirements
  "Produce the requirements given the model, attack-plan, and desired properties"
  [rq]
  (if (> irx/verbosity 0) (println "In generate requirements"))
  (let [title (rq :title)
        cdict (rq :class-dictionary)
        ap (or (rq :ap-json) nil)
        pm (or (rq :pm-json) nil)
        ;; desired (rq :desired)
        - (if (> irx/verbosity 0) (do (println "\n dictionary =") (pprint cdict)))
        ;;- (if (> irx/verbosity 0) (do (println "\n attack plan =") (pprint ap)))
        ;;- (if (> irx/verbosity 0) (do (println "\n pamela model =") (pprint pm)))

        target-prob (or (rq :desired-prob) 0.96)
        generated (diagnose-requirements ap pm #_desired target-prob cdict)]
    ;;(if (> irx/verbosity 0) (do (println "\n attack plan =") (pprint ap)))
    ;;(if (> irx/verbosity 0) (do (println "\n pamela model =") (pprint pm)))
    ;; (if (> irx/verbosity 0) (do (println "\n desired =") (with-out-str (pprint desired))))
    (if generated
      (do
        (println (count (get generated :requirements)) " requirements generated.")
        #_(doseq [rn (get generated :requirements)] (pprint rn))))
    (or generated "No requirements generated.")))


(defn output-requirements-to-a-file
  [destination reqs]
  (with-open [ostrm (java.io.OutputStreamWriter.(java.io.FileOutputStream. destination) "UTF-8")]
    ;;(xml/emit reqs ostrm)
    (.write ostrm (with-out-str (pprint reqs)))
    (.write ostrm "\n")))

(defn load-model-for-requirements-generation
  [pamela-json-string root]
  #_(println "In load-model-for-requirements-generation with root=:" root)
  #_(pprint pamela-json-string)
  (if (> irx/verbosity 0)  (println "Resetting the model"))
  (resetall)
  (if (> irx/verbosity 0) (println "Loading the model"))
  ;(try
    (do
      (load-model-from-ir pamela-json-string root nil)
      (establish-unidirectional-connectivity-propositions root))
    #_(catch Exception e (println (str "Bad PAMELA json model: " (.getMessage e))));)
  nil)

(defn make-requirements
  "Generate the requirements."
  [args]
  (let []
    (binding [attack-plan (args :attack-plan)
              irx/pamela-model (args :pamela-model)
              ;;pamela-model-json  (args :pamela-model-json)
              pamela-model-json (irx/read-ir-from-json-string (args :pamela-model-json))
              irx/outfile (args :outfile)
              ;; desired (args :dps-from)
              irx/verbosity (args :verbose)]
      (if (or true (> irx/verbosity 0)) (println "args=" args))
      (if (or true (> irx/verbosity 0)) (printf "Generating requirements%n"))
      ;; (println "pamela-model=" irx/pamela-model)

      (irx/setirfortesting pamela-model-json)

      (def model-root (args :root))

      (load-model-for-requirements-generation pamela-model-json model-root)
      (mcp/describe-belief-state)

      (let [attack-plan-json attack-plan
            ;; desired-str desired
            requirements (generate-requirements
                          (merge args {
                                       :title "2019"
                                       :ap-json attack-plan-json
                                       :pm-json pamela-model-json
                                       ;; :desired desired-str
                                       :desired-prob (args :desired-prob)}))
             - (if (not (empty? irx/outfile))
                (do
                  (if (> irx/verbosity 0) (printf "Requirements output to %s%n" irx/outfile))
                  (output-requirements-to-a-file irx/outfile requirements)))]
         (if (> irx/verbosity 0) (pprint requirements))
        requirements))))

;;; How to build a map of connected objects
;;; within a class, some fields are lvars
;;;   those fields are passed to a constructor in another field definition.
;;;   Each constructed object is connected to the other objects that are created with the same lvar args.
;;;
;;; So, for each pclass, 1. find the fields that are lvars 2. find the fileds that are constructors.
;;; 3. find which lvars are passed to which constructors.  If object A assigned to fields Y and object B assigned to field Z both share an LVAR, they are "connected".
;;; [{fieldname object type [lvars-used]} ...] for each field receiving a constructor.
;;; pclassname.fieldname therefore names the components in pclassname
;;; if the lvars-used is represented as a set, if the lvarset of component A has a non-null intersection with
;;; component B, they are "connected"
;;; if A is connected to B and B is connected to C, A is not necessarily directly connected to C.
;;;
;;; We want to find connections between objects and local network... between objects and a cellular network.
;;; We want to find an object that has sensors and how many and of what type in order to determine if there is
;;; adequate redundency.

(defn load-desirable-properties-from-raw-json-string
  [raw-json-string]
  (let [desprops (json/read-str raw-json-string)]
    desprops))

(defn load-desired-properties [fn]
  (let [desprops (and fn (.exists (io/file fn)) (json/read-str (slurp fn)))]
    desprops))

(defn load-dp-json-from-file
  [fn]
  (let [rawdesprops (and fn (.exists (io/file fn)) (json/read-str (slurp fn)))]
    rawdesprops))

(defn load-attack-plan-from-json-file
  [fn]
  (let [attack-model (and fn (.exists (io/file fn)) (json/read-str (slurp fn)))]
    attack-model))

(def raw-pamela-json "model")
(def raw-dp-json "dp")
(def attack-plan-json "attack")

(defn interpret-attack-sequence
  [dp-implication-map attack]
  (loop [attack-path attack]
    (let [[method variable dp type] (first attack-path)
          rest (rest attack-path)
          dpmap @dp-implication-map
          val (get dpmap type)
          nudp (read-string (first dp))]
      (reset! dp-implication-map
              (into dpmap
                    {type (if val
                            (do (reset! val (conj @val nudp)) val)
                            (atom #{nudp}))}))
      (if (not (empty? rest)) (recur rest)))))

;;; (reverse-labeling-of-plans attack-plan-json)

(defn reverse-labeling-of-plans
  [ap]
  (map (fn [aseq]
         (let [labels (into ["start"]
                            (map #(if (= (first %) "up") "down" (first %)) aseq))]
           (map (fn [act nulab]
                  (cons nulab (rest act)))
                aseq (take (count aseq) labels))))
   ap))

;; (defn interpret-attack-plan
;;   [oap]
;;   (if oap
;;     (let [ap (reverse-labeling-of-plans oap)
;;           lateral-movements (atom [])
;;           dp-implication-map (atom {})]
;;       (doseq [attack-path ap]
;;         (let [latpath (remove nil? (map (fn [path]
;;                                           (if (not (= (first path) "down")) (nth path 3)))
;;                                         (rest attack-path)))
;;               latdp (read-string (first (nth (second attack-path) 2)))]
;;           (reset! lateral-movements (into @lateral-movements [[latpath latdp]]))
;;           (interpret-attack-sequence dp-implication-map (rest attack-path)))
;;         #_(println "dp-implication-map=" @dp-implication-map))
;;       [@dp-implication-map @lateral-movements])))


(defn interpret-attack-plan
  [oap]
  (if oap
    (let [ap oap                      ;No longer need to reverse (reverse-labeling-of-plans oap)
          lateral-movements (atom [])
          other-methods (atom [])
          dp-implication-map (atom {})]
      (doseq [attack-path ap]
        (let [latpath (remove nil? (map (fn [path]
                                          (if (or (= (first path) "up")
                                                  (= (first path) "down")
                                                  (= (first path) "lateral"))
                                            (nth path 1)))
                                        attack-path)) ; was (rest ...)
              methods (remove nil? (map (fn [path]
                                          (if (not (or (= (first path) "up")
                                                       (= (first path) "down")
                                                       (= (first path) "lateral")))
                                            (list (nth path 0) (nth path 3)))) ; 1 gives the numbered object
                                        attack-path)) ; was (rest ...)
              latdp (read-string (first (nth (second attack-path) 2)))]
          (reset! lateral-movements (into @lateral-movements [[latpath latdp]]))
          (reset! other-methods (into @other-methods [[methods latdp]]))
          (interpret-attack-sequence dp-implication-map (rest attack-path)))
        #_(println "dp-implication-map=" @dp-implication-map @other-methods))
      (println "**** ATTACK-PLAN INTERPRETATION ****")
      (println "dp-implication-map=" @dp-implication-map)
      (println "lateral-movements=" @lateral-movements)
      (println "other-methods=" @other-methods)

      [@dp-implication-map @lateral-movements @other-methods])))

;;; (second (interpret-attack-plan attack-plan-json))

(defn emit-pclass
  [pclass-name args method-list]
  {pclass-name
   {:args args,
    :methods method-list
    :type :pclass}})

(defn emit-pmethod ; body is a list
  [pmethod-name args body]
  [pmethod-name
   (list {:args args,
          :body body})])

(defn emit-sequence ; body is a list
  [body]
  {:type :sequence
   :body body})

(defn emit-parallel ; body is a list
  [body]
  {:type :parallel
   :body body})

(defn emit-choose ; body is a list
  [body]
  {:type :choose,
   :body body})

(defn emit-choice ; body is a list
  [body]
  {:type :choice,
   :body body})

(defn emit-call
  [name args]
  {:type :method-fn,
   :method-ref
   {:type :symbol-ref, :names (list 'this name)},
   :args args})

(defn emit-args
  [args]
  (seq (map (fn [arg] {:type :state-variable, :name arg}) args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generating a symbolic form from an attack plan

(defn compile-call
  [acall]
  (if (get #{"lateral" "start" "down"} (first acall))
    (let [direction (first acall)
          objname (second acall)
          dp (if (>= (count acall) 3) (nth acall 2))
          classname (if (>= (count acall) 4) (nth acall 3) objname)]
      (if (not (= direction "start"))
        [:call (if (= direction "lateral") 'move-lateral 'move-down)
         (list classname)]))
    (do
      ;; (println "acall=" acall)
      acall)))

(defn encode-as-tpn
  [sequence paths]
  ;;(println "seq=" sequence "remain=" paths)
  (if (empty? (first paths))
    sequence
    (if (every? #(= (second (first (first paths))) (second (first  %))) paths)
      (let [call (compile-call (first (first paths)))]
        (encode-as-tpn (if (not (empty? call))
                         (into sequence [call])
                         sequence)
                       (remove empty? (map rest paths))))
      (into sequence
            (let [divergeset (into #{} (map (fn [x] (second (first x))) paths))
                  ;; - (println "divergeset=" divergeset)
                  threads (map (fn [target]
                                 (remove nil?
                                         (map (fn [path]
                                                (if (= (second (first path)) target) path))
                                              paths)))
                               divergeset)]
              ;;(println "threads=" threads)
              [(into [:choice]
                     (map (fn [x]
                            (if (> (count (first x)) 1)
                              (into  [:sequence] (encode-as-tpn [] x))
                              (first (encode-as-tpn [] x))))
                          threads))])))))

(defn make-parallel-tpn-from-attack-plan
  [oap]
  (let [ap (reverse-labeling-of-plans oap)]
    (if (= (count ap) 1)
      (encode-as-tpn [] ap)
      ;; First divide the major parallel attack plans based on target
      (let [targetset (into #{} (map last ap))
            threads (map (fn [target]
                           (remove nil?
                                   (map (fn [path]
                                          (if (= (last path) target) path))
                                        ap)))
                         targetset)]
        (into [:parallel] (map #(into [:sequence] (encode-as-tpn [] %)) threads))))))

;;; (make-parallel-tpn-from-attack-plan attack-plan-json)
;;; (make-parallel-tpn-from-attack-plan (list (first attack-plan-json)))

(defn make-single-thread-tpn-from-attack-plan
  [ap]
  (encode-as-tpn [:sequence] (reverse-labeling-of-plans ap)))

;;; (make-single-thread-tpn-from-attack-plan attack-plan-json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convert a symbolic tpn onto an IR version

(defn convert-symbolic-tpn-to-ir
  [symbolic]
  (case (first symbolic)
    :pclass
    (emit-pclass (second symbolic) ; name
                 (nth symbolic 2) ; args
                 (into {} (map convert-symbolic-tpn-to-ir (nth symbolic 3))))

    :pmethod
    (emit-pmethod (second symbolic) ; method name
                  (map convert-symbolic-tpn-to-ir (nth symbolic 2)) ; arglist (or nil)
                  (map convert-symbolic-tpn-to-ir (nth symbolic 3)))

    :parallel
    (emit-parallel (map convert-symbolic-tpn-to-ir (rest symbolic)))

    :sequence
    (emit-sequence (map convert-symbolic-tpn-to-ir (rest symbolic)))

    :call
    (emit-call (second symbolic)
               (map (fn [arg]
                      {:type :state-variable, :name (symbol arg)})
                    (nth symbolic 2)))
    :choice
    (emit-choose
     (map (fn [achoice]
            (emit-choice (list (convert-symbolic-tpn-to-ir achoice))))
          (rest symbolic)))
    symbolic)) ;; unhandled cases are dumped in unconverted

(defn make-pclass-for-tpn
  [pclass-name pcargs pmethod-name args json]
  [:pclass pclass-name pcargs
           (list [:pmethod pmethod-name
                  args
                  (list (make-parallel-tpn-from-attack-plan json))])])

(defn convert-attack-model-to-ir
  [attack-model]
  (convert-symbolic-tpn-to-ir
   (make-pclass-for-tpn 'TypicalAttacker nil 'main nil attack-model)))

;; (defn convert-attack-model-to-pamela-text
;;   [attack-model]
;;   (pup/unparse-fn
;;    (convert-symbolic-tpn-to-ir
;;     (make-pclass-for-tpn 'TypicalAttacker nil 'main nil attack-model))))

;;; (convert-attack-model-to-ir attack-plan-json)
;;; (convert-attack-model-to-pamela-text attack-plan-json)
;;; (pprint (make-pclass-for-tpn 'TypicalAttacker 'main nil attack-plan-json))
;;; (pprint (convert-symbolic-tpn-to-ir (make-parallel-tpn-from-attack-plan attack-plan-json)))
;;; (pprint (make-pclass-for-tpn 'TypicalAttacker nil 'main nil attack-plan-json))
;;; (pprint (convert-symbolic-tpn-to-ir (make-pclass-for-tpn 'TypicalAttacker nil 'main nil attack-plan-json)))
;;; (pprint (pup/unparse-fn (convert-symbolic-tpn-to-ir (make-pclass-for-tpn 'TypicalAttacker nil 'main nil attack-plan-json))))

(def attack-model-filename "Filename goes here")

(defn interface-test ; to simulate the strings that we receive from the interface
  []
  ;;(def model-root "MissileGuidanceUnit") ;+++
  (def model-root "Main") ;+++
  (def raw-pamela-json (slurp irx/dcryppstest-model))
  ;; (def raw-dp-json (load-dp-json-from-file irx/desiredproperties))
  (def attack-plan-json (load-attack-plan-from-json-file attack-model-filename)))

;;; ************************************************************************
;;; DAN you wil need to cchange the pathname specified for outfile below...

(defn tester
  []
  (interface-test)
  (make-requirements {:attack-plan attack-plan-json
                      :pamela-model-json raw-pamela-json
                      :outfile "/users/paulr/checkouts/bitbucket/CASE-Vanderbilt-DOLL/data/missile-guidance/cyber-requirements.txt"
                      ;; :dps-from raw-dp-json
                      :desired-prob 0.85
                      :verbose 0}))

;;; (tester)




;;; --- Fin ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
