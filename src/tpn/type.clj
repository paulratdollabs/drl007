;
; The software contained herein is proprietary and
; a confidential trade secret of Dynamic Object Language Labs Inc.
; Copyright (c) 2012.  All rights reserved.
;

(ns tpn.type
  "In absence of formal type system, need to store hierarchy information")

(def nodetypes #{:state :p-begin :p-end :c-begin :c-end})
(def edgetypes #{:null-activity :activity :delay-activity})

(def begin-nodes #{:p-begin :c-begin})
(def end-nodes #{:p-end :c-end})



(def tpn-to-symbol {:cost         'cost= :reward 'reward= :temporal-constraint 'in-range :null-activity '=
                    :in-range-max 'in-range-max})

; When choosing var of which node to use in constraints, we prefer begin nodes then end nodes and then state
(def equivalent-class-preferences {:p-begin 0 :p-end 2
                                   :c-begin 1 :c-end 3
                                   :state   4})
