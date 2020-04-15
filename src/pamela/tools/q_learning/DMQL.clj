



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAMELA RL Q-Learning

(defn actionselector
  [list-of-atoms]
  ...)

(defn initialize-learner
  [cycletime learningrate discount epsilon actionselecter initialQ]
  {
   :q-table (atom initialQ)             ; Q-table
   :cycletime cycletime                 ; cycle time in milliseconds
   :alpha learningrate                  ; 0 lt learningrate lt 1
   :gamma discount                      ; 0 lt discount lt 1
   :epsilon 1                           ;
   :actionselectfunction actionselector ; function that selects an action.
   :reset-state reset-state             ; function returns starting discrete-state
   })

;;;
(defn make-fixed-sized-q-table-uniform-random
  [num-state-variables discretization num-actions low high]
  (if (= num-state-variables 0)
    (repeatedly num-actions (fn [] (atom
                                    (+ low (* (- high low)(rand))))))
    (repeatedly discretization
                (fn [] (make-fixed-sized-q-table-uniform-random
                        (- num-state-variables 1) discretization num-actions low high)))))

(defn fixed-sized-q-value
  [q-table & args]
  (reduce nth q-table args))

;;; (def test-q-table (make-fixed-sized-q-table-uniform-random 2 3 2 -2 0))

;;; (pprint (make-fixed-sized-q-table-uniform-random 2 3 2 -2 0))

(defn max-atom
  [list-of-atoms]
  (max (map deref list-of-atoms)))

(defn run-episode
  [learner startstate]
  (let [{;; Pull out all of the pieces aheadof the loop
         q-table :q-table
         alpha   :alpha
         gamma   :gamma
         select  :actionselectionfunction} learner]
    (loop [state startstate
           donep false
           ereward 0]
      (if (not donep)
        (let [;; Select a action
              action (select (apply fixed-sized-q-value state))
              ;; Perform action
              [new-state reward d] (perform action)
              discrete-state (get-discrete-state new-state)]
          (cond
            (not d)
            (let [max-future-q (max-atom (fixed-sized-q-value q-table discrete-state))
                  q-pos (fixed-sized-q-value (fixed-sized-q-value q-table state) action)
                  current-q (deref q-pos)
                  new-q (+ (* (- 1.0 alpha) current-q) ; Bellman's Equation
                           (* alpha (+ reward (* gamma max-future-q))))]
              (set! qpos new-q))

            (goal-achieved new-state)
            (set! (fixed-sized-q-value (fixed-sized-q-value q-table discrete-state) action) (+ ereward reward)))

          (recur  discrete-state d (+ ereward reward)))
        ))))

(defn train
  [learner episodes]
  (let [{q-table :q-table)
         reset-state :reset-state} learner]
    (dotimes [episode episodes]
      (run-episode learner (get-discrete-state (reset-state)))))

;;; Fin
