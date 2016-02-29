#|----------------------------------------------------------------------
------------------------------------------------------------------------
Copyright 2015 Francisco Javier Pulido Arrebola

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Description:   Several functions to carry out the execution of the corresponding
               algorithm with the parameters given by console applied to DIMACS problems 
------------------------------------------------------------------------
----------------------------------------------------------------------|#

(in-package :TEST-CASES)

(defvar *GOALS-FILES-PATH*)
(setf *GOALS-FILES-PATH* (concatenate 'string *DIMACS-FILES-PATH* "goals/"))

#|---------------------------------------------------------------------
Show an error whenever some of the parameters of the .exe file are not
correct (number or type of them)
-----------------------------------------------------------------------|#
(defun get-dimacs-goal-file (map)
  "Get the appropiate goals file according to the number of objectives used"
  (concatenate 'string *GOALS-FILES-PATH* (symbol-name map) ".goals.txt"))

#|---------------------------------------------------------------------
Show an error whenever some of the parameters of the .exe file are not
correct (number or type of them)
-----------------------------------------------------------------------|#
(defun show-parameter-dimacs-info (map prob-num ncosts algor tie-breaking)
  (format t "~%WRONG PARAMETERS!~%")
  (format t "map prob-num algor~%")
  (format t "~%map       (~S)  -  USA map" map)
  (format t "~%prob-num  (~S)  -  Number of problem to solve" prob-num)
  (format t "~%ncosts    (~S)  -  #Number of costs to consider (3, 4, 5)" ncosts)
  (format t "~%algor     (~S)  -  Algorithm to employ (:NAMOA, :LEXGO  ...) " algor)
  (format t "~%tie-breaking   (~S)  -  Tie-breaking policy (:LEXICOGRAPHIC, :LINEAR) " tie-breaking)
  (sleep 10))

(defun get-dimacs-problem (probn)
  "From parameters readed from console it returns a dimacs problem already stored in memory"
  (aref *DIMACS-MAP-PROBLEMS*  (1- probn)))

#|---------------------------------------------------------------------
Function to run a DIMACS experiment. Solve given problem and write result to file
-----------------------------------------------------------------------|#
(defun run-dimacs-problem (map prob-num ncosts algor tie-breaking &optional goals goal-per)
  "Execute map : prob-num from benchmark with algor. When lexgo is used, the optional parameters must be given"
  ; load map when is neccesary (this can be avoided preloading the map in advance)
  (let ((load-ini-time (get-internal-run-time)))
    (format t "Loading map of ~S... " map) 
    (load-map map)
    (format t "~%Map loaded in ~2,2f seconds.~%" 
            (float (/ (- (get-internal-run-time) load-ini-time) internal-time-units-per-second)))
    (let* ((alg                (get-algorithm-function algor ncosts))
           (dimacs-prob        (get-dimacs-problem prob-num))
           (tie-break-policy   (get-tie-breaking-function tie-breaking))
           (initial-graph-f    (get-initial-multiobjective-seach-graph algor ncosts))
           (stats-file         (concatenate 'string cl-user::*root* (get-stats-file-path algor tie-breaking)))
           (solutions-file     (concatenate 'string cl-user::*root* (get-solutions-file-path algor tie-breaking)))
           (stopped-probs-file (concatenate 'string cl-user::*root* "stopped-problems.txt"))
           (unsolved-probs-file (concatenate 'string cl-user::*root* "unsolved-problems.txt"))
           (t-end nil))
    ; check first if both nodes are connected by launching a single-objective djikstra search
      (multiple-value-bind (c1-ht solution? nnodes)
          (djikstra (ini-s dimacs-prob) (end-s dimacs-prob) dimacs-prob 0)
        (declare (ignore c1-ht))
        (format t "Checking problem has indeed a solution --> ")
        (if (null solution?)
            (progn (format T "Error!~%Nodes ~S and ~S are not connected in the provided graph.~%Aborting execution ...~%"
                           (ini-s dimacs-prob) (end-s dimacs-prob))
              (write-problem-stopped unsolved-probs-file prob-num algor tie-breaking ncosts goal-per))
          (progn 
            (format t "OK.")
            (format t "~%Calculating TC Lower Bound Table. Number of nodes: ~s" nnodes)
          ; change ncost attribute of problem to the one read from console
            (let* ((time-h-ini (get-internal-run-time))
                   (htable (tc-heuristic (end-s dimacs-prob) (ini-s dimacs-prob) dimacs-prob ncosts))
                   (ini-graph-params (get-initial-graph-params algor (ini-s dimacs-prob) ncosts tie-break-policy htable goals))
                   (time-start-solving (get-internal-run-time))
                   (time-h (float (/ (- time-start-solving time-h-ini) internal-time-units-per-second)))
                   (search-g (apply initial-graph-f ini-graph-params))  
                   (alg-params (get-algorithm-params algor search-g 
                                                     (ini-s dimacs-prob) (end-s dimacs-prob) dimacs-prob tie-break-policy htable goals)))
              (format t "~%Calculation finished in ~4,2f seconds." time-h)
              (format t "~%Algorithm ~S launched  ...~%" algor)
              (setf *execution-start* (get-internal-run-time))
              (multiple-value-bind (costs it stopped?) (apply alg alg-params)
                (setf t-end (get-internal-run-time))
                (write-stats stats-file prob-num algor tie-breaking ncosts
                             (float (/ (- t-end time-start-solving) internal-time-units-per-second)) 
                             it costs goals goal-per :time-h time-h)
                (write-solutions solutions-file prob-num algor tie-breaking ncosts costs goals goal-per)
                (when stopped? (write-problem-stopped stopped-probs-file prob-num algor tie-breaking ncosts (length costs) goal-per )))
              'EXECUTION-FINISHED-CORRECTLY)))))))
;(test-cases::run-dimacs-problem :VT2 1 3 :namoa-dr :LEX)

#|---------------------------------------------------------------------
Function called by deliver to process command line parameters, and execute the algorithm 
over the problem number read and writing a trace file with results.
-----------------------------------------------------------------------|#
(defun solve-dimacs-problem ()
  "Process command line parameters. If they are correct, the appropiate algorithm is called"
  (when system:*line-arguments-list*
      ; First of all, command line parameters are read
      (let* ((line          system:*line-arguments-list*)
             (algor         (when (car line) (read-from-string (nth 1 line) nil nil)))
             (tie-breaking  (when (nth 2 line) (read-from-string (nth 2 line) nil nil)))     ; :lexicographic :linear
             (map           (when (nth 3 line) (read-from-string (nth 3 line) nil nil)))     ; (:NY :VT2 ...)  
             (probn         (when (nth 4 line) (read-from-string (nth 4 line) nil nil)))     ; Problem #Number
             (ncosts        (when (nth 5 line) (read-from-string (nth 5 line) nil nil)))     ; Number of objectives (2 or 3)
             (goal-per      (when (nth 6 line) (read-from-string (nth 6 line) nil nil)))     ; goal1 percentage level 
             (goal-per2     (when (nth 7 line) (read-from-string (nth 7 line) nil nil)))     ; goal2 percentage level 
             (goals         (when goal-per (create-hash-goals (get-dimacs-goal-file map)))))
        (cond ((and probn map ncosts algor tie-breaking)
               (if goal-per ; lexgo, else -> namoa
                   (progn 
                     (format t "~%DIMACS problem: | ~S - ~S |, Number of Costs: | ~S |, to solve with: ~S - ~S, Goal perc.: (~S ~S), Goals: ~S.~%" 
                             map probn ncosts algor tie-breaking goal-per goal-per2 (gethash (list probn (list goal-per goal-per2)) goals))
                   (run-dimacs-problem map probn ncosts algor tie-breaking
                                       (gethash (list probn (list goal-per goal-per2)) goals) ; read goal from hash
                                       (list goal-per goal-per2))) ; goal percentage -> to write in trace file
                 (progn ; NAMOA
                   (format t "~%DIMACS problem: | ~S - ~S |, Number of Costs: | ~S |, to solve with: ~S - ~S.~%" 
                           map probn ncosts algor tie-breaking)
                 (run-dimacs-problem map probn ncosts algor tie-breaking))))
              (t (show-parameter-dimacs-info map probn ncosts algor tie-breaking))))))
