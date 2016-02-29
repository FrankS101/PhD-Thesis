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
               algorithm with the parameters given by console in grid problems
------------------------------------------------------------------------
----------------------------------------------------------------------|#

(in-package :TEST-CASES)

(defvar *goals-grids*)
(setf *goals-grids* (concatenate 'string cl-user::*files-path* "grids/goals files/goals3.txt"))

#|---------------------------------------------------------------------
Show an error whenever some of the parameters of the .exe file are not
correct (number or type of them)
-----------------------------------------------------------------------|#
(defun show-parameter-info (prob-num ncosts algor tie-breaking)
  (format t "~%WRONG PARAMETERS!~%")
  (format t "prob-num algor~%")
  (format t "~%prob-num  (~S)  -  #Problem to solve" prob-num)
  (format t "~%ncosts    (~S)  -  #Number of costs to consider (2, 3, 4, 5)" ncosts)
  (format t "~%algor     (~S)  -  Algorithm to employ (:NAMOA, :NAMOA-TE, :LEXGO, :LEXGO-TE) " algor)
  (format t "~%tie-breaking   (~S)  -  Tie-breaking policy (:LEX, :LIN, :LEX-LINEAR) " tie-breaking)
  (sleep 10))

#|---------------------------------------------------------------------
Function to return the appropiate function to call the desired algorithm
Parameter:
- algor: keyword with the algorithm read from console input
-----------------------------------------------------------------------|#
(defun get-algorithm-function (algor ncosts)
  "Return a function object to call the desired algorithm"
  (cond 
   ((and (equalp algor :namoa) (= ncosts 2))     #'namoa2)
   ((equalp algor :namoa)                        #'namoa)
   ((and (equalp algor :namoa-dr) (= ncosts 3))  #'namoa-dr-3)
   ((and (equalp algor :namoa-dr) (> ncosts 3))  #'namoa-dr)
   ((equalp algor :lexgo)                        #'lexgo)
   ((equalp algor :lexgo-dr)                     #'lexgo-dr)
   (T (format t "~%ERROR! Algorithm provided: (~S) unknown.~%" algor) (abort))))

#|---------------------------------------------------------------------
Function to return a list of parameters to call a function to create 
an initial multiobjective search graph depending on the algorithm given 
Parameters:
- algor: keyword with the algorithm read from console input
- search-g: multiobjective search graph
- start: start node
- goal: goal node
- grid: grid problem
- tie-breaking 
- htable: heuristic table
- goals: to be used by lexgo or lexgo-dr
-----------------------------------------------------------------------|#
(defun get-algorithm-params (algor search-g start goal grid tie-breaking htable goals)
  "Since each algorithm is called with a different set of parameters, this functions returns these parameters in a list"
  (cond 
   ((equalp algor :namoa)         (list search-g goal grid start htable tie-breaking))
   ((equalp algor :namoa-dr)      (list search-g goal grid start htable tie-breaking))
   ((and (equalp algor :lexgo)    (equalp tie-breaking #'b-lex))     
                                  (list search-g goal grid start goals htable tie-breaking tie-breaking))
   ((and (equalp algor :lexgo)    (equalp tie-breaking #'b-lex-linear))     
                                  (list search-g goal grid start goals htable #'b-lin tie-breaking))
   ((equalp algor :lexgo-dr)      (list search-g goal grid start goals htable tie-breaking))
   (T (format t "~%ERROR! Algorithm provided: (~S) unknown.~%" algor) (abort))))

#|---------------------------------------------------------------------
Function to return the appropiate function with the tie-breaking policy
Parameter:
- tie-breaking: keyword with the tie-breaking read from console input
-----------------------------------------------------------------------|#
(defun get-tie-breaking-function (tie-breaking)
  "Return a function object to use an algorithm with the desired tie-breaking policy"
  (cond 
   ((equalp tie-breaking :LEX)           #'b-lex)
   ((equalp tie-breaking :LIN)           #'b-lin)
   ((equalp tie-breaking :LEX-LINEAR)    #'b-lex-linear)
   (T (format t "~%ERROR! Tie-breaking policy provided: (~S) unknown.~%" tie-breaking) (abort))))

#|---------------------------------------------------------------------
Function to return the trace file path depending on the parameter.
Parameter:
- alg: keyword with the algorithm read from console input
-----------------------------------------------------------------------|#
(defun get-solutions-file-path (alg tie-breaking)
  "Return the name of the trace file (string) according to the algorithm given by parameter"
  (cond 
   ((and (equalp alg :namoa) (equalp tie-breaking :LEX))             "namoa-lex-solutions.txt")
   ((and (equalp alg :namoa) (equalp tie-breaking :LIN))             "namoa-lin-solutions.txt")
   ((equalp alg :namoa-dr)                                           "namoa-dr-solutions.txt")
   ((and (equalp alg :lexgo) (equalp tie-breaking :LEX))             "lexgo-lex-solutions.txt")
   ((and (equalp alg :lexgo) (equalp tie-breaking :LEX-LINEAR))      "lexgo-lin-solutions.txt")
   ((equalp alg :lexgo-dr)                                           "lexgo-dr-solutions.txt")
   (T (format t "~%ERROR! Algorithm provided: (~S) unknown.~%" alg) (abort))))

#|---------------------------------------------------------------------
Function to return the trace file path depending on the parameter.
Parameter:
- alg: keyword with the algorithm read from console input
-----------------------------------------------------------------------|#
(defun get-stats-file-path (alg tie-breaking)
  "Return the name of the trace file (string) according to the algorithm given by parameter"
  (cond 
   ((and (equalp alg :namoa) (equalp tie-breaking :LEX))             "namoa-lex-stats.txt")
   ((and (equalp alg :namoa) (equalp tie-breaking :LIN))             "namoa-lin-stats.txt")
   ((equalp alg :namoa-dr)                                           "namoa-dr-stats.txt")
   ((and (equalp alg :lexgo) (equalp tie-breaking :LEX))             "lexgo-lex-stats.txt")
   ((and (equalp alg :lexgo) (equalp tie-breaking :lex-linear))      "lexgo-lin-stats.txt")
   ((equalp alg :lexgo-dr)                                           "lexgo-dr-stats.txt")
   (T (format t "~%ERROR! Algorithm provided: (~S) unknown.~%" alg) (abort))))

#|---------------------------------------------------------------------
Function to return an object function to create the initial multiobjective
search graph depending on the algorithm given 
Parameter:
- algor: keyword with the algorithm read from console input
-----------------------------------------------------------------------|#
(defun get-initial-multiobjective-seach-graph (algor ncosts)
  "Return a function object to create the initial multiobjective search graph for a given algorithm"
  (cond 
   ((equalp algor :namoa)         #'new-sgraph-namoa)
   ((and (equalp algor :namoa-dr) 
         (= ncosts 3))            #'new-sgraph-namoa-dr-3)
   ((and (equalp algor :namoa-dr) 
         (> ncosts 3))            #'new-sgraph-namoa-dr)
   ((equalp algor :lexgo)         #'new-sgraph-lexgo)
   ((equalp algor :lexgo-dr)      #'new-sgraph-lexgo-dr)
   (T (format t "~%ERROR! Algorithm provided: (~S) unknown.~%" algor) (abort))))

#|---------------------------------------------------------------------
Function to return a list of parameters to call a function to create 
an initial multiobjective search graph depending on the algorithm given 
Parameters:
- algor: keyword with the algorithm read from console input
- s: initial-state
- nc: number of costs
- tie-breaking 
- htable: heuristic table
- goals: goals for lexgo algorithm
- open-l-initial-size: to define the initial size of the open list
-----------------------------------------------------------------------|#
(defun get-initial-graph-params (algor s nc tie-breaking htable goals)
  "Since each algorithm need a different set of parameters to create an initial search graph, this functions returns these parameters in a list"
  (cond 
   ((equalp algor :namoa)      (list s nc tie-breaking htable))
   ((equalp algor :namoa-dr-3) (list s nc tie-breaking htable))
   ((equalp algor :namoa-dr)   (list s nc tie-breaking htable))
   ((equalp algor :lexgo)      (list s nc tie-breaking htable goals))
   ((equalp algor :lexgo-dr)   (list s nc tie-breaking htable goals))
   (T (format t "~%ERROR! Algorithm provided: (~S) unknown.~%" algor) (abort))))

#|---------------------------------------------------------------------
Function to run a grid problem and write to file the stats of the execution
-----------------------------------------------------------------------|#
(defun run-grid-problem (prob-num ncosts algor tie-break-policy &optional goals goal-per)
  "Execute prob-num considering ncosts with algor employing tie-break-policy. When lexgo is used, the optional parameters must be given"
  (let* ((alg              (get-algorithm-function algor ncosts))
         (file-problem     (concatenate 'string *Problems-path* (write-to-string ncosts) "/Problem" (write-to-string prob-num) ".txt"))
         (file-grid        (concatenate 'string *Grids-path* "Grid4.txt")) ;(write-to-string prob-num) ".txt"))
         (grid             (read-grid-problem file-problem file-grid))
         (tie-breaking     (get-tie-breaking-function tie-break-policy))
         (initial-graph-f  (get-initial-multiobjective-seach-graph algor ncosts))
         (stats-file       (concatenate 'string cl-user::*root* (get-stats-file-path algor tie-break-policy)))
         (solutions-file   (concatenate 'string cl-user::*root* (get-solutions-file-path algor tie-break-policy)))
         (stopped-probs-file (concatenate 'string cl-user::*root* "stopped-problems.txt"))
         (t-end nil))
    (format t "~%Processing problem number ~A ..." prob-num)
    (format t "~%Calculating table of distance estimates ...")
    (let* ((time-h-ini (get-internal-run-time))
           (htable (tc-heuristic (goal grid) (start grid) grid (nc grid) T))
           (ini-graph-params (get-initial-graph-params algor (start grid) (nc grid) tie-breaking htable goals))
           (time-start-solving (get-internal-run-time))
           (time-h (float (/ (- time-start-solving time-h-ini) internal-time-units-per-second)))
           (search-g (apply initial-graph-f ini-graph-params))  
           (alg-params (get-algorithm-params algor search-g (start grid) (goal grid) grid tie-breaking htable goals)))
      (format t "~%Calculation finished in ~4,2f seconds." time-h)
      (format t "~%Algorithm ~S launched  ...~%" algor)
      (multiple-value-bind (costs it stopped?) (apply alg alg-params)
        (setf t-end (get-internal-run-time))
        (let ((exe-time (float (/ (- t-end time-start-solving) internal-time-units-per-second))))
          (format t "Execution finished in ~5,2f seconds.~%~%" exe-time)
          (write-stats stats-file prob-num algor tie-break-policy (nc grid) exe-time
                       it costs goals goal-per :time-h time-h)
          (write-solutions solutions-file prob-num algor tie-break-policy (nc grid) costs goals goal-per)
          (when stopped? (write-problem-stopped stopped-probs-file prob-num algor tie-breaking (nc grid) goal-per))
          (reset-stats))))
      'EXECUTION-FINISHED-CORRECTLY))
;(test-cases::run-grid-problem 1 3 :namoa :LEX)
;(test-cases::run-grid-problem 1 3 :lexgo :LEX '(((252 0.5) (268 0.5)) ((233 1))) '(1 1))

#|--------------------------------------------------------------------------------------------------------
Function called by deliver to process command line parameters, and execute the algorithm 
over the problem number read and writing a trace file with results.
--------------------------------------------------------------------------------------------------------|#
(defun solve-grid-problem ()
  "Process command line parameters. If they are correct, the appropiate algorithm is called"
  (when system:*line-arguments-list*
      ; First of all, command line parameters are read
      (let* ((line      system:*line-arguments-list*)
             (algor         (when (car line) (read-from-string (nth 1 line) nil nil)))
             (tie-breaking  (when (nth 2 line) (read-from-string (nth 2 line) nil nil)))
             (probn         (when (nth 3 line) (read-from-string (nth 3 line) nil nil)))
             (ncosts        (when (nth 4 line) (read-from-string (nth 4 line) nil nil)))
             (goal-per      (when (nth 5 line) (read-from-string (nth 5 line) nil nil)))       ; goal percentage level 1
             (goal-per2     (when (nth 6 line) (read-from-string (nth 6 line) nil nil)))       ; goal percentage level 2
             (goals         (create-hash-goals *goals-grids*)))  ; AQUI
        (cond ((and algor tie-breaking probn ncosts)
               (if goal-per ; lexgo, else -> namoa
                   (progn 
                     (format t "~%Grid problem: | ~S |, Number of Costs: | ~S |, to solve with: ~S - ~S, Goal perc.: (~S ~S), Goals: ~S.~%" 
                             probn ncosts algor tie-breaking goal-per goal-per2 (gethash (list probn (list goal-per goal-per2)) goals))
                     (run-grid-problem probn ncosts algor tie-breaking
                                       (gethash (list probn (list goal-per goal-per2)) goals)  ; read goal from hash
                                       (list goal-per goal-per2)))                             ; goal percentage -> to write in trace file
                 (progn ; NAMOA*
                   (format t "~%Grid problem: | ~S |, Number of Costs: | ~S |,  to solve with: ~S - ~S.~%" probn ncosts algor tie-breaking)
                   (run-grid-problem probn ncosts algor tie-breaking))))
              (t (show-parameter-info probn ncosts algor tie-breaking))))))
