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

Description:   Trace files management (read/write from/to disk)
------------------------------------------------------------------------
----------------------------------------------------------------------|#

(in-package :TEST-CASES)

(defvar *Grids-path*)
(setq *Grids-path* (concatenate 'string cl-user::*files-path* "grids/benchmarks/grids/"))

(defvar *Problems-path*)
(setq *Problems-path* (concatenate 'string cl-user::*files-path* "grids/benchmarks/problems/"))

#|---------------------------------------------------------------------
Function to write results to file
Parameters:
- file-out
- prob-num
- algor: algorithm (keyword)
- tie-breaking (keyword)
- ncosts: number of objectivesx
- exe-time: total execution time in seconds
- it: iterations of the algorithm
- costs: number of optimal costs returned
- goals: integer values which represent the goals 
- goal-per: percentage of the goals with respect to ideal and nadir points 
for each priority level, i.e: (0.5 0.75)
-----------------------------------------------------------------------|#
(defun write-stats (file-out prob-num algor tie-breaking ncosts exe-time it costs           
                                          &optional goals goal-per &key time-h)
  "Write results of algorithm execution to file"
  (cond ((equalp algor :namoa-dr)
         (write-stats-namoa-dr file-out prob-num algor tie-breaking ncosts exe-time it costs time-h))
        ((equalp algor :namoa)
         (write-stats-namoa file-out prob-num algor tie-breaking ncosts exe-time it costs time-h))
        ((equalp algor :lexgo)
         (write-stats-lexgo file-out prob-num algor tie-breaking ncosts exe-time it costs time-h goals goal-per))
        ((equalp algor :lexgo-dr)
         (write-stats-lexgo-dr file-out prob-num algor tie-breaking ncosts exe-time it costs time-h goals goal-per))
        (T (format T "Error!! Given algorithm to write trace file is not correct.~%"))))

(defun write-solutions (file-out prob-num algor tie-breaking ncosts costs &optional goals goal-per)
  "Write optimal vectors resulting of algorithm execution to file"
  (cond ((or (equalp algor :namoa) (equalp algor :namoa-dr))
         (write-solutions-namoa file-out prob-num algor tie-breaking ncosts costs))
        ((or (equalp algor :lexgo) (equalp algor :lexgo-dr))
         (write-solutions-lexgo file-out prob-num algor tie-breaking ncosts costs goals goal-per))
        (T (format T "Error!! Given algorithm to write trace file is not correct.~%"))))

#|---------------------------------------------------------------------
Several functions necessary since each algorithm keep different stats
-----------------------------------------------------------------------|#
(defun write-stats-namoa-dr (file-out prob-num algor tie-breaking ncosts exe-time it costs time-h)
  (with-open-file (f file-out :direction :output :if-exists :append :if-does-not-exist :create)
    "Write results of NAMOA_dr execution to file"
    (format f "((:T-PROB ~S) (:T-ALG ~S) (:T-TIE ~S) (:T-NC ~S) (:T-TTOT ~A) (:T-TIME-H ~A) (:T-IT ~A) (:T-ND-CL2 ~A) (:T-COSTS* ~A) (:T-COSTS2 ~A) (:T-MAX-OP ~A) (:T-FILT ~A) (:T-PRUN-CL ~A) (:T-PRUN-OP ~A) (:T-EQ ~A))~%" 
            prob-num algor tie-breaking ncosts exe-time time-h it
            *t-gcl* (length costs) *t-costs* *max-open* *tot-filtered* 
            *tot-pruned-cl* *tot-pruned-op* *tot-eq*)))

(defun write-stats-namoa (file-out prob-num algor tie-breaking ncosts exe-time it costs time-h)
  (with-open-file (f file-out :direction :output :if-exists :append :if-does-not-exist :create)
    "Write results of NAMOA execution to file"
    (format f "((:T-PROB ~S) (:T-ALG ~S) (:T-TIE ~S) (:T-NC ~S) (:T-TTOT ~A) (:T-TIME-H ~A) (:T-IT ~A) (:T-COSTS* ~A) (:T-FILT-COMP ~A) (:T-PRUN-COMP ~A) (:T-MAX-OP ~A) (:T-FILT ~A) (:T-PRUN-CL ~A) (:T-PRUN-OP ~A) (:T-EQ ~A))~%" 
            prob-num algor tie-breaking ncosts exe-time time-h it
            (length costs) *filt-comp* *prun-comp* *max-open* *tot-filtered* 
            *tot-pruned-cl* *tot-pruned-op* *tot-eq*)))

(defun write-stats-lexgo (file-out prob-num algor tie-breaking ncosts exe-time it costs time-h          
                                      &optional goals goal-per)
  (with-open-file (f file-out :direction :output :if-exists :append :if-does-not-exist :create)
    "Write results of LEXGO execution to file"
    (format f "((:T-PROB ~S) (:T-ALG ~S) (:T-TIE ~S) (:T-NC ~S) (:T-PERC ~S) (:T-GOALS ~A) (:T-TTOT ~A) (:T-TIME-H ~A) (:T-IT ~A) (:T-COSTS* ~A) (:T-FILT-COMP ~A) (:T-PRUN-COMP ~A) (:T-MAX-OP ~A) (:T-FILT ~A) (:T-PRUN-CL ~A) (:T-PRUN-OP ~A) (:T-EQ ~A) (:T-FILT-DEV ~A) (:T-PRUN-DEV ~A) (:T-FILT-DEV-COMP ~A) (:T-PRUN-DEV-COMP ~A))~%" 
            prob-num algor tie-breaking ncosts goal-per goals exe-time time-h it (length costs) 
            *filt-comp* *prun-comp* *max-open* *tot-filtered* *tot-pruned-cl* 
            *tot-pruned-op* *tot-eq* *tot-dev-filtered* *tot-dev-pruned* 
            *dev-filt-comp* *dev-prun-comp*)))

(defun write-stats-lexgo-dr (file-out prob-num algor tie-breaking ncosts exe-time it costs time-h          
                                      &optional goals goal-per)
  (with-open-file (f file-out :direction :output :if-exists :append :if-does-not-exist :create)
    "Write results of LEXGO_dr execution to file"
    (format f "((:T-PROB ~S) (:T-ALG ~S) (:T-TIE ~S) (:T-NC ~S) (:T-PERC ~S) (:T-GOALS ~A) (:T-TTOT ~A) (:T-TIME-H ~A) (:T-IT ~A) (:T-ND-CL2 ~A) (:T-COSTS* ~A) (:T-COSTS2 ~A) (:T-REGULAR-PRUN ~A) (:T-TIME-EF-PRUN ~A) (:T-MAX-OP ~A) (:T-FILT ~A) (:T-PRUN-CL ~A) (:T-PRUN-OP ~A) (:T-EQ ~A) (:T-FILT-DEV ~A) (:T-PRUN-DEV ~A) (:T-FILT-DEV-COMP ~A) (:T-PRUN-DEV-COMP ~A))~%" 
            prob-num algor tie-breaking ncosts goal-per goals exe-time time-h it *t-gcl* (length costs) 
            *t-costs* *regular-prun* *time-ef-prun* *max-open* *tot-filtered* 
            *tot-pruned-cl* *tot-pruned-op* *tot-eq* *tot-dev-filtered* *tot-dev-pruned* 
            *dev-filt-comp* *dev-prun-comp*)))

(defun write-solutions-namoa (file-out prob-num algor tie-breaking ncosts costs)
  "Write optimal vectors resulting of NAMOA execution to file"
  (with-open-file (f file-out :direction :output :if-exists :append :if-does-not-exist :create)
    (format f "((:T-PROB ~S) (:T-ALG ~S) (:T-TIE ~S) (:T-NC ~S) (:T-COSTS* ~A))~%" 
            prob-num algor tie-breaking ncosts costs)))

#|
(defun write-solutions-namoa (file-out prob-num algor tie-breaking ncosts costs)
  "Write optimal vectors resulting of NAMOA execution to file"
  (declare (ignore tie-breaking) (ignore algor))
  (setf costs (reverse costs))
  (with-open-file (f file-out :direction :output :if-exists :append :if-does-not-exist :create)
    (format f "~A ~A~%~A~%" prob-num ncosts (length costs))
    (loop for c in costs do
          (dotimes (n ncosts)
            (format f "~A " (aref c n)))
          (format f "~%"))))
|#

(defun write-solutions-lexgo (file-out prob-num algor tie-breaking ncosts costs         
                                      &optional goals goal-per)
  "Write optimal vectors resulting of LEXGO execution to file"
  (with-open-file (f file-out :direction :output :if-exists :append :if-does-not-exist :create)
    (format f "((:T-PROB ~S) (:T-ALG ~S) (:T-TIE ~S) (:T-NC ~S) (:T-PERC ~S) (:T-GOALS ~A) (:T-COSTS* ~A))~%" 
            prob-num algor tie-breaking ncosts goal-per goals costs)))


#|---------------------------------------------------------------------
Write to a file problems that reached the execution time limit
-----------------------------------------------------------------------|#
(defun write-problem-stopped (file-out prob-num algor tie-breaking ncosts costs &optional goal-per)
  "Write the given prob-num to a file of problems which reach the execution time limit"
  (with-open-file (f file-out :direction :output :if-exists :append :if-does-not-exist :create)
    (format f "(:T-PROB ~S) (:T-ALG ~S) (:T-TIE ~S) (:T-NC ~S) (:T-PERC ~S) (:T-COSTS* ~A)~%" 
            prob-num algor tie-breaking ncosts goal-per costs)))
