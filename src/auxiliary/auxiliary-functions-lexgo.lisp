#|-----------------------------------------------------------------
-------------------------------------------------------------------
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

Description:   Functions to create manage goals in LEXGO algorithm
-------------------------------------------------------------------
------------------------------------------------------------------|#

(in-package :LEXGO)

#|---------------------------------------------------------------------
Sereval functions to return vectors (d,f,g) from a given label.
In order to extract these vectors from the label, it is necessary to specify
the number of priority levels and attributes
Parameters:
- label:    Label to extract vectors from
- nlevels:  Number of priority levels that given goals are grouped
- ncosts:   Number of attributes.
-----------------------------------------------------------------------|#
(defun vd (label nlevels) (subseq (car label) 0 nlevels))
(defun vf (label nlevels ncosts) (subseq (car label) nlevels (+ nlevels ncosts)))
(defun vg (label nlevels ncosts) (subseq (car label) (+ nlevels ncosts) (+ nlevels (* ncosts 2))))

#|---------------------------------------------------------------------
Function to return a label (vector) with the concatenation of vectors
d, f and g.
Parameters:
- d:  deviation vector
- f:  estimate vector
- g:  cost vector
-----------------------------------------------------------------------|#
(defun create-label (d f g)
  "create a label from vectors d, f and g"
  (concatenate 'vector d f g))

#|---------------------------------------------------------------------
Function to read from a descriptor file all problem goals and store them
in a hash table. Each (problem percentage) has a list with the associated
goals.
Parameters:
- goals-file: open descriptor file with all problem goals for each problem-perc
-----------------------------------------------------------------------|#
(defun create-hash-goals (goals-file)
  "Function to read from a descriptor file all problem goals and store them
in a hash table"
  (let ((htable (make-hash-table :test #'equalp)))
    (with-open-file (f goals-file :direction :input)
      (do ((item (read f nil :eof) (read f nil :eof)))
          ((eq :eof item))    
        (let ((problem (cadr (assoc :t-prob item)))
              (perc (cadr (assoc :t-perc item)))
              (goals (cadr (assoc :t-goals item))))
          (setf (gethash (list problem perc) htable) goals))))
    htable))

#|---------------------------------------------------------------------
Function to create a file with the goals calculated from a NAMOA trace file.
For each problem the following goals in two levels are calculated:
Both levels have equal rates
(0 0) (0.25 0.25) (0.5 0.5) (0.75 0.75) (1 1)
First level has a fixed rate and second varies 
(0.5 0.125) (0.5 0.25) (0.5 0.375) 
(0.75 0.1875) (0.75 0.375) (0.75 0.5675) 
-----------------------------------------------------------------------|#
(defun create-goals-file (&optional (file (capi:prompt-for-file "File with NAMOA trace"))
                                    (file-out (capi:prompt-for-file "File to write")))
  "Create a file with the goals calculated from a NAMOA trace file"
  (progn
    (calculate-goals-from-pareto-frontier-by-level '(0) '(0) 1 file file-out)
    (calculate-goals-from-pareto-frontier-by-level '(0.25) '(1) 1 file file-out)
    (calculate-goals-from-pareto-frontier-by-level '(0.5 0.75) '(0.25 0.5 0.75 1) 1 file file-out)
    (calculate-goals-from-pareto-frontier-by-level '(1) '(1) 1 file file-out)))

#|---------------------------------------------------------------------
Function to create a file with the goals calculated from a NAMOA trace file.
For each problem the following goals in two levels are calculated:
Both levels have equal rates
(0 0) (0.125 0.125) (0.25 0.25) (0.375 0.375) (0.5 0.5)
One level has a fixed rate and the other varies 
(1 0.125) (1 0.25) 
(0.125 1) (0.25 1) 
-----------------------------------------------------------------------|#
(defun create-goals-file2 (&optional (file (capi:prompt-for-file "File with NAMOA trace"))
                                    (file-out (capi:prompt-for-file "File to write")))
  "Create a file with the goals calculated from a NAMOA trace file"
  (progn
    (calculate-goals-from-pareto-frontier-by-level '(0) '(0) 1 file file-out)
    (calculate-goals-from-pareto-frontier-by-level '(0.125) '(1) 1 file file-out)
    (calculate-goals-from-pareto-frontier-by-level '(0.25) '(1) 1 file file-out)
    (calculate-goals-from-pareto-frontier-by-level '(0.375) '(1) 1 file file-out)
    (calculate-goals-from-pareto-frontier-by-level '(0.5) '(1) 1 file file-out)
    (calculate-goals-from-pareto-frontier-by-level '(1) '(0.125 0.25) 1 file file-out)
    (calculate-goals-from-pareto-frontier-by-level '(0.125 0.25) '(1) 2 file file-out)))
                                           
#|---------------------------------------------------------------------
To create a file with goals for LEXGO problems:
By default the list of goals has two priority levels, with two targets in the first
level and one target in the second level, e.g. '(((10 0.5) (10 0.5)) ((10 1)))
Parameters:
- perc-list1: list with percentages for first priority level
- perc-list2: list with percentages for second priority level
-----------------------------------------------------------------------|#
(defun calculate-goals-from-pareto-frontier-by-level (perc-list1 perc-list2 fixed
                                                                 &optional (file (capi:prompt-for-file "File with NAMOA trace"))
                                                                 (file-out (capi:prompt-for-file "File to write")))
  "A file to write the different goals for problems read from file"
    (with-open-file (fout file-out :direction :output :if-exists :append :if-does-not-exist :create)
      (with-open-file (f file :direction :input)
        (do ((item (read f nil :eof) (read f nil :eof)))
            ((eq :eof item))    
          (let ((problem (cadr (assoc :t-prob item)))
                (c* (cadr (assoc :t-costs* item))))
            (multiple-value-bind (ideal nadir)
                (get-ideal-and-nadir c* (length (car c*)))
              (dolist (p perc-list1)
                (dolist (p2 perc-list2)
                  (let* ((nadir-ideal (sub-v nadir ideal))
                         (goal-v0 (floor 
                                   (+ (aref ideal 0) (* p (aref nadir-ideal 0)))))
                         (goal-v1 (floor 
                                   (+ (aref ideal 1) (* p (aref nadir-ideal 1)))))
                         (goal-v2 (floor 
                                   (+ (aref ideal 2) (* p p2 (aref nadir-ideal 2))))))
                    (format fout "((:T-PROB ~S) (:T-PERC ~S) (:T-GOALS ~S))~%"
                            problem 
                            (if (= fixed 1) (list p (* p p2))
                              (if (= fixed 2) (list (* p p2) p2)))
                            (list (list (list goal-v0 0.5)
                                        (list goal-v1 0.5))
                                  (list (list goal-v2 1)))))))))))))

#|---------------------------------------------------------------------
Function to calculate from Pareto frontier and goals, which costs are goal-optimal
-----------------------------------------------------------------------|#
(defun get-goal-optimal-costs (&optional (file (capi:prompt-for-file "Trace with solutions of LEXGO*")))
  "To calculate from Pareto frontier and goals, which costs are goal-optimal"
  (labels ((filter-vectors (c-dev c-solution)
             (cond ((null c-solution) nil)
                   ((b-lex c-dev (caar c-solution)) 
                    (filter-vectors c-dev (cdr c-solution)))
                   (T ; not necessary to filter
                    (cons (car c-solution) (filter-vectors c-dev (cdr c-solution)))))))
    (let ((namoa-sols (make-hash-table :test #'equalp)))
      (with-open-file (fn (capi:prompt-for-file "NAMOA solutions file") :direction :input)
        (do ((item (read fn nil :eof) (read fn nil :eof)))
            ((eq :eof item))    
          (let* ((problem (cadr (assoc :t-prob item)))
                 (c* (cadr (assoc :t-cost item))))
            (setf (gethash problem namoa-sols) c*))))
      (with-open-file (f file :direction :input)
        (do ((item (read f nil :eof) (read f nil :eof)))
            ((eq :eof item))    
          (let* ((problem (cadr (assoc :t-prob item)))
                 (perc (cadr (assoc :t-perc item)))
                 (goals (cadr (assoc :t-goals item)))
                 (goal-optimal-costs) 
                 (dev #(0 0))
                 (goals-vec (make-array 3 :initial-contents 
                                        (list (car (car (car goals))) (car (cadr (car goals)))  (car (car (cadr goals)))))))
            (let ((time-ini (get-internal-run-time)))
              (dolist (cost* (gethash problem namoa-sols))
                (when (dominates cost* goals-vec)
                  (push cost* goal-optimal-costs)))
              (when (null goal-optimal-costs)              ; there is some vector which satisfy goals-> dev=(0,0), otherwise calculate dev
                (setf dev (make-array 2 :initial-element most-positive-fixnum))
                (dolist (cost* (gethash problem namoa-sols))
                  (let ((c-dev (deviation cost* goals)))
                    (when (not (b-lex dev c-dev))
                      (push (list c-dev cost*) goal-optimal-costs)
                      (when (b-lex c-dev dev)              ; filter worse vectors already in csolution
                        (setf dev c-dev)                   ; update the best deviation so far
                        (setf goal-optimal-costs (filter-vectors c-dev goal-optimal-costs)))))))     
              (format T "Problem: ~A, Perc: ~A, Deviation: ~A, COSTS*: ~A. Calculation of c* costs which satisfy goals was: ~A~%" 
                      problem perc dev (length goal-optimal-costs)
                      (float (/ (- (get-internal-run-time) time-ini) internal-time-units-per-second))))))))))
; NOTE: Time to calculate that is negligible!
