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

Description:   Functions to check algorithms correctness by reading 
               their solution files
------------------------------------------------------------------------
----------------------------------------------------------------------|#

(in-package :TEST-CASES)

#|---------------------------------------------------------------------
Function to check if two different versions of NAMOA* return the same
set of solutions to a set of problems. 
Finally, a resume of results is given
-----------------------------------------------------------------------|#
(defun check-namoa-solutions (&optional (file (capi:prompt-for-file "Trace with solutions of 1st version of NAMOA")))
  "Function to check if two different versions of NAMOA* return the same set of solutions to a set of problems"
  (let ((nprobs 0)
        (nprobs-failed 0)
        (namoa2-sols (make-hash-table :test #'equalp)))
    (with-open-file (fn (capi:prompt-for-file "Trace with solutions of 2nd version of NAMOA") :direction :input)
      (do ((item (read fn nil :eof) (read fn nil :eof)))
          ((eq :eof item))    
        (let* ((problem (cadr (assoc :t-prob item)))
               (c* (cadr (assoc :t-costs* item))))
          (setf (gethash problem namoa2-sols) c*))))
    (with-open-file (f file :direction :input)
      (do ((item (read f nil :eof) (read f nil :eof)))
          ((eq :eof item))    
        (let* ((problem (cadr (assoc :t-prob item)))
               (c* (sort (cadr (assoc :t-costs* item)) #'b-lex)))
          (let ((check-sol (equalp c* (sort (gethash problem namoa2-sols) #'b-lex))))
            (incf nprobs)
            (format t "Problem: ~A has ~A costs*~%" problem (length c*))
            (when (not (equalp check-sol T)) (incf nprobs-failed))
            (format t "Problem, Solution OK?: ~A~%" problem check-sol)))))
    (format t "~% Number of problems solved: ~A, from which ~A failed!!!~%" nprobs nprobs-failed)))

#|---------------------------------------------------------------------
Function to check all lexgo solutions to a set of problems. 
NAMOA* solutions file has to be given as well.
Finally, a resume of results is given
-----------------------------------------------------------------------|#
(defun check-lexgo-solutions (&optional (file (capi:prompt-for-file "Trace file with solutions of LEXGO*")))
  "Function to check all lexgo solutions to a set of problems. NAMOA* solutions file has to be given as well"
  (let ((nprobs 0)
        (nprobs-failed 0)
        (namoa-sols (make-hash-table :test #'equalp)))
    ; read from file namoa-solutions
    (with-open-file (fn (capi:prompt-for-file "Trace file with NAMOA solutions") :direction :input)
      (do ((item (read fn nil :eof) (read fn nil :eof)))
          ((eq :eof item))    
        (let* ((problem (cadr (assoc :t-prob item)))
               (c* (cadr (assoc :t-costs* item))))
          (setf (gethash problem namoa-sols) c*))))
    (with-open-file (f file :direction :input)
      (do ((item (read f nil :eof) (read f nil :eof)))
          ((eq :eof item))    
        (let* ((problem (cadr (assoc :t-prob item)))
               (perc (cadr (assoc :t-perc item)))
               (goals (cadr (assoc :t-goals item)))
               (c* (cadr (assoc :t-costs* item))))
          (multiple-value-bind (check-sol best-dev) 
                (check-solution-satisfy-goals (gethash problem namoa-sols) c* goals 2)
            (incf nprobs)
            (when (not (equalp check-sol T)) (incf nprobs-failed))
            (format t "Problem, Perc: (~A, ~A), Solution: ~A, d*: ~A~%" problem perc check-sol best-dev)
            ))))
    (format t "~% Number of problems solved: ~A, from which ~A failed!!!~%" nprobs nprobs-failed))) 

#|---------------------------------------------------------------------
Given the Pareto frontier, the solution returned by lexgo, 
problem goals and number of priority levels, the function returns T 
whenever the solutions returned are correct.  
Parameters:
- Pareto-frontier     Pareto frontier with all nondominated costs (list)
- Solution-returned   Solution returned by lexgo (list of goal-optimal vectors)
- Goals:              List with grouped goals
- Plevels:            Number of priority levels
-----------------------------------------------------------------------|#
(defun check-solution-satisfy-goals (pareto-frontier solution-returned goals plevels)
  "Auxiliary function of check-lexgo-solutions"
  (labels ((filter-vectors (c-dev c-solution)
             (cond ((null c-solution) nil)
                   ((b-lex c-dev (caar c-solution)) 
                    (filter-vectors c-dev (cdr c-solution)))
                   (T                                                                    ; not necessary to filter
                    (cons (car c-solution) (filter-vectors c-dev (cdr c-solution))))))
           (remove-dev-from-list (c-solution) ; each element is (#dev-vec #cost-vec) and the dev-vec is removed
             (let ((new nil)) 
               (dolist (c c-solution new)
                 (push (cadr c) new))))
           (insert-ordered (l e tie-breaking)
              (if (null l) 
                  (cons e l)
                (if (funcall tie-breaking e (car l))
                    (cons e l)
                  (cons (car l) (insert-ordered (cdr l) e tie-breaking)))))
           (order-lexicographically (cost-list)
             (let ((cost-list-ordered nil))
               (dolist (c cost-list cost-list-ordered)
                 (setf cost-list-ordered (insert-ordered cost-list-ordered c #'b-lex))))))
    (let ((c-solution nil) ; to store the solution 
          (best-dev-so-far (make-array plevels :initial-element most-positive-fixnum)))
      (dolist (c pareto-frontier)
        (let ((c-dev (deviation c goals)))
          (when (not (b-lex best-dev-so-far c-dev))
            (push (list c-dev c) c-solution)
            (when (b-lex c-dev best-dev-so-far) ; filter worse vectors already in csolution
              (setf best-dev-so-far c-dev) ; update the best deviation so far
              (setf c-solution (filter-vectors c-dev c-solution))))))
    ; check if the calculated solution and the solution returned by the algorithm and pass as parameter are equal.
      (setf c-solution (remove-dev-from-list c-solution))
      (setf c-solution (order-lexicographically c-solution))
      (setf solution-returned (order-lexicographically solution-returned))
      (if (equalp c-solution solution-returned) T
        (values c-solution best-dev-so-far)))))

