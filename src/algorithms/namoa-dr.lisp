#|---------------------------------------------------------------------
-----------------------------------------------------------------------
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

Description:   Implementation of NAMOA*_dr algorithm
-----------------------------------------------------------------------
---------------------------------------------------------------------|#

(in-package :namoa-dr)

#|---------------------------------------------------------------------
                 NAMOA*_dr algorithm (general version to use with q > 3 objectives)
Parameters:
- sgraph:        MO search graph 
- gamma:         Goal state
- problem:       Problem to solve by LEXGO*
- s:             Initial state
- htable:        Table with precalculated heuristic values
- tie-breaking:  Tie-breaking policy
- it:            Label expansions
-----------------------------------------------------------------------|#
(defun namoa-dr (sgraph gamma problem s htable tie-breaking &optional (it 0))
  "NAMOA*_dr algorithm (general version for n objectives)"
  (let* ((label (best-open-label sgraph tie-breaking)))
    (cond ((not label)
           (setf *t-costs* (length (t-costs sgraph)))
           (maphash #'(lambda (k v)
                        (declare (ignore k))
                        (incf *t-gcl* (length (t-gcl v))))
                    (nodes-table sgraph))
           (when *show-stats* (show-stats-namoa-dr sgraph it))
           (values (costs* sgraph) it))
          ((equal? (l-state label) gamma) 
           (store-solution sgraph label)
           (namoa-dr sgraph gamma problem s htable tie-breaking (1+ it)))   
          ((> (/ (- (get-internal-run-time) *execution-start*) internal-time-units-per-second)
              *time-limit*)
           (when *show-stats* (show-stats-namoa-dr sgraph it))
           (values (costs* sgraph) it T))
          (T 
           (let* ((e (l-state label))
                  (node (gethash (key e) (nodes-table sgraph)))
                  (g (vec (car (g-cl node)))))
             (dolist (e2 (expand (l-state label) problem))
               (let ((g2 (add-v g (cost e e2 problem))))
                 (if (in-graph? e2 sgraph)
                     (update-node sgraph e2 g2 (h e2 htable) tie-breaking :parent e)
                   (new-node sgraph e2 g2 (h e2 htable) :parent e ))))
             (namoa-dr sgraph gamma problem s htable tie-breaking (1+ it)))))))

#|----------------------------------------------------------------------------
Function to create a new NAMOA*_dr search graph (general version for n objectives)
Parameters:
- s:                 initial state
- ncosts:            number of criteria
- tie-breaking:      tie-breaking policy to choose the best label from OPEN
- htable:            heuristic table
- size op:           open list initial size (depends on the available information 
                     about the problem
-----------------------------------------------------------------------------|#
(defun new-sgraph-namoa-dr (s ncosts tie-breaking htable &optional size-op)
  "Function to create a new NAMOA* search graph with N objectives"
  (let* ((h-value (h s htable))
         (c-vector (make-instance 'c-vector-f 
                                  :vec (make-vector0 ncosts)
                                  :vec-f h-value
                                  :parent-list (list nil))))
  (create-generic-sgraph (make-instance 'mo-graph-te :s s) (make-instance 'node-te) 
                         s ncosts tie-breaking htable h-value c-vector size-op)))

#|---------------------------------------------------------------------
Move a label from G_op(node) to G_cl(node).
This method is called after a call to namoa::close-label to insert the new
truncated closed cost vector and filter the ones that are dominated by it
Parameters:
- label:   Label to move from G_op to G_cl
- node:    Node with information regarding the label
- open-l:  Queue of OPEN alternatives
-----------------------------------------------------------------------|#
(defmethod close-label-n (label (node node-te) open-l)
  "insert truncated vector and filter new dominated ones"
  (setf (t-gcl node) 
        (insert-and-filter-t-costs-n (t-gcl node) (trunc-v (f label)))))

#|---------------------------------------------------------------------
Method to update T(C*). This method is called after a call to namoa::store-solution 
Parameters:
- sgraph:        MO search graph
- label:         label used to introduce a new T(c*) cost vector and filter the ones
                 already in T(C*) and now dominated by it
-----------------------------------------------------------------------|#
(defmethod store-solution-n ((sgraph mo-graph-te) label) 
  "Update T(C*)" 
  (setf (t-costs sgraph) 
        (insert-and-filter-t-costs-n (t-costs sgraph) (trunc-v (f label)))))

#|---------------------------------------------------------------------
Method to create a new node with the label (g+vec-h, state)
Parameters:
- sgraph:        MO search graph
- state:         problem state
- g:             cost of the new path
- vec-h:         estimate vector of the new path
- parent:        parent of the new path
-----------------------------------------------------------------------|#
(defmethod new-node ((sgraph mo-graph-te) state g vec-h &key parent)
  "Method to create a new node with the label (g+vec-h, state)"
  (let ((estim (not-dominated-by-TC*? (add-v g vec-h) (t-costs sgraph) (costs* sgraph))))
    (if (null estim)
        (incf *tot-filtered*) 
      (create-new-node sgraph (make-instance 'node-te) state estim g vec-h parent))))

#|---------------------------------------------------------------------
Function to update an existing node in the search graph.
If f(state) is not dominated by any known Pareto solution the label is considered

Note:
It is not necessary to check that g dominates or equals some closed label
of Node-labels, since we consider a consistent heuristic is employed.

Parameters:
- sgraph:        MO search graph 
- state:         explored state
- g:             g(state)
- vec-h:         h(state)
- tie-breaking:      tie-breaking policy to choose the best label from OPEN
- parent:        predecessor(state)
-----------------------------------------------------------------------|#
(defmethod update-node ((sgraph mo-graph-te) state g vec-h tie-breaking &key parent) 
  "Function to update an existing node in the search graph"
  (let ((estim (not-dominated-by-TC*? (add-v g vec-h) (t-costs sgraph) (costs* sgraph))))
    (if (null estim)
        (incf *tot-filtered*)  
      (let* ((node (gethash (key state) (nodes-table sgraph)))
             (g-op (g-op node))      
             (g-cl (g-cl node)))  
        (multiple-value-bind (dominated? igual) 
            (dominated-or-equal-g-cl (t-gcl node) g-cl estim)
          (if igual 
              (progn (incf *tot-eq*)
                (add-parent (make-instance 'c-vector-f 
                                           :vec g 
                                           :vec-f estim
                                           :parent-list (list parent)) 
                            parent))        
            (if dominated?
                (incf *tot-pruned-cl*)
              (update-g-op sgraph state g-op node g estim parent tie-breaking))))))))

#|---------------------------------------------------------------------
SPECIFIC FUNCTIONS TO CHECK DOMINANCE ON SETS T(C*) AND T(G_cl) AND 
FILTER THE TRUNCATED VECTORS WHICH BECOME DOMINATED AFTER THE ADDITION
OF A NEW TRUNCATED VECTOR.
---------------------------------------------------------------------|#
(defun dominated-or-equal-g-cl (t-gcl g-cl v)
  "Returns two values: 1) if v is dominated by any vector in T(G_cl), 2) if v is equal to any vector of G_cl"
  (if (null t-gcl)
      (values nil nil)
    (if (equalp v (vec-f (car g-cl))) 
          (values nil v)
      (t-gcl-dominance t-gcl (trunc-v v)))))

(defun t-gcl-dominance (t-gcl tv)
  "Auxiliar function of dominated-or-equal-g-cl"
  (dolist (tv-c t-gcl (values nil nil))
    (when (dominate-or-equals tv-c tv)
      (return-from t-gcl-dominance (values T nil)))))

(defmethod not-dominated-by-TC*? ((v vector) TC* C*) 
  "Return V vector whenever V is not dominated by any vector in T(C*), otherwise nil"
  (if (null TC*) v
    (multiple-value-bind (d? e?) 
        (dominate-or-equals-2v (car C*) v)
      (if d? nil
        (if e? v
          (if (not-dominated-by-TC*?-2 (trunc-v v) TC*)
              v
            nil))))))

(defun not-dominated-by-TC*?-2 (t-v TC*) 
  "Auxiliar function of not-dominated-by-TC*"
  (if (null TC*) t-v
    (if (dominate-or-equals (car TC*) t-v)
        nil
      (not-dominated-by-TC*?-2 t-v (cdr TC*)))))


(defun insert-and-filter-t-costs-n (t-costs t-v &optional rest inserted?)
  ""
  (cond ((null t-costs)   (if inserted?
                                (reverse rest)
                              (reverse (cons t-v rest))))
        ((dominates t-v (car t-costs))
         (insert-and-filter-t-costs-n (cdr t-costs) t-v rest inserted?))
        ((and (not inserted?) (b-lex t-v (car t-costs)))
         (insert-and-filter-t-costs-n t-costs t-v (cons t-v rest) T))
        (t
         (insert-and-filter-t-costs-n (cdr t-costs) t-v (cons (car t-costs) rest) inserted?))))



