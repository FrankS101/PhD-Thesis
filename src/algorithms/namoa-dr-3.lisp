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

Description:   Implementation of NAMOA*_dr algorithm optimized to use 
               with 3-objectives
-----------------------------------------------------------------------
---------------------------------------------------------------------|#

(in-package :namoa-dr-3)

#|---------------------------------------------------------------------
                 NAMOA*_dr algorithm optimized to use with 3-objectives
Parameters:
- sgraph:        MO search graph 
- gamma:         Goal state
- problem:       Problem to solve by LEXGO*
- s:             Initial state
- htable:        Table with precalculated heuristic values
- tie-breaking:  Tie-breaking policy
- it:            Label expansions
-----------------------------------------------------------------------|#
(defun namoa-dr-3 (sgraph gamma problem s htable tie-breaking &optional (it 0))
  "NAMOA*_dr algorithm optimized to use with 3-objectives"
  (let* ((label (best-open-label sgraph tie-breaking)))
    (cond ((not label)
           (setf *t-costs* (fill-pointer (t-costs sgraph)))
           (maphash #'(lambda (k v)
                        (declare (ignore k))
                        (incf *t-gcl* (fill-pointer (t-gcl v))))
                    (nodes-table sgraph))
           (when *show-stats* (show-stats-namoa-dr sgraph it))
           (values (costs* sgraph) it))
          ((equal? (l-state label) gamma) 
           (store-solution sgraph label)
           (namoa-dr-3 sgraph gamma problem s htable tie-breaking (1+ it)))
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
             (namoa-dr-3 sgraph gamma problem s htable tie-breaking (1+ it)))))))

#|----------------------------------------------------------------------------
Function to create a new NAMOA* search graph (optimized to 3-objectives)
Parameters:
- s:                 initial state
- ncosts:            number of criteria
- tie-breaking:      tie-breaking policy to choose the best label from OPEN
- htable:            heuristic table
- size op:           open list initial size (depends on the available information 
                     about the problem
-----------------------------------------------------------------------------|#
(defun new-sgraph-namoa-dr-3 (s ncosts tie-breaking htable &optional size-op)
  "Function to create a new NAMOA* search graph with 3 objectives (improved version)"
  (let* ((h-value (h s htable))
         (c-vector (make-instance 'c-vector-f 
                                  :vec (make-vector0 ncosts)
                                  :vec-f h-value
                                  :parent-list (list nil)))
         (sgraph (create-generic-sgraph (make-instance 'mo-graph-te :s s) (make-instance 'node-te) 
                                        s ncosts tie-breaking htable h-value c-vector size-op)))
    (setf (t-costs sgraph) (make-array (list 10000) 
                                       :element-type (class-of 'vector)
                                       :adjustable T :fill-pointer 0)
          (t-gcl (gethash (key s) (nodes-table sgraph))) (make-array (list 10000) 
                                                                     :element-type (class-of 'vector)
                                                                     :adjustable T :fill-pointer 0))
    (vector-push-extend (make-vector0 2) (t-gcl (gethash (key s) (nodes-table sgraph))))
    sgraph))

#|---------------------------------------------------------------------
Move a label from G_op(node) to G_cl(node).
This specialized version of the method calls to namoa::close-label and 
depending on the number of objectives considered it goes to the appropiate
function 
Parameters:
- label:   Label to move from G_op to G_cl
- node:    Node with information regarding the label
- open-l:  Queue of OPEN alternatives
-----------------------------------------------------------------------|#
(defmethod close-label (label (node node-te) open-l)
  "close a time-efficient label with either 3 or n objectives"
  ;(close-label label node open-l)
  (call-next-method)
  (let ((n-obj (array-dimension (f label) 0)))
    (if (= n-obj 3) 
        (close-label-3 label node open-l)
      (namoa-dr::close-label-n label node open-l))))

(defmethod close-label-3 (label (node node-te) open-l)
  "insert truncated vector and filter new dominated ones"
  (let ((trunc-c (trunc-v (f label))))
    (setf (t-gcl node) 
          (insert-and-filter-t-costs (t-gcl node) trunc-c))))

#|---------------------------------------------------------------------
Method to store a new solution cost vector in set costs*
This specialized version of the method calls to namoa::store-solution and 
depending on the number of objectives considered it goes to the appropiate
function 
Parameters:
- sgraph:        MO search graph
- tie-breaking:  tie-breaking employed to sort the labels in OPEN
-----------------------------------------------------------------------|#
(defmethod store-solution ((sgraph mo-graph-te) label)
  "To record a new Pareto optimal cost in COSTS*"
  (call-next-method)
  (let ((n-obj (array-dimension (f label) 0)))
    (if (= n-obj 3) 
        (store-solution-3 sgraph label)
      (namoa-dr::store-solution-n sgraph label))))

(defmethod store-solution-3 ((sgraph mo-graph-te) label)
  "Update T(C*)" 
  (let ((trunc-c (trunc-v (f label))))
    (setf (t-costs sgraph) 
          (insert-and-filter-t-costs (t-costs sgraph) trunc-c))))

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
  (let ((estim (not-dominated-by-tc*? (add-v g vec-h) (t-costs sgraph) (costs* sgraph))))
    (if (null estim)
        (incf *tot-filtered*) 
      (progn
        (create-new-node sgraph (make-instance 'node-te) state estim g vec-h parent)
        (setf (t-gcl (gethash (key state) (nodes-table sgraph))) 
              (make-array (list 1000) 
                          :element-type (class-of 'vector)
                          :adjustable T :fill-pointer 0))))))

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
 - parent:        predecessor(state)
-----------------------------------------------------------------------|#
(defmethod update-node ((sgraph mo-graph-te) state g vec-h tie-breaking &key parent) 
  "Function to update an existing node in the search graph"
  (let ((estim (not-dominated-by-tc*? (add-v g vec-h) (t-costs sgraph) (costs* sgraph))))
    (if (null estim)
        (incf *tot-filtered*)  
      (let* ((node (gethash (key state) (nodes-table sgraph)))
             (g-op (g-op node))       
             (g-cl (g-cl node)))  
        (multiple-value-bind (dominated? igual) 
            (not-dominated-by-t-gcl? estim (t-gcl node) g-cl)
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
SPECIFIC FUNCTIONS TO OPTIMIZE THE PERFORMANCE OF THE 3-OBJECTIVES SEARCH
---------------------------------------------------------------------|#

#|---------------------------------------------------------------------
Method to return g vector whenever g is not dominated by any vector in T(C*), otherwise nil
Parameters:
- g:          vector to compare to
- t-costs:  T(C*) set with truncated solution vectors
- costs*:     C* set of optimal solution vectors
---------------------------------------------------------------------|#
(defmethod not-dominated-by-tc*? ((g vector) t-costs costs*)
  "Method to return g vector whenever g is not dominated by any vector in T(C*), otherwise nil"
  (if (or (zerop (fill-pointer t-costs)) (equalp g (car costs*)))
      g
    (let* ((g2 (trunc-v g))
           (key (aref g2 0))
           (index (binary-search t-costs key 3 0 (1- (fill-pointer t-costs))))
           (index-to-compare (if (or (= index (fill-pointer t-costs)) 
                                     (> (aref (aref t-costs index) 0) key))
                                 (max 0 (1- index)) index)))
      (unless (dominate-or-equals (aref t-costs index-to-compare) g2)
        g))))
  
#|---------------------------------------------------------------------
Method to compare a given cost vector g with a set of truncated closed cost vectors T(G_cl)
First returned value establishes whether the truncated vector is dominated by any truncated vector in T(G_cl),
and second value states if the vector is equal to any vector in G_cl, returning that vector whenever it exists.
Parameters:
- f:         vector to compare to
- t-gcl:    T(G_cl(n)) set of truncated closed labels reaching node n
- g-cl:      G_cl(n) set of closed labels reaching node n
---------------------------------------------------------------------|#
(defun not-dominated-by-t-gcl? (f t-gcl g-cl)
  "Method to compare a given cost vector g with a set of truncated closed cost vectors T(G_cl)"
  (if (zerop (fill-pointer t-gcl))
      (values nil nil)
    (if (equalp f (vec-f (car g-cl))) 
        (values nil f)
      (let* ((f2 (trunc-v f))
             (key (aref f2 0))
             (index (binary-search t-gcl key 3 0 (1- (fill-pointer t-gcl))))
             (index-to-compare (if (or (= index (fill-pointer t-gcl)) 
                                       (> (aref (aref t-gcl index) 0) key))
                                   (max 0 (1- index)) index)))
        (values (dominate-or-equals (aref t-gcl index-to-compare) f2) nil)))))

#|---------------------------------------------------------------------
Function to show namoa execution stats on screen
Parameters:
- sgraph:        MO search graph 
- it:            Number of iterations 
-----------------------------------------------------------------------|#
(defun show-stats-namoa-dr (sgraph it)
  "show namoa-dr execution stats on screen"
  (format t "~%Trace values ~%")
  (format t "===========================~%")
  (format t "Total expanded:  ~A~%" it)
  (format t "Max Open:        ~A~%" *max-open*)
  (format t "Pruned in Cl:    ~A~%" *tot-pruned-cl*)
  (format t "Pruned in Op:    ~A~%" *tot-pruned-op*)
  (format t "Filtered:        ~A~%" *tot-filtered*)
  (format t "Total eq:        ~A~%" *tot-eq*)
  (format t "Total T(COSTS):  ~A~%" *t-costs*)
  (format t "Total T(Gcl):    ~A~%" *t-gcl*)
  (format t "C*:              ~A~%" (length (costs* sgraph)))
  (format t "===========================~%"))
