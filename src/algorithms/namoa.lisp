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

Description:   Implementation of NAMOA* algorithm (considering just one heuristic function)
-----------------------------------------------------------------------
---------------------------------------------------------------------|#

(in-package :NAMOA)

#|---------------------------------------------------------------------
                 NAMOA* algorithm (General version)
Parameters:
- sgraph:        MO search graph 
- gamma:         Goal state
- problem:       Problem to solve by LEXGO*
- s:             Initial state
- htable:        Table with precalculated heuristic values
- tie-breaking:  Tie-breaking policy
- it:            Number of label expansions
-----------------------------------------------------------------------|#
(defun namoa (sgraph gamma problem s tablah tie-breaking &optional (it 0))
  (let* ((label (best-open-label sgraph tie-breaking))) 
    (cond ((not label)
           (when *show-stats* (show-stats-namoa sgraph it))
           (values (costs* sgraph) it))
          ((equal? (l-state label) gamma) 
           (store-solution sgraph label)
           (namoa sgraph gamma problem s tablah tie-breaking (1+ it)))     
          ((> (/ (- (get-internal-run-time) *execution-start*) internal-time-units-per-second)
              *time-limit*)
           (when *show-stats* (show-stats-namoa sgraph it))
           (values (costs* sgraph) it T))
          (t 
           (let* ((e (l-state label))
                  (g (vec (car (g-cl (gethash (key e) (nodes-table sgraph))))))) 
             (dolist (e2 (expand (l-state label) problem))
               (if (in-graph? e2 sgraph)
                   (update-node sgraph e2 
                                      (add-v g (cost (l-state label) e2 problem)) 
                                      (h e2 tablah) 
                                      tie-breaking
                                      :parent e)
                 (new-node sgraph e2 
                                (add-v g (cost (l-state label) e2 problem)) 
                                (h e2 tablah)
                                tie-breaking
                                :parent e )))
             (namoa sgraph gamma problem s tablah tie-breaking (1+ it)))))))

#|---------------------------------------------------------------------
Function to create a new Multiobjective Search Graph to be used by NAMOA*.
Parameters:
s:             initial state
ncosts:        Number of attributes
tie-breaking:  Tie breaking policy
htable:        Heuristic hash table
-----------------------------------------------------------------------|#
(defun new-sgraph-namoa (s ncosts tie-breaking htable &optional size-op)
  "Function to create a new NAMOA* search graph"
  (let* ((h-value (h s htable))
         (c-vector (make-instance 'c-vector-f 
                                 :vec (make-vector0 ncosts)
                                 :vec-f h-value
                                 :parent-list (list nil))))
    (create-generic-sgraph (make-instance 'mo-graph :s s) (make-instance 'node) 
                           s ncosts tie-breaking htable h-value c-vector size-op)))

#|---------------------------------------------------------------------
Function to return the best label in OPEN. It returns the corresponding label
Parameters:
- sgraph:        MO search graph
- tie-breaking:  tie-breaking employed to sort the labels in OPEN
-----------------------------------------------------------------------|#
(defmethod best-open-label ((sgraph mo-graph) tie-breaking)
  "Returns the most promising label in OPEN and close it afterwards"
  (let* ((open (open-l sgraph))
         (label (pull-highest-pq open))
         (node (when label (gethash (key (l-state label)) (nodes-table sgraph)))))
    (when node
      (close-label label node open) 
      label)))

#|---------------------------------------------------------------------
Move a label from G_op(node) to G_cl(node).
Whenever there are more labels in the node, the best is added to the OPEN queue
Parameters:
- label:   Label to move from G_op to G_cl
- node:    Node with information regarding the label
- open-l:  Queue of OPEN alternatives
-----------------------------------------------------------------------|#
(defmethod close-label (label (node node) open-l)
  "Move a label from OPEN to G_cl(node)"
  (setf (loc node) nil)
  (decf *cur-open*)                     
  (push (pop (g-op node)) (g-cl node)) 
  ; whenever there are more labels in the node, the best is added to the OPEN queue
  (when (car (g-op node))
    (setf (loc node)
          (insert-pq open-l (get-vector (car (g-op node))) (l-state label)))))

#|---------------------------------------------------------------------
Function to insert the new label in its position in G_op according to the tie-breaking order.
Moreover, all labels from G_op dominated by the new label are removed from the set.
The new G_op set is returned
Parameters:
- g-op:             Set of partial open paths reaching a node
- c-vector:         cost-vector to insert in G_op
- tie-breaking:     tie-breaking employed to sort the labels in OPEN
-----------------------------------------------------------------------|#
(defun insert-and-filter-g-op (f-label g-op tie-breaking &optional (g (vec f-label)) rest inserted?)
  "Function to insert the new label in its position according to the tie-breaking order. Moreover, all labels from G_op dominated by the new label are removed from the set. The new G_op set is returned"
  (cond ((null g-op)   (if inserted?
                            (reverse rest)
                          (reverse (cons f-label rest))))
        ((dominates g (vec (car g-op)))
         (incf *tot-pruned-op*) 
         (decf *cur-open*)      
         (incf *prun-comp*)     
         (insert-and-filter-g-op f-label (cdr g-op) tie-breaking g rest inserted?))
        ((and (not inserted?) (funcall tie-breaking g (vec (car g-op))))
         (incf *prun-comp*)     
         (insert-and-filter-g-op f-label g-op tie-breaking g (cons f-label rest) T))
        (t
         (incf *prun-comp*)     
         (insert-and-filter-g-op f-label (cdr g-op) tie-breaking g (cons (car g-op) rest) inserted?))))

#|---------------------------------------------------------------------
Method to compare a given cost vector g with a set of closed cost vectors G_cl
First returned value establishes whether the vector is dominated by any vector in G_cl,
and second value states if vector is equal to any vector in G_cl, returning that vector whenever it exists.
Parameters:
- g-cl:    Set of closed (or permanent) labels reaching a node
- g:       Cost vector to compare with those from g-cl  
-----------------------------------------------------------------------|#		 
(defmethod dominated-or-equal-g-cl ((g vector) g-cl)
  "Function to compare a given cost vector g with a set of closed cost vectors G_cl. Returns two values: if g is dominated in g_cl and if there is a vector in g_cl equal to g"
  (dolist (v-c g-cl (values nil nil))
    (multiple-value-bind (dominated? equal?) (dominate-or-equals-2v (vec v-c) g)
      (incf *prun-comp*) 
      (cond (dominated? (return-from dominated-or-equal-g-cl (values T nil)))
            (equal? (return-from dominated-or-equal-g-cl (values nil v-c)))))))

#|---------------------------------------------------------------------
Method to return g vector whenever g is not dominated by any vector in C*, otherwise nil
Parameters:
- g:        Vector to compare
- C*:       Set of solution cost vectors  
-----------------------------------------------------------------------|#
(defmethod not-dominated-by-C*? ((g vector) C*) 
  "Return V vector whenever V is not dominated by any vector in C*, otherwise nil"
  (if (null C*) g            
    (progn (incf *filt-comp*)    
    (if (dominates (car C*) g)
        nil
      (not-dominated-by-c*? g (cdr C*))))))

#|---------------------------------------------------------------------
Method to store a new solution cost vector in set costs*
Parameters:
- sgraph:        MO search graph
- tie-breaking:  tie-breaking employed to sort the labels in OPEN
-----------------------------------------------------------------------|#
(defmethod store-solution ((sgraph mo-graph) label)
  "Method to store a new solution cost vector in set costs*"
  ; this part is neccesary whenever there is more than one destination node
  (setf (dest-nodes sgraph) (union (list (l-state label)) (dest-nodes sgraph) :test #'equal?)) 
  ; just compare with the last cost found (solutions are discovered according to the tie-breaking rule chosen)
  (when (not (equalp (f label) (car (costs* sgraph))))   
    (push (f label) (costs* sgraph))))

#|---------------------------------------------------------------------
Method to create a new node with the label (g+vec-h, state)
Parameters:
- sgraph:        MO search graph
- state:         problem state
- g:             cost of the new path
- vec-h:         estimate vector of the new path
- tie-breaking:  tie-breaking employed to sort the labels in OPEN
- parent:        parent of the new path
-----------------------------------------------------------------------|#
(defmethod new-node ((sgraph mo-graph) state g vec-h tie-breaking &key parent)
  "A new path reaches a new node which has not been yet discovered"
  (let ((estim (not-dominated-by-C*? (add-v vec-h g) (costs* sgraph)))) 
    (if (null estim)
        (incf *tot-filtered*)  
      (create-new-node sgraph (make-instance 'node) state estim g vec-h parent))))

(defmethod create-new-node ((sgraph mo-graph) node state estim g vec-h parent)
  "Creates a new node and inserts it in the Nodes table"     
  (let* ((label   (make-instance 'c-vector-f 
                                 :vec g 
                                 :parent-list (list parent) 
                                 :vec-f estim)))
    (incf *cur-open*)   
    (when (> *cur-open* *max-open*) (setf *max-open* *cur-open*))   
    (setf (g-op node)    (list label) 
          (vec-h node)    vec-h
          (loc node)      (insert-pq (open-l sgraph) estim state)
          (gethash (key state) (nodes-table sgraph)) node)))
          
#|---------------------------------------------------------------------
Method to update a node when a new path reaches it
Parameters:
- sgraph:        MO search graph
- state:         problem state
- g:             cost of the new path
- vec-h:         estimate vector of the new path
- tie-breaking:  tie-breaking employed to sort the labels in OPEN
- parent:        parent of the new path
-----------------------------------------------------------------------|#
(defmethod update-node ((sgraph mo-graph) state g vec-h tie-breaking &key parent)
  "A new path reaches a node and this needs to be updated"
  (let ((estim (not-dominated-by-C*? (add-v vec-h g) (costs* sgraph))))
    (if (null estim)
        (incf *tot-filtered*)                                                              
      (let* ((node (gethash (key state) (nodes-table sgraph)))
             (g-op (g-op node))    
             (g-cl (g-cl node)))  
        (multiple-value-bind (dominated? equal-node) (dominated-or-equal-g-cl g g-cl)
          (if equal-node 
              (progn (incf *tot-eq*)
                (add-parent equal-node parent)) 
            (if dominated?
                (incf *tot-pruned-cl*)
              (update-g-op sgraph state g-op node g estim parent tie-breaking))))))))

(defmethod update-g-op ((sgraph mo-graph) state g-op node g estim parent tie-breaking)
  "Check whether a new path is dominated by any other in g-op or not, insert and filter g-op and inserts the next label in OPEN"
  (multiple-value-bind (equal? dominated?) (dominated-or-equal-g-op g g-op)
    (cond (equal? (progn (incf *tot-eq*)                                       
                    (add-parent equal? parent)))
          ((not dominated?)
           (incf *cur-open*)                                                  
           (when (> *cur-open* *max-open*) (setf *max-open* *cur-open*))      
           (let ((best-g (when (car g-op) (vec (car g-op)))))
             (setf (g-op node) 
                   (insert-and-filter-g-op (make-instance 'c-vector-f 
                                                          :vec g 
                                                          :parent-list (list parent)
                                                          :vec-f estim) 
                                           (g-op node)
                                           tie-breaking
                                           g))
             (cond ((not best-g)
                    (setf (loc node) (insert-pq (open-l sgraph) estim state)))
                   ((not (equalp best-g (vec (car (g-op node)))))
                    (decrease-key-pq (open-l sgraph) (loc node) estim)))))
          (dominated? (incf *tot-pruned-op*)))))

#|---------------------------------------------------------------------
Function to show namoa execution stats on screen
Parameters:
- sgraph:        MO search graph 
- it:            Number of iterations 
-----------------------------------------------------------------------|#
(defun show-stats-namoa (sgraph it)
  "show namoa execution stats on screen"
  (format t "Trace values ~%")
  (format t "===========================~%")
  (format t "Total expanded:   ~A~%" it)
  (format t "Max in OPEN:      ~A~%" *max-open*)
  (format t "Pruned in CLOSED: ~A~%" *tot-pruned-cl*)
  (format t "Pruned in OPEN:   ~A~%" *tot-pruned-op*)
  (format t "Filtered:         ~A~%" *tot-filtered*)
  (format t "Total eq:         ~A~%" *tot-eq*)
  (format t "Filtering comp.:  ~A~%" *filt-comp*)
  (format t "Pruning comp.:    ~A~%" *prun-comp*)
  (format t "C*:               ~A~%" (length (costs* sgraph)))
  (format t "===========================~%"))
