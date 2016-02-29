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

Description:   Implementation of NAMOA* algorithm 
The following considerations are taken:
- Just one heuristic value
- Two objectives optimizated
- TC heuristic is employed, therefore h(n) is consistent

-----------------------------------------------------------------------
---------------------------------------------------------------------|#

(in-package :NAMOA2)

#|---------------------------------------------------------------------
                 NAMOA* algorithm
Parameters:
- sgraph:        MO search graph 
- gamma:         Goal state
- problem:       Problem to solve by LEXGO*
- s:             Initial state
- htable:        Table with precalculated heuristic values
- tie-breaking:  Tie-breaking policy
- it:            Label expansions
-----------------------------------------------------------------------|#

(defun namoa2 (sgraph gamma problem s htable tie-breaking &optional (it 0))
  (let* ((label (best-open-label2 sgraph))) 
    (cond ((not label)
           (values (costs* sgraph) it))
          ((equal? (l-state label) gamma) 
           (store-solution sgraph label)
           (namoa2 sgraph gamma problem s htable tie-breaking (1+ it))) 
          ((> (/ (- (get-internal-run-time) *execution-start*) internal-time-units-per-second)
              *time-limit*)
           (values (costs* sgraph) it T))
          (T 
           (let* ((e (l-state label))
                  (node (gethash (key e) (nodes-table sgraph)))
                  (g (vec (aref (node-labels node) (1- (first-open node))))))
             (dolist (e2 (expand (l-state label) problem))
               (let ((g2 (add-v g (cost e e2 problem))))
                 (if (in-graph? e2 sgraph)
                     (update-node2 sgraph e2 g2 (h e2 htable) it :parent e)
                   (new-node2 sgraph e2 g2 (h e2 htable) :parent e ))))
;                     (update-node2 sgraph e2 g2 #(0 0) it :parent e)
;                   (new-node2 sgraph e2 g2 #(0 0) :parent e ))))
             (namoa2 sgraph gamma problem s htable tie-breaking (1+ it)))))))


#|----------------------------------------------------------------------------
Function to create a new NAMOA* search graph

Parameters:
- s:                 initial state
- ncosts:            number of criteria
- tie-breaking:      tie-breaking policy to choose the best label from OPEN
- htable:            heuristic table
- size graph:        the nodes table is created as an array considering that
                     each state is represented as an integer number, because
                     of that size of array has to be provided to create the 
                     search graph
Note:                  
If we want to create the priority queue (binary heap) with a certain initial size,
the key :open-size has to be added when creating open-l. This can be interesting since
implementation is done with a hash table, and rehashing operations are costly 
-----------------------------------------------------------------------------|#
#|
(defun new-sgraph-namoa (s ncosts tie-breaking htable size-sgraph)
  "Function to create a new NAMOA* search graph"
  (let ((sgraph (make-instance 'mo-graph :s s))
        (node (make-instance 'node))
        (vector0 (make-vector0 ncosts))
        (h-value (h s htable)))
    (setf (open-l sgraph)             (make-pq :tie-breaking tie-breaking)
          (nodes-table sgraph)        (make-array (list size-sgraph) :element-type (class-of 'node))
          (node-labels node)          (make-array (list (node-labels-size sgraph)) 
                                                  :element-type (class-of 'c-vector-f)
                                                  :adjustable T :fill-pointer 1)
          (mo-search::vgset node)       (list (make-instance 'c-vector-f 
                                                    :vec vector0 
                                                    :parent-list (list nil)))
          (vec-h node)   h-value
          (loc node)     (insert-pq (open-l sgraph) h-value s)
          (aref (node-labels node) 0) (make-instance 'c-vector-f 
                                                     :vec vector0 
                                                     :vec-f h-value
                                                     :parent-list (list nil))
          (svref (nodes-table sgraph) s) node)
    sgraph))
|#

#|---------------------------------------------------------------------
Function to remove the best label from the top of the PQ and change 
its flag to closed. It is returned a list (g e)

Parameters:
- sgraph:        MO search graph 
-----------------------------------------------------------------------|#

(defmethod best-open-label2 ((sgraph mo-graph))
  "Remove the best label from the top of the PQ and change its flag to closed.
It is returned a list (f state)"
  (let* ((open (open-l sgraph))
         (fe (pull-highest-pq open))
         (node (when fe (gethash (key (cadr fe)) (nodes-table sgraph)))))
    (when node
      (setf (loc node) nil)
      (incf (first-open node))
      ;whenever there are more vectors in node-labels of the node, insert the best in OPEN
      (when (and (< (first-open node) (fill-pointer (node-labels node))) 
                 (aref (node-labels node) (first-open node)))
        (setf (loc node)
              (insert-pq open (get-vector (best-open-vector node)) (cadr fe))))
      fe)))


#|---------------------------------------------------------------------
Function to record a new solution.
New destination node is added whenever it is not already in the list.
Afterwards the new Pareto cost is added if it was not found previously.
In order to check if the new Pareto costs is already in COSTS*, we take 
advantage of lexicographic order of exploration and just the last optimal
cost found is checked to see if they are equal or the new one has to be
added.
Note that no filtering of dominated solutions in OPEN is done after recording
a new optimal cost. There can be several estimates in OPEN dominated by this
new one, we don't care. These will be discarded and time performance is 
better with this approach.

Parameters:
- sgraph:        MO search graph 
- label:         label corresponding a new solution to record
-----------------------------------------------------------------------|#

#|
(defmethod record-solution ((sgraph mo-graph) label)
  "To record a new Pareto optimal cost in COSTS*"
  (when (not (member (l-state label) (dest-nodes sgraph)))
    (push (l-state label) (dest-nodes sgraph)))
  (when (not (equalp (f label) (car (costs* sgraph))))   ; just compare with the last cost found
    (push (f label) (costs* sgraph))))
|#

#|---------------------------------------------------------------------
Function to create a new node in the search graph.
If f(state) is not dominated by any known Pareto solution it is stored
in its correspondent position in the nodes table

Parameters:
- sgraph:        MO search graph 
- state:         explored state
- g:             g(state)
- vec-h:         h(state)
- parent:        predecessor(state)
-----------------------------------------------------------------------|#

(defmethod new-node2 ((sgraph mo-graph) state g vec-h &key parent)
  (let ((estim (not-dominated-by-C*? (add-v vec-h g) (costs* sgraph))))  
    ; f(state) is not dominated by any known Pareto solution
    (when estim 
      (let* ((vg   (make-instance 'c-vector-f 
                                  :vec g 
                                  :vec-f estim
                                  :parent-list (list parent))))
      (create-new-node sgraph (make-instance 'node) state estim g vec-h parent)
      (let ((node (gethash (key state) (nodes-table sgraph))))
        (setf (node-labels node)  (make-array (list (node-labels-size sgraph)) 
                                              :element-type (class-of 'c-vector-f)
                                              :adjustable T :fill-pointer 1)
              (aref (node-labels node) 0)          vg))))))



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

(defmethod update-node2 ((sgraph mo-graph) state g vec-h it &key parent) 
  "Function to update an existing node in the search graph"
  (let ((estim (not-dominated-by-C*? (add-v g vec-h) (costs* sgraph))))
    (when estim 
      (let* ((node (gethash (key state) (nodes-table sgraph)))
             (1st-open-index (first-open node))
             (num-node-labels (fill-pointer (node-labels node))))
          ; check first if g is dominated or equal to any vector in Node-label (G-op U G_cl)
          (multiple-value-bind (dominated? equals? prev-open-node i-prev-open)
              (find-previous-open-label (node-labels node) g 1st-open-index)
            (unless dominated?   
              (if equals? (add-parent prev-open-node parent)
                ; remove vectors dominated by g and insert g in its lexicog. position  
                  (multiple-value-bind (insert-last? prune? insert-index)
                      (find-first-posible-dominated-label (node-labels node) g i-prev-open)
                    (let ((first-open-g (when (/= 1st-open-index num-node-labels) 
                                          (vec (aref (node-labels node) 1st-open-index))))
                          (vg (make-instance 'c-vector-f 
                                             :vec g 
                                             :vec-f estim
                                             :parent-list (list parent))))
                      (if insert-last? ; g does not dominate any vector in Node-labels
                            (vector-push-extend vg (node-labels node))
                        (if prune?
                            (setf (node-labels node) 
                                  (insert-and-remove-dom (node-labels node) i-prev-open insert-index vg))
                          ; no prune, insert in the middle of the array
                          (setf (node-labels node) (insert-into-array (node-labels node) vg insert-index)))) 
                      ; insert in PQ if there wasn't any open in Node-labels or it is the best of all open labels
                      (cond ((= 1st-open-index num-node-labels)
                             (setf (loc node) (insert-pq (open-l sgraph) estim state)))
                            ((not (equalp first-open-g 
                                          (vec (aref (node-labels node) 1st-open-index))))
                             (decrease-key-pq (open-l sgraph) (loc node) estim))))))))))))
                     

#|---------------------------------------------------------------------
Function to find previous vector in lexicographic order with respect to
the one given by parameter

Parameters:
- node-labels:        Labels (closed and opened) of the node 
- g:                  g(state)
- low-i:              index of the node-labels array to start search
Return values:
- dominated           T if g is dominated by some label in node-labels
- equals?             T if g is equal to some vector in node-labels
- prev-open-node      previous open node which g was compared
- i-prev-open         index of previous open node
-----------------------------------------------------------------------|#

(defun find-previous-open-label (node-labels g low-i &optional (high (1- (fill-pointer node-labels))))
  "Function to find previous vector in lexicographic order with respect to
the one given by parameter"
    (let* ((key (aref g 0))
           (index (binary-search node-labels key 2 low-i high))
           (index-to-compare (if (or (= index (fill-pointer node-labels)) 
                                     (> (aref (vec (aref node-labels index)) 0) key))
                                 (max 0 (1- index)) index)))
      (multiple-value-bind (d? e?)
          (dominate-or-equals-2v (vec (aref node-labels index-to-compare)) g)
        (values d? e? (aref node-labels index-to-compare) index))))
          

#|---------------------------------------------------------------------
Function to find insert index for g in node-labels of a node

Parameters:
- node-labels:        Labels (closed and opened) of the node 
- g:                  g(state)
- low-i:              index of the node-labels array to start search
Return values:
- insert-last?        Whether the new label must be inserted in the last position or not
- prune?              T when some labels from node-labels will be pruned
- insert-index        index in the array of node-labels to insert new label
-----------------------------------------------------------------------|#

(defun find-first-posible-dominated-label (node-labels g low-i)
  "Function to find insert index for g in node-labels of a node"
  (if (= (fill-pointer node-labels) low-i) ; not open nodes
      (values T nil low-i)
    (let* ((key (aref g 1))
           (index (binary-search-backward node-labels key 2 low-i))
           (index-to-compare (if (or (= index (fill-pointer node-labels)) 
                                     (< (aref (vec (aref node-labels index)) 1) key))
                                 (max 0 (1- index)) index))
           (p? (dominates g (vec (aref node-labels index-to-compare))))
           (l? (= (fill-pointer node-labels) index-to-compare)))
      (values l? p? index))))


#|---------------------------------------------------------------------
Return V vector whenever V is not dominated by any vector in C*. 
In C* vectors are sorted lexicographically. 
*) Notice that comparison is with <, therefore estimates equal to some
c* vector are not discarded, since their path should be also recorded.
This was done (discard that paths) in the old implementation.
-----------------------------------------------------------------------|#

(defmethod not-dominated-by-C*? ((v vector) C*) 
  "Return V vector whenever V is not dominated by any vector in C*, otherwise nil"
  (if (null C*) v            ; no C*
    (if (<= (aref (car C*) 1) ; second cost, since first has to be bigger for v_1
           (aref v 1)) 
        nil
      v)))

