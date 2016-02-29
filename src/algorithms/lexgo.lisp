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

Description:   Implementation of multiobjective graph search algorithm 
               with goal-based preferences (LEXGO*)
-----------------------------------------------------------------------
---------------------------------------------------------------------|#

(in-package :LEXGO)

#|---------------------------------------------------------------------
                 LEXGO* algorithm
Parameters:
- sgraph:        MO search graph 
- gamma:         Goal state
- problem:       Problem to solve by LEXGO*
- s:             Initial state
- goals:         Given goals (in lexicographic levels)
- htable:        Table with precalculated heuristic values
- tie-breaking:  Tie-breaking policy to sort G_op, G_cl and COSTS sets
- tie-breaking2: Tie-breaking policy to sort OPEN queue of alternatives
- it:            Label expansions
-----------------------------------------------------------------------|#
(defun lexgo (sgraph gamma problem s goals htable tie-breaking tie-breaking2 &optional (it 0))
  "lexgo* algorithm"
  (multiple-value-bind (label node c-vector) (best-open-label sgraph)               ; Select the best open label
    (when (or (null label)                                                          ; If OPEN is empty or
              (b-lex (d* sgraph) (vd label (nlevels sgraph))))                      ; deviation of label is worse than d* 
      (incf *tot-dev-filtered* *cur-open*)                                          ; all remaining labels are filtered by their deviation
      (when label (incf *dev-filt-comp*))                                           
      (when *show-stats* (show-stats-lexgo sgraph it))                              ; show stats on screen
      (return-from lexgo (values (costs* sgraph) it)))                              ; END and return COSTS* and labels expanded 
    (when (> (/ (- (get-internal-run-time) *execution-start*) internal-time-units-per-second)
              *time-limit*)
      (when *show-stats* (show-stats-lexgo sgraph it))
      (return-from lexgo (values (costs* sgraph) it T)))
    (push c-vector (g-cl node))                                                     ; Add cost to G_cl(node)
    (let* ((e (l-state label))
           (g (vg label (nlevels sgraph) (ncosts sgraph))))                                 
      (cond ((equal? e gamma)                                                       ; state is a destination node
             (store-solution sgraph label)                                          ; record solution and continue
             (lexgo sgraph gamma problem s goals htable tie-breaking tie-breaking2 (1+ it)))          
            (T 
             (dolist (e2 (expand e problem))                                        ; label expansion
               (if (in-graph? e2 sgraph)                                            ; state is already in search graph -> update node
                   (update-node sgraph e2 (add-v g (cost e e2 problem)) 
                                      (h e2 htable) goals tie-breaking tie-breaking2
                                      :parent e)
                 (new-node sgraph e2 (add-v g (cost e e2 problem))                  ; is not -> create new node
                                 (h e2 htable) goals tie-breaking
                                 :parent e )))
             (lexgo sgraph gamma problem s goals htable tie-breaking tie-breaking2 (1+ it)))))))

#|---------------------------------------------------------------------
Function to create a new Multiobjective Search Graph with lexicographic
goal-based preferences.
Parameters:
s:             initial state
ncosts:        Number of attributes
tie-breaking:  Tie breaking policy
htable:        Heuristic hash table
goals:         Goals grouped in priority levels (as list of lists)
size-op:       Optional parameter to establish size of the OPEN queue
-----------------------------------------------------------------------|#
(defun new-sgraph-lexgo (s ncosts tie-breaking htable goals &optional size-op)
  "Create a new Multiobjective Search Graph with lexicographic goal-based preferences"
  (let* ((plevels (n-priority-levels goals))              ; number of priority levels 
         (deviation (deviation (h s htable) goals))       ; deviation of h(s) regarding to goals
         (h-value (h s htable))                           
         (d-vector (make-instance 'c-vector-lexgo                
                                  :vec (make-vector0 ncosts) 
                                  :parent-list (list nil)
                                  :vec-f h-value
                                  :vec-dev deviation))
         (sgraph (create-generic-sgraph 
                  (make-instance 'mo-graph-lexgo :s s) 
                  (make-instance 'node-lexgo) 
                  s ncosts tie-breaking htable 
                  (create-label deviation h-value (make-vector0 ncosts)) d-vector size-op)))
    (setf (goals sgraph)    goals                        ; goals given in priority groups
          (nlevels sgraph)  plevels                      ; number f priority levels
          (d* sgraph)       (make-vector-inf plevels))   ; best lexicographic deviation
    sgraph))

#|---------------------------------------------------------------------
Function to return the best label in OPEN. It returns 4 values: 
a list (#(d f g) e), its corresponding node, f and cost-vector
Parameters:
- sgraph:   MO search graph
-----------------------------------------------------------------------|#
(defmethod best-open-label ((sgraph mo-graph-lexgo))
  "Best label in OPEN. Returns 4 values: 
a list (#(d f g) e), its corresponding node and cost-vector"
  (let ((label (pull-highest-pq (open-l sgraph))))                     ; get the best label from OPEN
    (when label                                                        ; if OPEN is not empty and label is not nil 
      (let* ((node (gethash (key (cadr label)) (nodes-table sgraph)))  ; get the node from hash table
             (c-vector (pop (g-op node))))                             ; it is removed from G_op
        (progn (decf *cur-open*))                                          
        (if (g-op node)                                                ; whenever there are more open vectors from the same node,  
            (setf (loc node)                                           ; the first is inserted into the OPEN structure
                  (insert-pq (open-l sgraph)                           ; otherwise locator is set to nil
                                    (create-label 
                                     (vec-dev (car (g-op node))) 
                                     (vec-f (car (g-op node)))
                                     (vec (car (g-op node)))) 
                                    (cadr label)))
          (setf (loc node) nil))
        (values label node c-vector)))))
        
#|---------------------------------------------------------------------
Function to store solution cost in COSTS*. D* is also updated
Parameters:
- sgraph: MO graph search
- label:  solution label
-----------------------------------------------------------------------|#
(defmethod store-solution ((sgraph mo-graph-lexgo) label)
  (let ((nl (nlevels sgraph))
        (nc (ncosts sgraph))
        (c* (costs* sgraph)))
    (setf (dest-nodes sgraph) 
          (union (list (l-state label)) (dest-nodes sgraph) :test #'equal?))  ; if it is a new destination node -> include 
                                                                              ; (whenever there is more than one destination nodes)
    (when (not (member (vf label nl nc) c* :test #'equalp))                   ; new cost, insert it
      (push (vf label nl nc) (costs* sgraph))) 
    (setf (d* sgraph) (vd label nl))))                                        ; update d*

#|---------------------------------------------------------------------
G_op management function to remove all estimates which can be goal-pruned
by the estimate explored.
Parameters:
- g-op:     G_op set(vf label nl nc)
- d:         deviation of vector to insert in G_op
- f:         f-vector
- goals:     given goals to the problem
-----------------------------------------------------------------------|#
(defun g-op-d-management (g-op d f goals &optional tail-g-op)
  "It goes through the G_op list to prune by deviation vectors dominated_P by the new one explored"
  (if (null g-op)
      tail-g-op
    (let ((cv (car (last g-op))))                                          ; last cost vector of g-op (equal or worse deviation than the rest in G_op)
      (cond ((not (b-lex d (vec-dev cv)))
             (return-from g-op-d-management (append g-op tail-g-op)))
            ((prune-by-deviation? d (vec-dev cv) f (vec-f cv) goals)       ; remove and keep watching
             (incf *tot-dev-pruned*)                                        
             (g-op-d-management (butlast g-op) d f goals tail-g-op))
            (T                                                             ; insert and keep watching
             (g-op-d-management (butlast g-op) d f goals 
                                 (push cv tail-g-op)))))))

#|---------------------------------------------------------------------
G_op management function to insert a vector in G_op and remove vectors dominated by it
Parameters:
- g-op:      G_op set
- d:         deviation of vector to insert in G_op
- f:         f-vector
- g:         cost vector
- parent:    parent node of cost vector
- sat?:      states whether or not there is some path in G_op which deviation is not vector-0
- tie1:      tie-breaking policy 1 is used whenever all vectors in G_op satisfy the goals
- tie2:      tie-breaking policy 2 is used whevever some vector in G_op does not satisfy the goals
- rest:      optional parameter
- inserted?: T if the vector was already inserted in G_op
-----------------------------------------------------------------------|#
(defun g-op-management (g-op d f g parent sat? tie1 tie2 &optional rest inserted?)
  "Inserting a new cost vector in G_op"
  (cond ((null g-op) (if inserted?                                                 ; if G_op is null or recursivity gets to the end
                          (reverse rest)                                           ; insert a new instance of the vector with parameters given
                        (reverse (cons (make-instance 'c-vector-lexgo              ; the vector will be inserted the last if inserted? is null
                                                      :vec g 
                                                      :parent-list (list parent)
                                                      :vec-f f
                                                      :vec-dev d) rest))))
        ((dominates g (vec (car g-op)))                                            ; if the vector dominates current analyzed vector of G_op
         (incf *tot-pruned-op*)                                                     
         (decf *cur-open*)                                                          
         (incf *prun-comp*)                                                         
         (g-op-management (cdr g-op) d f g parent sat? tie1 tie2 rest inserted?))  ; purge vector and call recursive function with the rest of the list
        ((and (not inserted?)                                                      ; if vector was not inserted and is lexicographically better than 
              (if sat?                                                             ; current analyzed vector of G_op -> insert and call recursive function again
                  (funcall tie1 g (vec (car g-op)))                                ; since there can be vectors dominated in G_op to remove
                (funcall tie2                                                      ; when sat? is T deviation vectors are not compared in order to get a better
                 (concatenate 'vector d g)                                         ; time performance
                 (concatenate 'vector (vec-dev (car g-op)) (vec (car g-op))))))
         (incf *prun-comp*)                                                         
         (g-op-management g-op d f g parent sat? tie1 tie2
                           (cons (make-instance 'c-vector-lexgo 
                                                :vec g 
                                                :parent-list (list parent)
                                                :vec-f f
                                                :vec-dev d) rest) T))
        (T
         (incf *prun-comp*)                                                         
         (g-op-management (cdr g-op) d f g parent sat? tie1 tie2 (cons (car g-op) rest) inserted?))))
                
#|---------------------------------------------------------------------
Function to create a new node (lexgo)
Parameters:
- sgraph:        MO search graph 
- state:         Problem state
- g:             Cost vector
- h:             h-vector
- goals:         Given goals (in lexicographic levels)
- tie-breaking:  Tie-breaking policy
- parent:        Parent node of the new node to create
-----------------------------------------------------------------------|#
(defmethod new-node ((sgraph mo-graph-lexgo) state g h goals tie-breaking &key parent)
  "A new path reaches a new node which has not been yet discovered"
  (let* ((f (add-v g h))
         (d (deviation f goals)))
    (incf *dev-filt-comp*)                                                        
    (if (b-lex (d* sgraph) d)
        (incf *tot-dev-filtered*)
      (if (null (not-dominated-by-C*? f (costs* sgraph)))
          (incf *tot-filtered*)
        (create-new-node sgraph (make-instance 'node-lexgo) state f g h d parent)))))

(defmethod create-new-node ((sgraph mo-graph-lexgo) node state f g h d parent)
  "Creates a new LEXGO node and inserts it in the Nodes table"                          
  (let ((label (make-instance 'c-vector-lexgo :vec g :parent-list (list parent) :vec-f f :vec-dev d)))
    (incf *cur-open*)                                                     
    (when (> *cur-open* *max-open*) (setf *max-open* *cur-open*))         
    (setf (g-op node)    (list label) 
          (sat? node)     (equalp #(0 0) d)
          (vec-h node)    h
          (loc node)      (insert-pq (open-l sgraph) 
                                     (create-label d f g) state)
          (gethash (key state) (nodes-table sgraph)) node)))

#|---------------------------------------------------------------------
Method to update node (lexgo). It is called when there is a new path reaching the node
from parent with cost g.
Parameters:
- sgraph:        MO search graph 
- state:         Problem state
- g:             Cost vector
- h:             h-vector
- goals:         Given goals (in lexicographic levels)
- tie-breaking:  Tie-breaking policy
- parent:        Parent node of the new node to create
-----------------------------------------------------------------------|#
(defmethod update-node ((sgraph mo-graph-lexgo) state g h goals tie-breaking tie-breaking2 &key parent)
  "Function to update a lexgo node. It is called when there is a new path reaching the node
from parent with cost g."
  (let* ((f (add-v g h))
         (d (deviation f goals)))
    (incf *dev-filt-comp*)                                                                        
    (if (b-lex (d* sgraph) d)                                                                 ; Filtering by deviation
        (incf *tot-dev-filtered*)
      (let ((v (not-dominated-by-C*? f (costs* sgraph))))                                     ; COSTS* filtering
        (if (null v)
            (incf *tot-filtered*)                                                                                                     
          (let* ((node (gethash (key state) (nodes-table sgraph)))
                 (g-op (g-op node))                                                           ; G_op
                 (g-cl (g-cl node))                                                           ; G_cl
                 (prune? (or (prune-by-deviation?-l d f g-op goals)
                             (prune-by-deviation?-l-cl d f g-cl goals))))                     ; Is there any vector in G_op U G_cl that prec_P f? 
            (if prune?                                                                        ; Pruning by deviation
                (incf *tot-dev-pruned*)                                                          
              (multiple-value-bind (dominated? equal?) (dominated-or-equal-g-cl g g-cl)       ; Comparison with closed vectors
                (if equal? 
                    (progn (incf *tot-eq*)                                                       
                      (add-parent equal? parent))                                             ; add pointer to parent of closed node
                  (if dominated?                                                              ; Pruning by dominance (closed nodes)
                      (incf *tot-pruned-cl*)                                                   
                    (update-g-op sgraph state goals g-op node g f d parent tie-breaking tie-breaking2)))))))))))

(defmethod update-g-op ((sgraph mo-graph-lexgo) state goals g-op node g f d parent tie-breaking tie-breaking2)
  "Check whether a new path is dominated by any other in g-op or not, insert and filter g-op and inserts the next label in OPEN"
  (multiple-value-bind (equal? dominated?) (dominated-or-equal-g-op g g-op)
    (cond (equal? (progn (incf *tot-eq*)                                        
                    (add-parent equal? parent)))                            ; If vector is equal to some in open -> add a pointer
          ((not dominated?)                                                 ; Pruning by dominance (open nodes)
           (incf *cur-open*)                                                   
           (when (> *cur-open* *max-open*) (setf *max-open* *cur-open*))       
           (let ((best-c-vector (when (car g-op) (vec (car g-op)))))
             (when (sat? node)                                              ; if all paths reaching node have deviation 0, this vector
               (setf (sat? node) (equalp #(0 0) d)))                        ; is not taken into account in comparisons 
             (setf (g-op node)                                              ; G_op dominance update
                   (g-op-management (g-op node) d f g parent (sat? node)
                                    tie-breaking tie-breaking2))
             (setf (g-op node) (g-op-d-management (g-op node) d f goals))   ; prune_P estimates dominated by the new one
             (cond ((not best-c-vector)                                     ; if G_op was empty, new label is located in OPEN PQ
                    (setf (loc node) 
                          (insert-pq (open-l sgraph) 
                                     (create-label d f g) 
                                     state)))
                   ((not (equalp best-c-vector (vec (car (g-op node)))))    ; if update turned out to locate the new path the first in G_op  
                    (decrease-key-pq (open-l sgraph)                        ; node key in priority queue (OPEN) is updated too
                                     (loc node) 
                                     (create-label d f g))))))
          (dominated? (incf *tot-pruned-op*)))))

#|---------------------------------------------------------------------
Function to show lexgo execution stats on screen
Parameters:
- sgraph:        MO search graph 
- it:            Number of iterations 
-----------------------------------------------------------------------|#
(defun show-stats-lexgo (sgraph it)
  "show lexgo execution stats on screen"
  (format T "~%Trace values ~%")
  (format T "===========================~%")
  (format T "Total expanded:     ~A~%" it)
  (format T "Max Open:           ~A~%" *max-open*)
  (format T "Pruned in Cl:       ~A~%" *tot-pruned-cl*)
  (format T "Pruned in Op:       ~A~%" *tot-pruned-op*)
  (format T "Filtered:           ~A~%" *tot-filtered*)
  (format T "Total eq:           ~A~%" *tot-eq*)
  (format T "Pruned by Dev:      ~A~%" *tot-dev-pruned*)
  (format T "Filtered by Dev:    ~A~%" *tot-dev-filtered*)
  (format T "Pruning comp.:      ~A~%" *prun-comp*)
  (format T "Filtering comp.:    ~A~%" *filt-comp*)
  (format T "Prun_P comp.:       ~A~%" *dev-prun-comp*)
  (format T "Filt_P comp.:       ~A~%" *dev-filt-comp*)
  (format T "C*:                 ~A~%" (length (costs* sgraph)))
  (format T "===========================~%"))

#|---------------------------------------------------------------------
Function to return solution. It returns a list with costs and states in the 
path with less deviation from goals. NOT USED! DEBUGGING PURPOSES USING GRIDS
Parameters:
- sgraph:        MO search graph 
- s:             Initial state
- gamma:         Final state
- problem:
-----------------------------------------------------------------------|#
(defmethod return-solution ((sgraph mo-graph-lexgo) s gamma problem)
  "It returns a list with costs and states in the path with less deviation from goals"
  (let* ((state gamma)
         (cost (vec (car (g-cl (gethash (key state) (nodes-table sgraph))))))
         (node (gethash (key state) (nodes-table sgraph)))
         (g-cl (g-cl node))
         (parent (car (parent-list (car g-cl)))))
    (format t "State: ~A  |  Cost: ~A~%" (grids::cont state) cost)
    (loop do     
          (format t "State: ~A  |  Cost: ~A~%" (grids::cont parent) (sub-v cost (cost parent state problem)))
          ; now parent becomes the current state
          (setf cost (sub-v cost (cost parent state problem))
                state parent
                node (gethash (key parent) (nodes-table sgraph))
                g-cl (g-cl node))
          ; looking for the parent
          (dolist (closed g-cl)
            (when (equalp cost (vec closed))
              (setf parent (car (parent-list closed)))))
          while (not (equalp parent s)))))


