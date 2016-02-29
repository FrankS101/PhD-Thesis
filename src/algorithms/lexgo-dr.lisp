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
               with goal-based preferences that employs the time-efficient
               "t-discarding" technique (LEXGO*_dr) optimized for 
               three-objective search
-----------------------------------------------------------------------
---------------------------------------------------------------------|#

(in-package :LEXGO-TE)

#|---------------------------------------------------------------------
                 LEXGO* algorithm
Parameters:
- sgraph:        MO search graph 
- gamma:         Goal state
- problem:       Problem to solve by LEXGO*
- s:             Initial state
- goals:         Given goals (in lexicographic levels)
- htable:        Table with precalculated heuristic values
- tie-breaking:  Tie-breaking policy
- it:            Label expansions
-----------------------------------------------------------------------|#
(defun lexgo-dr (sgraph gamma problem s goals htable tie-breaking &optional (it 0))
  "lexgo* algorithm"
  (multiple-value-bind (label node c-vector) (best-open-label sgraph)               ; Select the best open label
    (when (or (null label)                                                          ; If OPEN is empty or
              (b-lex (d* sgraph) (vd label (nlevels sgraph))))                      ; deviation of label is worse than d* 
      (incf *tot-dev-filtered* *cur-open*)                                          ; all remaining labels are filtered by their deviation
      (when label (incf *dev-filt-comp*))                                           
      ; store t-costs and t-gcl values 
      (setf *t-costs* (fill-pointer (t-costs sgraph)))
      (maphash #'(lambda (k v)
                   (declare (ignore k))
                   (incf *t-gcl* (fill-pointer (t-gcl v))))
               (nodes-table sgraph))
      (when *show-stats* (show-stats-lexgo-dr sgraph it))                           ; show algorithm stats on screen
      (return-from lexgo-dr (values (costs* sgraph) it)))                           ; END and return COSTS* and labels expanded 
    (when (> (/ (- (get-internal-run-time) *execution-start*) internal-time-units-per-second)
              *time-limit*)
      (when *show-stats* (show-stats-lexgo-dr sgraph it))
      (values (costs* sgraph) it T))
    (push c-vector (g-cl node))                                                     ; Add cost to G_cl(node)
    (let* ((e (l-state label))
           (g (vg label (nlevels sgraph) (ncosts sgraph))))                                 
      (cond ((equal? e gamma)                                                       ; state is a destination node
             (store-solution sgraph label)                                          ; record solution and continue
             (lexgo-dr sgraph gamma problem s goals htable tie-breaking (1+ it)))          
            (T 
             (dolist (e2 (expand e problem))                                        ; label expansion
               (if (in-graph? e2 sgraph)                                            ; state is already in search graph -> update node
                   (update-node sgraph e2 (add-v g (cost e e2 problem)) 
                                         (h e2 htable) goals tie-breaking tie-breaking
                                         :parent e)
                 (new-node sgraph e2 (add-v g (cost e e2 problem))                  ; is not -> create new node
                                    (h e2 htable) goals tie-breaking
                                    :parent e )))
             (lexgo-dr sgraph gamma problem s goals htable tie-breaking (1+ it)))))))

#|---------------------------------------------------------------------
Function to create a new Multiobjective Search Graph with lexicographic
goal-based preferences.
Parameters:
s:             initial state
ncosts:        Number of attributes
tie-breaking:  Tie breaking policy
htable:        Heuristic hash table
goals:         Goals grouped in priority levels (as list of lists)
size-op:       Optional parameter to establish size of the OPEN structure
-----------------------------------------------------------------------|#
(defun new-sgraph-lexgo-dr (s ncosts tie-breaking htable goals &optional size-op)
  "Create a new Multiobjective Search Graph with lexicographic goal-based preferences"
  (let* ((plevels (n-priority-levels goals))                          ; number of priority levels 
         (deviation (deviation (h s htable) goals))                   ; deviation of h(s) regarding to goals
         (h-value (h s htable))                           
         (d-vector (make-instance 'c-vector-lexgo                
                                  :vec (make-vector0 ncosts) 
                                  :parent-list (list nil)
                                  :vec-f h-value
                                  :vec-dev deviation))
         (sgraph (create-generic-sgraph 
                  (make-instance 'mo-graph-lexgo-dr :s s) 
                  (make-instance 'node-lexgo-dr) 
                  s ncosts tie-breaking htable 
                  (create-label deviation h-value (make-vector0 ncosts)) d-vector size-op)))
    (setf (goals sgraph)    goals                                     ; goals given in priority groups
          (nlevels sgraph)  plevels                                   ; number f priority levels
          (d* sgraph)       (make-vector-inf plevels)                 ; best lexicographic deviation
          (d0 sgraph)       (make-vector0 (n-priority-levels goals))  ; Auxiliary null deviation vector
          (t-costs sgraph)  (make-array (list 10000) 
                                        :element-type (class-of 'vector)
                                        :adjustable T :fill-pointer 0)
          (t-gcl (gethash (key s) (nodes-table sgraph)))
           (make-array (list 10000) 
                       :element-type (class-of 'vector)
                       :adjustable T :fill-pointer 0))
    (vector-push-extend (make-vector0 2) (t-gcl (gethash (key s) (nodes-table sgraph))))
    sgraph))

#|---------------------------------------------------------------------
Function to return the best label in OPEN. It returns 4 values: 
a list (#(d f g) e), its corresponding node, f and cost-vector
Parameters:
- sgraph:   MO search graph
-----------------------------------------------------------------------|#
(defmethod best-open-label ((sgraph mo-graph-lexgo-dr))
  "Best label in OPEN. Returns 4 values: 
a list (#(d f g) e), its corresponding node and cost-vector"
  (multiple-value-bind (label node c-vector)
      (call-next-method)
    (when label 
      (when (equalp (vd label (nlevels sgraph)) (d0 sgraph)) 
        (let ((trunc-c (trunc-v (vg label (nlevels sgraph) (ncosts sgraph)))))
          (setf (t-gcl node) 
                (insert-and-filter-t-costs (t-gcl node) trunc-c))))
      (values label node c-vector))))
    
#|---------------------------------------------------------------------
Function to store solution cost in COSTS*. D* is also updated
Parameters:
- sgraph: MO graph search
- label:  solution label
- goals:  a list with the goals grouped by levels 
-----------------------------------------------------------------------|#
(defmethod store-solution ((sgraph mo-graph-lexgo-dr) label)
  (call-next-method)
  (let ((nl (nlevels sgraph))
        (nc (ncosts sgraph)))
    (setf (d* sgraph) (vd label nl))                                          
    (when (equalp (vd label nl) (d0 sgraph))                                  
      (let ((trunc-c (trunc-v (vf label nl nc))))                              
        (setf (t-costs sgraph) 
              (insert-and-filter-t-costs (t-costs sgraph) trunc-c))))))
                
#|---------------------------------------------------------------------
Function to create a new node (lexgo-dr)
Parameters:
- sgraph:        MO search graph 
- state:         Problem state
- g:             Cost vector
- h:             h-vector
- goals:         Given goals (in lexicographic levels)
- tie-breaking:  Tie-breaking policy
- parent:        Parent node of the new node to create
-----------------------------------------------------------------------|#
(defmethod new-node ((sgraph mo-graph-lexgo-dr) state g h goals tie-breaking &key parent)
  "A new path reaches a new node which has not been yet discovered"
  (let* ((f (add-v g h))
         (d (deviation f goals))
         (v nil))
    (incf *dev-filt-comp*)                                                    
    (if (b-lex (d* sgraph) d)
        (incf *tot-dev-filtered*)
      (progn 
        (if (equalp d (d0 sgraph))
            (setf v (not-dominated-by-tc*? f (t-costs sgraph) (costs* sgraph)))
          (setf v (not-dominated-by-C*? f (costs* sgraph))))
        (if (null v)
            (incf *tot-filtered*)
          (progn
            (create-new-node sgraph (make-instance 'node-lexgo-dr) state f g h d parent)
            (setf (t-gcl (gethash (key state) (nodes-table sgraph)))  
                  (make-array (list 1000) 
                              :element-type (class-of 'vector)
                              :adjustable T :fill-pointer 0))))))))

#|---------------------------------------------------------------------
Method to update node (lexgo-dr). It is called when there is a new path reaching the node
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
(defmethod update-node ((sgraph mo-graph-lexgo-dr) state g h goals tie-breaking tie-breaking2 &key parent)
  "Function to update a lexgo-dr node. It is called when there is a new path reaching the node from parent with cost g."
  (let* ((f (add-v g h))
         (d (deviation f goals))
         (v nil))
    (incf *dev-filt-comp*)                                                                        
    (if (b-lex (d* sgraph) d)
        (incf *tot-dev-filtered*)
      (progn 
        (if (equalp d (d0 sgraph))
            (setf v (not-dominated-by-tc*? f (t-costs sgraph) (costs* sgraph)))
          (setf v (not-dominated-by-C*? f (costs* sgraph))))
        (if (null v)
            (incf *tot-filtered*)                                                                                            
          (let* ((node (gethash (key state) (nodes-table sgraph)))
                 (g-op (g-op node))                                                              
                 (g-cl (g-cl node))                                                              
                 (prune? (or (prune-by-deviation?-l d f g-op goals)
                             (prune-by-deviation?-l-cl d f g-cl goals))))                      ; Is there any vector in G_op U G_cl that prec_P f? 
            (if prune?                                                                         ; Pruning by deviation
                (incf *tot-dev-pruned*)                                                           
              (multiple-value-bind (dominated? equal?) 
                  (if (equalp d (d0 sgraph))
                      (progn (incf *time-ef-prun*) 
                        (not-dominated-by-t-gcl? g (t-gcl node) g-cl))
                    (progn (incf *regular-prun*) (dominated-or-equal-g-cl g g-cl)))            ; Comparison with closed vectors
                (if equal? 
                    (progn (incf *tot-eq*)                                                        
                      (if (equalp d (d0 sgraph)) 
                          (multiple-value-bind (d? e?) (dominated-or-equal-g-cl g g-cl)        ; there is no vec information in t-gcl
                            (declare (ignore d?))
                            (add-parent e? parent))
                        (add-parent equal? parent)))                                           ; add pointer to parent of closed node
                  (if dominated?                                                               ; Pruning by dominance (closed nodes)
                      (incf *tot-pruned-cl*)  
                    (update-g-op sgraph state goals g-op node g f d parent tie-breaking tie-breaking2)))))))))))

#|---------------------------------------------------------------------
Function to show lexgo-dr execution stats on screen
Parameters:
- sgraph:        MO search graph 
- it:            Number of iterations 
-----------------------------------------------------------------------|#
(defun show-stats-lexgo-dr (sgraph it)
  "show lexgo-dr execution stats on screen"
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
  (format T "Regular Pruning:    ~A~%" *regular-prun*)
  (format T "Time ef. Pruning:   ~A~%" *time-ef-prun*)
  (format T "Prun_P comp.:       ~A~%" *dev-prun-comp*)
  (format T "Filt_P comp.:       ~A~%" *dev-filt-comp*)
  (format T "Total T(COSTS):     ~A~%" *t-costs*)
  (format T "Total T(Gcl):       ~A~%" *t-gcl*)
  (format T "C*:                 ~A~%" (length (costs* sgraph)))
  (format T "===========================~%"))









