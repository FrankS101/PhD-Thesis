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

Description:   Classes and methods to implement multiobjective (multicriteria) 
               search algorithms
-----------------------------------------------------------------------
---------------------------------------------------------------------|#

(in-package :MO-SEARCH)

#|---------------------------------------------------------------------
  Vector classes to store cost vectors as well as other necessary information
-----------------------------------------------------------------------|#
(defclass c-vector ()
  ((vec :initform nil 
        :accessor vec 
        :initarg :vec
        :documentation "Cost vector (g)")
   (parent-list :initform nil 
                :accessor parent-list 
                :initarg :parent-list
                :documentation "List of parents"))
  (:documentation "Cost vector and a list of parents"))

(defclass c-vector-f (c-vector)
  ((vec-f :initform nil 
          :accessor vec-f 
          :initarg :vec-f
          :documentation "Estimate vector (f=g+h)"))
  (:documentation "Cost vector adding estimate vector f"))

#|---------------------------------------------------------------------
  Standard NAMOA* node with the followind attributes:
- g-op:     G_op(node)
- g-cl:   G_cl(node)
- vec-h:     h(node)
- loc:       Locator to find node in the open list
-----------------------------------------------------------------------|#
(defclass node ()
  ((node-labels   :initform nil 
                  :initarg :node-labels
                  :accessor node-labels
                  :documentation "NodeLabels(node)") ; only used in bi-objective search
   (first-open    :initform 0
                  :initarg :first-open
                  :accessor first-open
                  :documentation "Store the index of the first label open in node-labels") ; only bi-objective
   (g-op         :initform nil 
                  :initarg :g-op
                  :accessor g-op
                  :documentation "G_op(node)") 
   (g-cl       :initform nil 
                  :initarg :g-cl
                  :accessor g-cl
                  :documentation "G_cl(node)")
   (vec-h         :initform nil
                  :accessor vec-h
                  :documentation "h(node)")    
   (loc           :initform nil 
                  :accessor loc
                  :documentation "To locate node in the open list"))
  (:documentation "Standard NAMOA* node with G_op, G_cl, one single h-vector,
and a locator to find the node in the open list"))

#|---------------------------------------------------------------------
  NAMOA*_dr node with the extra attribute:
- t-gcl:       T(G_cl(n)) 
-----------------------------------------------------------------------|#
(defclass node-te (node)
  ((t-gcl      :initform nil 
                :initarg :t-gcl
                :accessor t-gcl
             :documentation "non-dominated closed labels in node (removing first component)"))
  (:documentation "NAMOA*_dr node with an extra attribute to store T(G_cl(n))"))

#|---------------------------------------------------------------------
  Standard Multiobjective search graph with the following attributes:
- Costs*:       Optimal cost vectors
- Dest-nodes:   Set of destination nodes reached by optimal paths
- Nodes-table:  Table of explored nodes
- s:            Start node
- open-l:       List of open labels/nodes
-----------------------------------------------------------------------|#
(defclass mo-graph ()
  ((costs*      :initform nil 
                :accessor costs*
                :documentation "list of vector costs of solutions found (C*)")  
   (dest-nodes  :initform nil 
                :accessor dest-nodes
                :documentation "list of goal nodes reached")
   (nodes-table :initform nil 
                :accessor nodes-table
                :documentation "hash table of nodes explored ->stores node info")  
   (s           :initarg 
                :s 
                :reader s
                :documentation "start state")
   (ncosts   :initform nil 
             :accessor ncosts
             :documentation "number of costs")
   (open-l      :initform nil 
                :accessor open-l
                :documentation "priority queue of open labels/nodes")
   (node-labels-size :initform 4
                     :initarg :node-labels-size
                     :reader node-labels-size
                     :documentation "Size of Node-Labels(node) -> adjustable array size")) ; only used in biobjective search
  (:documentation "Standard multiobjective search graph for algorithms like NAMOA*"))

#|---------------------------------------------------------------------
  MO time-efficient search graph with the extra attributes:
- t-costs:   T(C*) 
-----------------------------------------------------------------------|#
(defclass mo-graph-te (mo-graph)
  ((t-costs   :initform nil
                :accessor t-costs
                :documentation "T(C*)")))

#|---------------------------------------------------------------------
  Extended classes to deal with lexicographic goal-based preferences 
-----------------------------------------------------------------------|#
(defclass c-vector-lexgo (c-vector-f)
  ((vec-dev :initform nil 
            :accessor vec-dev
            :initarg :vec-dev
            :documentation "Deviation vector"))
   (:documentation "Cost vector adding vector with deviation from goals"))

(defclass node-lexgo (node)
  ((sat?   :initform T 
              :initarg :sat? ; T whenever all paths reaching the node still estimate satisfiable goals
              :accessor sat?
              :documentation "T when deviation vector is vector 0 -> goals could be satisfied"))
   (:documentation "Extended node class with a flag to identify whenever a path which goes along this node can never satisfy all goals"))

(defclass node-lexgo-dr (node-lexgo)
  ((t-gcl      :initform nil 
                :initarg :t-gcl
                :accessor t-gcl
                :documentation "T(G_cl(n))"))
  (:documentation "LEXGO*_dr node, time efficient version with an extra attribute to store T(G_cl(n))"))

(defclass mo-graph-lexgo (mo-graph)
  ((d*       :initform nil 
             :accessor d*
             :documentation "optimal vector with deviation from goals")
   (goals    :initform nil 
             :accessor goals
             :documentation "goals in lexicographic order")
   (nlevels  :initform 0 
             :accessor nlevels
             :documentation "number of priority levels goals are grouped"))
  (:documentation "Multiobjective search graph extended for LEXGO algorithm"))

(defclass mo-graph-lexgo-dr (mo-graph-lexgo)
  ((t-costs   :initform nil
                :accessor t-costs
                :documentation "T(C*)")
   (d0          :initform nil
                :accessor d0
                :documentation "vector with as much 0's as priority levels")))

#|---------------------------------------------------------------------
Create a Generic Search Graph
-----------------------------------------------------------------------|#
(defmethod create-generic-sgraph ((sgraph mo-graph) (node node) s ncosts tie-breaking htable pq-item-key c-vector &optional size-op)
  "Create a generic sgraph with the common values valid for all algorithms"
  (let ((h-value (h s htable)))
    (setf (open-l sgraph)             (if size-op (make-pq :tie-breaking tie-breaking
                                                           :size size-op)
                                        (make-pq :tie-breaking tie-breaking))
          (nodes-table sgraph)        (make-hash-table :test #'equalp)
          (ncosts sgraph)             ncosts
          (g-op node)                 (list c-vector)
          (vec-h node)   h-value
          (loc node)     (insert-pq (open-l sgraph) pq-item-key s)
          (node-labels node)         (make-array (list (node-labels-size sgraph)) 
                                                  :element-type (class-of 'c-vector-f)
                                                  :adjustable T :fill-pointer 1) ; only used in biobjective search
          (aref (node-labels node) 0) (make-instance 'c-vector-f 
                                                     :vec (make-vector0 ncosts) 
                                                     :vec-f h-value
                                                     :parent-list (list nil))
          (gethash (key s) (nodes-table sgraph)) node)
    (incf *cur-open*) 
    sgraph))


#|---------------------------------------------------------------------
Generic Methods for classes defined above 
-----------------------------------------------------------------------|#
(defgeneric show (object))
(defgeneric best-open-vector (node))
(defgeneric get-vector (c-vector))

#|---------------------------------------------------------------------
Print on screen any kind of label (called vectors here)
-----------------------------------------------------------------------|# 
(defmethod show ((v c-vector))
  (format t "~%c-vector|| vec:~s  parent-list: ~s" (vec v) (parent-list v)))

(defmethod show :after ((v c-vector-f))
  (format t " Vec-f: ~s" (vec-f v)))

(defmethod show :after ((v c-vector-lexgo))
  (format t " Vec-dev: ~s" (vec-dev v)))

(defmethod show ((n node))
  (format t "~%~%node || g-op:")
  (dolist (g (g-op n)) (show g))
  (format t "~%~%node || g-cl:")
  (dolist (g (g-cl n)) (show g))
  (format t "~% vec-h: ~S|" (vec-h n))
  (format t "~% loc: ~s" (loc n)))

(defmethod show :after ((n node-lexgo))
  (format t "~%all paths satisfiable?:" (sat? n)))

#|---------------------------------------------------------------------
Function to add parent node to the list of parents
-----------------------------------------------------------------------|#
(defmethod add-parent ((vc c-vector) parent)
  "Add parent to the list of parents of cost vector vc"  
  (setf (parent-list vc) (union (list parent) (parent-list vc) :test #'equalp)))
 
#|---------------------------------------------------------------------
Function to return the best open vector 
(they are sorted, so just the first of the list)
-----------------------------------------------------------------------|#
; (defmethod best-open-vector ((n node))
;   (car (g-op n)))
; la que funciona bien excepto para dos objetivos

(defmethod best-open-vector ((n node))
  (aref (node-labels n) (first-open n)))

#|---------------------------------------------------------------------
Method to get the vector depending of the class 
- cost vector
- estimate vector 
-----------------------------------------------------------------------|#
(defmethod get-vector ((vc c-vector))
  (vec vc))

(defmethod get-vector ((vc c-vector-f))
  (vec-f vc))

(defmethod get-vector ((vc c-vector-lexgo))
  (concatenate 'vector 
               (vec-dev vc)
               (vec-f vc)
               (vec vc)))
 
#|---------------------------------------------------------------------
Function to return a boolean whether a given state is in the graph or not
(nodes explored are in a hash-table) 
-----------------------------------------------------------------------|#
(defmethod in-graph? (state (search-graph mo-graph))
  (gethash (key state) (nodes-table search-graph)))

#|---------------------------------------------------------------------
Print a label (label, state) 
-----------------------------------------------------------------------|#
(defun print-label (label)
  (let ((state (l-state label))
        (f (f label)))
    (format t "Label: ~S, Estado: " f)
    (show-state state)))

(defun l-state (label) (cadr label))

(defun f (label) (car label))

#|---------------------------------------------------------------------
Method to compare a given vector with a set of cost vectors. 
First returned value establishes whether the vector is in the set or not,
and second value states if vector is dominated by any vector in the set.
This is employed with the G_op set, therefore it is employed by all versions
of NAMOA* and LEXGO* algorithms, regardless the tie-breaking policy
-----------------------------------------------------------------------|#
(defmethod dominated-or-equal-g-op ((g vector) g-op)   
  "Whenever there is a cost vector with the same value in the set, 
this vector is returned, otherwise nil. ~
   A second value states if g is dominated by some cost vector."
  (if (null g-op) 
      (values nil nil)
    (let ((g2 (vec (car g-op))))
        (cond ((equalp g2 g) (values (car g-op) nil))
              ((dominates g2 g) (values nil T))
              (t (dominated-or-equal-g-op g (cdr g-op)))))))

#|---------------------------------------------------------------------
Given the pareto-frontier and number of costs, 
ideal and nadir points are returned in vectors of ncosts length
-----------------------------------------------------------------------|#
(defun get-ideal-and-nadir (pareto-frontier ncosts)
  "Given the pareto-frontier and number of costs, ideal and nadir points are returned in vectors of ncosts length"
  (let ((optimal (make-vector-inf ncosts))
        (nadir (make-vector0 ncosts)))
    (dolist (c pareto-frontier (values optimal nadir))
      (dotimes (n ncosts)
        (let ((value (aref c n)))
          (when (< value (aref optimal n))
            (setf (aref optimal n) value))
          (when (> value (aref nadir n))
            (setf (aref nadir n) value)))))))

