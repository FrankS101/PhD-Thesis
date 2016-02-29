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

Description:       Representation of DIMACS maps from:
                   http://www.dis.uniroma1.it/challenge9/download.shtml
                   http://www.dis.uniroma1.it/challenge9/data/tiger/
                   as a state space to perform a multiobjective search problem
-------------------------------------------------------------------
------------------------------------------------------------------|#

(in-package :DIMACS)

(defclass dimacs ()
  ((ss :initarg :ss
       :initform nil
       :accessor ss)
   (ini-s :initarg :ini-s
          :initform nil
          :accessor ini-s)
   (end-s :initarg :end-s
          :initform nil
          :accessor end-s)
   (ncost :accessor ncost 
          :initarg :ncost))
  (:documentation "Representation of a DIMACS problem, with the associated state space and the initial and goal states"))

(defclass ss-dimacs ()
  ((nnodes :initarg :nnodes   ;number of nodes in the graph
           :reader  nnodes)
   (narcs :initarg :narcs     ;number of arcs in the graph
           :reader  narcs)
   (nodes :initarg :nodes     ;vector with nodes from 0 to n-1 (in the challenge files are from 1 to n)
          :reader  nodes))    ;each vector element: (descendant #(cost1 cost2 cost3?))
  (:documentation "Dimacs state space"))

(defmethod show-problem ((prob dimacs))
  "Show content of problem object"
  (format t "Initial state: ~A~%" (ini-s prob))
  (format t "Goal state: ~A~%" (end-s prob))
  (format t "---------------------~%~%"))

(defmethod show-dimacs-ss ((ss ss-dimacs))
  "Show content of a dimacs state space"
  (format t "Number of nodes: ~A~%" (nnodes ss))
  (format t "Number of arcs: ~A~%" (narcs ss))
  (format t "Nodes: ~S~%" (nodes ss))
  (format t "---------------------~%~%"))

#|---------------------------------------------------------------------------------------
                    Search functions of DIMACS Challenge
----------------------------------------------------------------------------------------|#
(defmethod key (state)
  state)

(defmethod goal? (s1 s2)  
  (equal? s1 s2))

(defmethod expand (state (d dimacs))
  "Return a list with all the descendants of state"
  (mapcar #'(lambda (x) (first x)) 
          (aref (nodes (ss d)) state)))

(defmethod cost (s1 s2 (d dimacs))
  (dolist (elem (aref (nodes (ss d)) s1))
    (when (= (car elem) s2) 
      (return (second elem)))))    

(defmethod h (ini-s htable)  
  "Heuristic function which reads appropiate value from htable"
  (gethash ini-s htable))
;  (svref htable ini-s))

