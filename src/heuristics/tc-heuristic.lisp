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

Description:   Ideal point multiobjective heuristic function from
               "A multicriteria Pareto-optimal path algorithm" - 1992
               which calculates the optimal point, composed by the minimum
               cost path of each objective independently 
------------------------------------------------------------------------
----------------------------------------------------------------------|#

(in-package :TC-HEURISTIC)

#|---------------------------------------------------------------------
Create a multiobjective heuristic table according to the method proposed
by Tung & Chew. Ncosts dijkstra searches are performed from end-s to ini-s to 
calculate the heuristic table.
It is stored as an array, since we know all states are represented as 
integer numbers, otherwise change to a hash-table.

Parameters:
- htable-list:    List of ncosts number of heuristic tables (1obj) 
- ncosts:         Goal state
-----------------------------------------------------------------------|#
(defun TC-heuristic (ini-s end-s problem ncosts &optional (array))
  "Tung and Chew multiobjective heuristic"
  (let ((htable-list))
    (dotimes (n ncosts)
      (push (tree (djikstra ini-s end-s problem n)) htable-list))
    (setf htable-list (reverse htable-list)) 
    (if array
        (create-htable-TC-array htable-list ncosts)
      (create-htable-TC htable-list ncosts))))

#|---------------------------------------------------------------------
Auxiliar function to merge all heuristic tables from a list given by
parameter to one.

Parameters:
- htable-list:    List of ncosts number of heuristic tables (1obj) 
- ncosts:         Goal state
Notice that the result is a hash table, this is a generic solution,
it is more efficient to store it as an array, but it will depend on the
state space if this can be applied
-----------------------------------------------------------------------|#
; Implementation of Htable as hash-table
(defun create-htable-TC (htable-list ncosts)
  "copy in this table every value from one-objective searches"
  (let ((htable (make-hash-table :test #'equalp)))
    (maphash #'(lambda (key valor)
                 (let ((v (make-array ncosts)))
                   (setf (aref v 0) (cost valor))
                   (dotimes (n (1- ncosts))
                     (setf (aref v (1+ n)) (cost (gethash key (elt htable-list (1+ n))))))
                   (setf (gethash key htable) v)))
             (car htable-list))
    htable))

; Implementation of Htable as array
(defun create-htable-TC-array (htable-list ncosts)
  "copy in this table every value from one-objective searches"
  (let* ((dimension (isqrt (hash-table-count (car htable-list))))
         (htable (make-array (list dimension dimension))))
    (maphash #'(lambda (key valor)
                 (let ((v (make-array ncosts)))
                   (setf (aref v 0) (cost valor))
                   (dotimes (n (1- ncosts))
                     (setf (aref v (1+ n)) (cost (gethash key (elt htable-list (1+ n))))))
                   (setf (aref htable (aref key 0) (aref key 1)) v)))
             (car htable-list))
    htable))


