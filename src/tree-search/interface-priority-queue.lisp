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

Description:       Interface for a Priority Queue implemented as a 
                   binary heap stored in a hash table
-----------------------------------------------------------------------
-----------------------------------------------------------------------|#

(in-package :TREE-SEARCH)

#|---------------------------------------------------------------------
Class to define a Priority Queue implemented as a Binary Heap
-----------------------------------------------------------------------|#
(defclass pq ()
  ((tree         :initform nil
                 :initarg :tree
                 :accessor tree)
   (n            :initform nil
                 :initarg :n
                 :accessor n)
   (tie-breaking :initform nil
                 :initarg :tie-breaking
                 :accessor tie-breaking))
  (:documentation "Priority Queue class implemented as a Binary Heap"))


#|---------------------------------------------------------------------
Interface of a priority queue implemented as a binary heap
-----------------------------------------------------------------------|#

;(defgeneric make-pq (&key (tie-breaking #'<))
(defgeneric make-pq (&key tie-breaking)
  (:documentation "Create an empty priority queue implemented as a binary heap using tie-breaking function to order items."))


(defgeneric print-pq (cp)
  (:documentation "Show the elements of the priority queue"))

(defgeneric pq-n (pq)
  (:documentation "Returns the number of elements of the priority queue"))

(defgeneric get-highest-pq (pq)
  (:documentation "Returns the top element of the priority queue (NIL when it is empty)"))

(defgeneric insert-pq (pq key elem)
  (:documentation "Insert in the priority queue elem with priority corresponding to key. Returns a locator (insertion index) for further operations with decrease-key-pq."))

(defgeneric pull-highest-pq (pq)
  (:documentation "Pull the highest priority element from the priority queue and returns the corresponding list (key elem)"))

(defgeneric decrease-key-pq (pq locator new-key)
  (:documentation "Decrease the key from elem which locator is provided. It decreases its key to new-keys that must be better than the previous one. The new locator is returned."))

#|---------------------------------------------------------------------
Interface of make-pq-item 
-----------------------------------------------------------------------|#
(defgeneric make-pq-item (key elem &optional index)
  (:documentation "Creates and returns a new pq item with key priority and elem"))

#|------------------------------------------
Binary heap interface
--------------------------------------------|#
(defgeneric make-binary-heap ()
  (:documentation "Creates and returns an empty binary heap"))

(defgeneric print-binary-heap (heap)
  (:documentation "Print on screen sorted elements of the heap"))

(defgeneric parent-i (i)
  (:documentation "Returns index of parent node of i."))

(defgeneric child1-i (i)
  (:documentation "Returns index of left child of i"))

(defgeneric child2-i (i)
  (:documentation "Returns index of right child of i"))

(defgeneric insert-and-balance (key i item tree tie-breaking)
  (:documentation "If i is the position of key, inserts there the item updating its index, otherwise pushes parent of i and keep searching, returns as locator the structure"))
