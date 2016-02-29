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

Description:       Implementation of the Priority Queue class 
                   (as a binary heap stored in a hash table)
-----------------------------------------------------------------------
-----------------------------------------------------------------------|#

(in-package :TREE-SEARCH)

(defmethod make-pq (&key tie-breaking)
  "Returns an empty PQ where order is defined by the tie-breaking function provided (by default a min-pq)"
  (make-instance 'pq   :tree        (make-binary-heap)  ; hash table to store the heap
                       :n            0		         ; number of elements
                       :tie-breaking tie-breaking))   

(defmethod pq-n ((pq pq))
  "Returns the number of elements in PQ"
  (n pq))

(defmethod get-highest-pq ((pq pq))
  "Get a list (key elem) with the highest priority elem in the PQ (NIL when pq is empty)"
  (let ((i (gethash 1 (tree pq))))
    (list (item-key i) (elem i))))
  
(defmethod insert-pq ((pq pq) key elem)
  "Insert in the priority queue elem with priority corresponding to key. Returns a locator (insertion index) for further operations with decrease-key-pq"
  (setf (n pq) (1+ (n pq)))
  (insert-and-balance key (n pq) (make-pq-item key elem) (tree pq) (tie-breaking pq)))

(defmethod pull-highest-pq ((pq pq))
  "Pull the highest priority element from the priority queue and returns the corresponding list (key elem)"
   (let ((n (n pq)))
    (when (> n 0)
       (let* ((i      (gethash 1 (tree pq)))
              (tree  (tree pq))
              (last (gethash n tree)))
         (setf (gethash n tree) nil)
      (remove-and-balance (item-key last) last tree (tie-breaking pq) 1)
     (remhash n tree)
     (setf  (n pq) (1- n))
         (list (item-key i) (elem i))))))

(defmethod decrease-key-pq ((pq pq) locator new-key)
  "Decrease the key from elem which locator is provided. It decreases its key to new-keys that must be better than the previous one. The new locator is returned"
  (let ((tree (tree pq)))
    (setf (item-key locator) new-key)
    (insert-and-balance new-key 
                      (index (gethash (index locator) (tree pq)))  
                      locator tree  (tie-breaking pq))))

(defmethod print-pq ((pq pq))
  "Print PQ class (notice that becomes empty afterwards)"
  (dotimes (i (n pq))
    (print (get-highest-pq pq))
    (pull-highest-pq pq)))

#|---------------------------------------------------------------------
Class to define an Item of a Priority Queue
-----------------------------------------------------------------------|#
(defclass pq-item ()
  ((item-key   :initform nil
               :initarg :item-key
               :accessor item-key)
   (elem       :initform nil
               :initarg :elem
               :accessor elem)
   (index      :initform nil
               :initarg :index
               :accessor index))
  (:documentation "Class to define an Item of a Priority Queue implemented as a binary heap"))

(defmethod make-pq-item (key elem &optional (index nil))
  "Returns a make-pq-item object"
   (make-instance 'pq-item :item-key key :elem elem :index index))

#|------------------------------------------
Methods to implement the Binary Heap interface
--------------------------------------------|#
(defmethod make-binary-heap ()
  "Creates and returns an empty binary heap"  
  (make-hash-table :test #'equalp))                   

(defmethod print-binary-heap (heap) 
  "Prints on screen all nodes of the heap"
  (maphash #'(lambda (k v) (print (list k v))) heap))
    
(defmethod parent-i (i)
  "Returns index of parent node of i."
  (floor i 2))

(defmethod child1-i (i)
  "Returns index of left child of i"
  (* 2 i))

(defmethod child2-i (i)
  "Returns index of right child of i"
  (1+ (* 2 i)))

#|------------------------------------------
Auxiliary functions
--------------------------------------------|#  
(defmethod insert-and-balance (key i item tree tie-breaking)
   "If i is the position of key, inserts there the item updating its index, otherwise pushes parent of i and keep searching, returns as locator the structure"
   (let ((ip (parent-i i)))
    (cond ((or (= 1 i) (not (funcall tie-breaking key (item-key (gethash ip tree))))) 
        (setf (index item) i
                      (gethash i tree) item))
           (t (setf (gethash i tree) (gethash ip tree))
                  (setf (index (gethash i tree)) i)
          (insert-and-balance key ip item tree tie-breaking)))))

(defun remove-and-balance (key item tree tie-breaking &optional (i 1))
  "Removes i-th element from tree, it is overwritten with the smaller of its children by updating its index, the process carries on until it gets to a leaf. In this position item is positioned updating its index"
  (let* ((ih1 (child1-i i))
         (ih2 (1+ ih1))
         (eh1 (gethash ih1 tree))
         (eh2 (gethash ih2 tree)))
    (cond ((or (null eh1)  					      
               (and (null eh2) (funcall tie-breaking key (item-key eh1)))  
               (and (funcall tie-breaking key (item-key eh1))               
                    (funcall tie-breaking key (item-key eh2))))
           (setf (index item) i
                 (gethash i tree) item))
          ((or (null eh2)					      
               (funcall tie-breaking (item-key eh1) (item-key eh2)))               
           (setf (index eh1) i
                 (gethash i tree) eh1)
           (remove-and-balance key item tree tie-breaking ih1))
          (t (setf (index eh2) i
                   (gethash i tree) eh2)
             (remove-and-balance key item tree tie-breaking ih2)))))
  
