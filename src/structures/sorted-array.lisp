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

Description:       Sorted array to manage closed 2d vector labels. 
                   Functions here deal with a sorted array used to
                   optimize Multiobjective algorithms to the Biobjective
                   case.    
-------------------------------------------------------------------
------------------------------------------------------------------|#

(in-package :SORTED-ARRAY)

#|---------------------------------------------------------------------
Functions to insert an element in some position i, shifting all elements
with index i_2 >= i from position i_2 to i_2+1 to have a gap to insert 
the new one. Second function also removes all elements from position j. 
Parameters:
- vec:             Vector to insert into
- element:         Element to insert in vector
- i:               index to insert element into vector
Note that these functions return the new array.
-----------------------------------------------------------------------|#
(defun insert-into-array (vec element i)
  "Function to insert an element in some position i into a full vector"
  (let* ((size (1+ (length vec)))
         (new (make-array size
                          :element-type (class-of 'c-vector-f)
                          :adjustable T 
                          :fill-pointer size)))
    (setf (aref new i) element)
    (replace new vec :end1 i)
    (replace new vec :start1 (1+ i) :start2 i)))

(defun insert-and-remove-dom (vec i j element)
  "Function to insert an element in some position i into a full vector, removing
also elements from position j from this point forward"
  (let* ((size (- (1+ (length vec)) (- j i)))
         (new (make-array size  
                          :element-type (class-of 'c-vector-f)
                          :adjustable T 
                          :fill-pointer size)))
    (setf (aref new i) element)
    (replace new vec :end1 i)
    (replace new vec :start1 (1+ i) :start2 j)))

(defun binary-search (array key ncosts &optional (low 0) (high (1- (fill-pointer array))))
  "find the position of key in a lexicographic order array - forward"
  (loop (when (<= high low) (let ((minimum (min high low)))
                              (if (< (aref (if (= ncosts 2) 
                                               (mo-search::vec (aref array minimum)) 
                                             (aref array minimum)) 0) key)
                                  (return (1+ minimum))
                                (return high))))
        (let* ((middle (floor (/ (+ low high) 2)))
               (mid-value (aref (if (= ncosts 2) (mo-search::vec (aref array middle)) 
                                  (aref array middle)) 0)))          
          (cond ((< mid-value key)
                 (setf low (min (1- (fill-pointer array)) (1+ middle))))
                ((> mid-value key)
                 (setf high (max 0 (1- middle))))
                (t (return middle))))))

(defun binary-search-backward (array key ncosts &optional (low 0))
  "find the position of key in a lexicographic order array - backward"
  (let ((high (1- (fill-pointer array))))
    (loop (when (<= high low) (let ((maximum  (max high low)))
                                (if (>= (aref (if (= ncosts 2) 
                                                  (mo-search::vec (aref array maximum))
                                                (aref array maximum)) 1) key) 
                                    (return (1+ maximum))
                                  (return maximum))))
          (let* ((middle (floor (/ (+ low high) 2)))
                 (mid-value (aref (if (= ncosts 2) (mo-search::vec (aref array middle))
                                    (aref array middle)) 1)))
            (cond ((> mid-value key)
                   (setf low (min (1- (fill-pointer array)) (1+ middle))))
                  ((< mid-value key)
                   (setf high (max 0 (1- middle))))
                  (t (return middle)))))))

 
; estos son los buenos, funcionan, pero no con 2 objetivos donde dentro de node-labels hay un c-vector-f

#|
(defun binary-search (array key ncosts &optional (low 0) (high (1- (fill-pointer array))))
  "find the position of key in a lexicographic order array - forward"
  (loop (when (<= high low) (let ((minimum (min high low)))
                              (if (< (aref (mo-search::vec (aref array minimum)) 0) key)
                                  (return (1+ minimum))
                                (return high))))
        (let* ((middle (floor (/ (+ low high) 2)))
               (mid-value (aref (mo-search::vec (aref array middle)) 0)))          
          (cond ((< mid-value key)
                 (setf low (min (1- (fill-pointer array)) (1+ middle))))
                ((> mid-value key)
                 (setf high (max 0 (1- middle))))
                (t (return middle))))))

(defun binary-search-backward (array key ncosts &optional (low 0))
  "find the position of key in a lexicographic order array - backward"
  (let ((high (1- (fill-pointer array))))
    (loop (when (<= high low) (let ((maximum  (max high low)))
                                (if (>= (aref (mo-search::vec (aref array maximum)) 1) key) 
                                    (return (1+ maximum))
                                  (return maximum))))
          (let* ((middle (floor (/ (+ low high) 2)))
                 (mid-value (aref (mo-search::vec (aref array middle)) 1)))
            (cond ((> mid-value key)
                   (setf low (min (1- (fill-pointer array)) (1+ middle))))
                  ((< mid-value key)
                   (setf high (max 0 (1- middle))))
                  (t (return middle)))))))
|#

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
#|
(defun find-previous-open-label (node-labels g low-i &optional (high (1- (fill-pointer node-labels))))
  "Function to find previous vector in lexicographic order with respect to
the one given by paramenter"
    (let* ((key (aref g 0))
           (index (binary-search node-labels key 3 low-i high))
           (index-to-compare (if (or (= index (fill-pointer node-labels)) 
                                     (> (aref (aref node-labels index) 0) key))
                                     ;(> (aref (vec (aref node-labels index)) 0) key))
                                 (max 0 (1- index)) index)))
      (multiple-value-bind (d? e?)
          (dominate-or-equals-2v (aref node-labels index-to-compare) g)
        (values d? e? (aref node-labels index-to-compare) index))))
|#

(defun insert-and-filter-t-costs (node-labels v)
  "Function to find previous vector in lexicographic order with respect to 
the one given by paramenter, insert it and remove the ones dominated by it"
  (if (zerop (fill-pointer node-labels))
      (vector-push-extend v node-labels)
    (let* ((key (aref v 0))
           (key2 (aref v 1))
           (index (binary-search node-labels key 3 0 (1- (fill-pointer node-labels))))
           (index-to-compare (if (or (= index (fill-pointer node-labels)) 
                                     (> (aref (aref node-labels index) 0) key))
                                 (max 0 (1- index)) index)))
      (when (not (equalp (aref node-labels index-to-compare) v)) ; repeated costs are discarded 
        (if (= (fill-pointer node-labels) index)
            (vector-push-extend v node-labels) ; insert the last, no dominated will be removed
          (let* ((index2 (binary-search-backward node-labels key2 3 index))
                 (index-to-compare2 (if (or (= index2 (fill-pointer node-labels)) 
                                            (< (aref (aref node-labels index2) 1) key2))
                                        (max 0 (1- index2)) index2)))
            (if (= (fill-pointer node-labels) index-to-compare2)
                (vector-push-extend v node-labels) ; insert the last, no dominated will be removed
              (if (dominates v (aref node-labels index-to-compare2))
                  (setf node-labels 
                        (insert-and-remove-dom node-labels index index2 v))
                (setf node-labels (insert-into-array node-labels v index2)))))))))
    node-labels)

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
#|
(defun find-first-posible-dominated-label (node-labels g low-i)
  "Function to find insert index for g in node-labels of a node"
  (if (= (fill-pointer node-labels) low-i) ; not open nodes
      (values T nil low-i)
    (let* ((key (aref g 1))
           (index (binary-search-backward node-labels key 3 low-i))
           (index-to-compare (if (or (= index (fill-pointer node-labels)) 
                                     (< (aref (aref node-labels index) 1) key))
                                 (max 0 (1- index)) index))
           (p? (dominates g (aref node-labels index-to-compare)))
           (l? (= (fill-pointer node-labels) index-to-compare)))
      (values l? p? index))))
|#

