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

Description:       All functions for vectors management
-------------------------------------------------------------------
------------------------------------------------------------------|#

(in-package :vectors)

#|---------------------------------------------------------------------
Functions to create vectors
-----------------------------------------------------------------------|#
(defun make-vector0 (long) 
  "vector with long 0 elements"
  (make-array long :initial-element 0))

(defun make-vector-inf (long) 
  "vector with long most-positive-fixnum elements"
  (make-array long :initial-element most-positive-fixnum))
              
#|---------------------------------------------------------------------
Arithmetic functions
-----------------------------------------------------------------------|#
(defmethod add-v ((v1 vector) (v2 vector)) 
  "v1 + v2"
  (let ((r (make-array (array-dimension v1 0) :element-type (array-element-type v1))))
    (dotimes (i (array-dimension v1 0) r) 
      (setf (aref r i) (+ (aref v1 i) (aref v2 i)))))) 

(defmethod sub-v ((v1 vector) (v2 vector)) 
  "v1 - v2"
  (let ((r (make-array (array-dimension v1 0) :element-type (array-element-type v1))))
    (dotimes (i (array-dimension v1 0) r)
      (setf (aref r i) (- (aref v1 i) (aref v2 i)))))) 

#|---------------------------------------------------------------------
Dominance functions
-----------------------------------------------------------------------|#
(defmethod dominates ((v1 vector) (v2 vector)) 
  "T if v1 strictly dominates v2"
  (and (dominate-or-equals v1 v2) (not (equalp v1 v2))))

(defmethod non-dominated ((v1 vector) (v2 vector)) 
  "v1 if V1 is non-dominated by V2, nil if v2 dom v1"
  (if (dominates v2 v1) nil v1))

(defmethod dominate-or-equals ((v1 vector) (v2 vector)) 
  "T if V1 dominates or equals V2. Equality is compared with ="
  (dotimes (i (array-dimension v1 0) T)
    (when (and (not (= (aref v1 i) (aref v2 i))) 
               (> (aref v1 i) (aref v2 i)))
      (return nil))))

(defmethod dominate-or-equals-2v ((v1 vector) (v2 vector)) 
  "Two values are returned, first if v1 dominates v2, second if they are equal"
  (let ((equal T))
    (dotimes (i (array-dimension v1 0) (values (not equal) equal))
      (when (not (= (aref v1 i) (aref v2 i))) 
        (setf equal nil)
        (when (> (aref v1 i) (aref v2 i))
          (return-from dominate-or-equals-2v (values nil equal)))))))

(defmethod dominance ((x vector) (y vector) &optional (i 0) (n (array-dimension x 0)) d12 d21)
  "d12 indicates if any xi < yi, d21 if any yi < xi. Returns 4 boolean values x<y, y<x, x=y, x ind y"
  (cond ((= i n) (values d12 d21 (not (or d12 d21)) nil))
        ((< (aref x i) (aref y i))
         (if d21 (values nil nil nil T)
           (dominance x y (1+ i) n T nil)))
        ((> (aref x i) (aref y i))
         (if d12 (values nil nil nil T)
           (dominance x y (1+ i) n nil T)))
        (t (dominance x y (1+ i) n d12 d21))))

#|---------------------------------------------------------------------
Dominance with list of vectors
-----------------------------------------------------------------------|#
(defmethod nondominated-lex-list ((v vector) lex-list) 
  "Return V vector whenever V is not dominated by any vector in lex-list. 
In lex-list vectors are sorted lexicographically"
  (cond ((null lex-list) v)
        ((dominates (car lex-list) v) nil)
        ((> (aref (car lex-list) 0) (aref v 0)) v)
        (T (nondominated-lex-list v (cdr lex-list)))))

(defmethod non-dominated-l ((v vector) l) 
  "T if V is non-dominated by any vector in l"
  (cond ((null l) v)
        ((dominates (car l) v) nil)
        (T  (non-dominated-l v (cdr l)))))

#|---------------------------------------------------------------------
Function to truncate a vector to drop its first component
-----------------------------------------------------------------------|#
(defmethod trunc-v ((v vector))
  "Truncate a vector to drop its first component"
  (subseq v 1))

#|---------------------------------------------------------------------
Comparison functions between vectors
; 1. BEST LEXICOGRAPHIC VECTOR 
; 2. BEST AGGREGATE LINEAR VECTOR 
; 3. BEST MIN-MIN VECTOR 
-----------------------------------------------------------------------|#
(defmethod b-lex ((v1 vector) (v2 vector) &key (test #'<))
  "T if v1 is lexicographically better than v2 when using test"
  (dotimes (i (array-dimension v1 0) nil)
    (when (funcall test (aref v1 i) (aref v2 i)) (return T))
    (when (funcall test (aref v2 i) (aref v1 i)) (return nil))))

(defmethod b-lin ((v1 vector) (v2 vector) &key (test #'<))
  "T if v1 is linearly aggregated better than v2 when using test"
  (let ((suma1 0) 
        (suma2 0))
    (dotimes (i (array-dimension v1 0))
      (incf suma1 (aref v1 i))
      (incf suma2 (aref v2 i)))
    (if (funcall test suma1 suma2) T nil))) 

(defmethod b-lex-linear ((v1 vector) (v2 vector))
  "v1 and v2 are compared lexicographically until part component, when equal f-values are compared as a linear aggregation"
  (let* ((v1-0 (subseq v1 0 2))
         (v1-1 (subseq v1 2 5))
         (v2-0 (subseq v2 0 2))
         (v2-1 (subseq v2 2 5))
         (lex (b-lex v1-0 v2-0)))
    (if lex T
      (if (b-lex v2-0 v1-0) nil
        (b-lin v1-1 v2-1)))))

