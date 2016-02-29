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

Description:   State space of 2d square grids. It is used to test 
               algorithms and check their correctness. It is ready to 
               be used with n costs
-------------------------------------------------------------------
-----------------------------------------------------------------|#

(in-package :GRIDS)

#|---------------------------------------------------------------------
Grid class. Each matrix element has two arcs, 
1) the one going upwards (previous row)
2) the one going to the right (next column)
-----------------------------------------------------------------------|#
(defclass grid ()
  ((matrix :reader matrix :initarg :matrix)
   (nc :reader nc :initarg :nc)
   (start  :reader start  :initarg :start)
   (goal   :reader goal   :initarg :goal))
  (:documentation "2d grid represented by a matrix, start and goal positions"))

(defclass state ()
  ((cont :reader cont :initarg :cont))
  (:documentation "state inside of the grid (x,y)"))

(defun create-grid (matrix nc start goal)
  (make-instance 'grid :matrix matrix :nc nc :start start :goal goal))

(defun make-state (&rest coord)
  (make-instance 'state 
                 :cont (make-array (length coord) 
                                   :element-type *int-type*
                                   :initial-contents coord)))

(defun make-matrix (d1 d2 &key (nc 2) (maxc 10))
  "make a random matrix with dimensions d1 d2 and 2 costs by default"
  (let ((m (make-array (list d1 d2) :initial-element nil)))
    (labels ((cost-vec (&optional (v nil) (n 0)) "vector of random ncosts between 1 and maxc"
                           (cond ((= n nc) v)
                                 (t (cost-vec (cons (1+ (random maxc)) v) (1+ n)))))
             (2costs ()
                      (make-array 2 :initial-contents 
                                  (list (make-array nc 
                                                    :element-type *int-type*
                                                    :initial-contents (cost-vec))
                                        (make-array nc 
                                                    :element-type *int-type*
                                                    :initial-contents (cost-vec))))))
      (dotimes (f (array-dimension m 0) m)
        (dotimes (c (array-dimension m 1))
          (setf (aref m f c) (2costs)))))))


(defmethod c1 ((x state)) 
  (aref (cont x) 0))

(defmethod c2 ((x state)) 
  (aref (cont x) 1))

(defmethod show-state ((s state))  ; (Row , Column)
  (format t "|(~A,~A)|~%" (aref (cont s) 0) (aref (cont s) 1)))

(defmethod show-grid ((g grid))
  "show the grid on screen, each line an arc"
  (let* ((lab (matrix g))
         (ncosts (nc g))
         (start (start g))
         (goal (goal g))
         (nr (array-dimension lab 0))
         (nc (array-dimension lab 1))
         (array-2d-temp (make-array (list nr nc))))
    (format t "Start:  (~A,~A).~%" (aref (cont start) 0) (aref (cont start) 1))
    (format t "Goal:   (~A,~A).~%" (aref (cont goal) 0) (aref (cont goal) 1))
    (format t "Ncosts:  ~A.~%" ncosts)  
    (dotimes (f nr)
      (dotimes (c nc)
        (if (and (< f (1- nr)) (< c (1- nc)))
            (setf (aref array-2d-temp f c) 
                  (list (list (list (1+ f) c) (aref (aref lab f c) 0))
                        (list (list f (1+ c)) (aref (aref lab f c) 1))))
          (if (< f (1- nr))
              (setf (aref array-2d-temp f c) 
                    (list (list (list (1+ f) c) (aref (aref lab f c) 0))))
            (if (< c (1- nc))
                (setf (aref array-2d-temp f c) 
                      (list (list (list f (1+ c)) (aref (aref lab f c) 1)))))))))
    (dotimes (i nr) (dotimes (j nc)
          (let* ((arc (aref array-2d-temp i j))
                 (first-arc (first arc))
                 (second-arc (second arc)))
            (when first-arc
              (format t "(~A,~A)->~A: ~A. ~%" i j (first first-arc) (second first-arc))) 
            (when second-arc
              (format t "(~A,~A)->~A: ~A. ~%" i j (first second-arc) (second second-arc))))))))

(defmethod show-grid-start-goal ((g grid))
  "show the grid (only from start to goal) in a graphic way"
  (let* ((lab (matrix g))
         (ncosts (nc g))
         (start (start g))
         (goal (goal g))
         (nr (- (c1 goal) (c1 start)))
         (nc (- (c2 goal) (c2 start)))
         (arc-hash (make-hash-table :test #'equalp)))
    (format t "Start:  (~A,~A).~%" (aref (cont start) 0) (aref (cont start) 1))
    (format t "Goal:   (~A,~A).~%" (aref (cont goal) 0) (aref (cont goal) 1))
    (format t "Ncosts:  ~A.~%~%" ncosts)  
    ; writing all the values to print in a hash table
    (dotimes (f nr)
      (dotimes (c nc)
        (let* ((f-start (c1 start))
               (real-f (+ f f-start))
               (c-start (c2 start))
               (real-c (+ c c-start)))
          (setf (gethash (list (list real-f real-c) (list (1+ real-f) real-c)) arc-hash) (aref (aref lab real-f real-c) 0)
                (gethash (list (list real-f real-c) (list real-f (1+ real-c))) arc-hash) (aref (aref lab real-f real-c) 1)))))
    ; printing that values
    (dotimes (f nr)
      (dotimes (c nc)
        (let* ((f-start (c1 start))
               (real-f (+ f f-start))
               (c-start (c2 start))
               (real-c (+ c c-start)))
          (format t "     X   |~A|   " (gethash (list (list real-f real-c) (list real-f (1+ real-c))) arc-hash))))
      (format t "X~%" )
        ; PRINT THE LINE IN BETWEEN
      (dotimes (c nc)
        (format t "     |----------------"))
      (terpri)
      (dotimes (c nc)
        (let* ((f-start (c1 start))
               (real-f (+ f f-start))
               (c-start (c2 start))
               (real-c (+ c c-start)))
          (format t "|~A|            " (gethash (list (list real-f real-c) (list (1+ real-f) real-c)) arc-hash))))
      (format t "X~%" )
        ; PRINT THE LINE IN BETWEEN
      (dotimes (c nc)
        (format t "     |----------------"))
      (terpri))
    (terpri)))

(defmethod key ((x state)) 
  (cont x))

(defmethod cost ((e state) (e2 state) (g grid))
  "vector de coste de la transición e -> e2 en el laberinto m si es posible"
  (let* ((ncosts (nc g))
         (f (c1 e))
         (c (c2 e))
         (f2 (c1 e2))
         (c2 (c2 e2))
         (c-vectors (aref (matrix g) (min f f2) (min c c2)))
         (result nil))
    (when (= 1 (+ (abs (- f f2)) (abs (- c c2))))  ;son 4-vecinos
      (if (= c c2) 
          (setf result (aref c-vectors 0))
        (setf result (aref c-vectors 1))))
    (when (= ncosts 1) (setf result (aref result 0)))
    result))

(defun in? (m f c)
  (and (<= 0 f (1- (array-dimension m 0)))
       (<= 0 c (1- (array-dimension m 1)))))

(defmethod expand ((e state) (g grid)) 
  "expand state e to generate its successors in grid g"
  (let ((f (c1 e))
        (c (c2 e))
        (lista nil))
    (when (in? (matrix g) (1- f) c) (push (make-state (1- f) c) lista))
    (when (in? (matrix g) f (1+ c)) (push (make-state f (1+ c)) lista))
    (when (in? (matrix g) (1+ f) c) (push (make-state (1+ f) c) lista))
    (when (in? (matrix g) f (1- c)) (push (make-state f (1- c)) lista))
    lista))

(defmethod goal? ((e-ini state) (e-fin state))
  (equalp (key e-ini) (key e-fin)))

(defmethod h ((e state) htable)  
  (let ((ke (key e)))
    (aref htable (aref ke 0) (aref ke 1)))) 
  ;(gethash (key e) htable))









































