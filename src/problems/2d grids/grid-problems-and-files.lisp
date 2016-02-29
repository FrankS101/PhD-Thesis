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

Description:   Methods and functions to read and write grids from and to
               files, respectively.
-----------------------------------------------------------------------
---------------------------------------------------------------------|#

(in-package :GRIDS)

#|---------------------------------------------------------------------
  Write grid problem to disk
File format is the following one:
a 0 0 1 0 #(4 3) which indicates an arc from (0,0) to (1,0) which value is #(4,3)
Parameter:
- grid: Grid to write
- file: File path to write
-----------------------------------------------------------------------|#
(defmethod write-grid ((g grid) filepath)
  "write the grid into a file"
  (with-open-file (fi filepath :direction :output :if-exists :rename-and-delete
                     :if-does-not-exist :create)
    ; Problem features
    (format fi "Dimensions (~S ~S)~%~%" (array-dimension (matrix g) 0) (array-dimension (matrix g) 1))
    ; Printing arcs
    (let* ((lab (matrix g))
           (nf (array-dimension lab 0))
           (nc (array-dimension lab 1)))
      (dotimes (f nf)
        (dotimes (c nc)
          (if (and (< f (1- nf)) (< c (1- nc)))
              (progn 
                (format fi "a ~A ~A ~A ~A ~A~%" f c (1+ f) c (aref (aref lab f c) 0))
                (format fi "a ~A ~A ~A ~A ~A~%" f c f (1+ c) (aref (aref lab f c) 1)))
            (if (< f (1- nf))
                (format fi "a ~A ~A ~A ~A ~A~%" f c (1+ f) c (aref (aref lab f c) 0)) 
              (if (< c (1- nc))
                  (format fi "a ~A ~A ~A ~A ~A~%" f c f (1+ c) (aref (aref lab f c) 1))))))))))
           
#|---------------------------------------------------------------------
  Read grid problem from disk and returns a grid
File format is the following one:
a 0 0 1 0 #(4 3) which indicates an arc from (0,0) to (1,0) with value #(4,3)
Parameters:
- file-problem: File with the problem (start and goal nodes, number of costs)
- file-grid: File with the grid to be read
-----------------------------------------------------------------------|#
(defmethod read-grid-problem (file-problem file-grid)
  "read from file a grid and returns it"
  (with-open-file (fi file-problem :direction :input)
    (with-open-file (fi2 file-grid :direction :input)
      (let* ((ncosts (progn (read fi) (read fi)))
             (start (progn (read fi) (read fi)))
             (goal (progn (read fi) (read fi)))
             (dim (progn (read fi2) (read fi2)))
             (grid (create-grid (make-array dim :initial-element nil) 
                                ncosts 
                                (make-state (first start) (second start)) 
                                (make-state (first goal) (second goal)))))
        (do ((linea (read fi2 nil :eof) (read fi2 nil :eof)))
            ((eq :eof linea))
          (when (equalp (symbol-name linea) "a")
            (let ((f-ini (read fi2))
                  (c-ini (read fi2))
                  (f-obj (read fi2))
                  (c-obj (read fi2))
                  (vec (read fi2)))
              (declare (ignore c-obj))
;
;              (setf vec (concatenate 'vector (subseq vec 1 2) (subseq vec 2 5)))
;              
              (when (null (aref (matrix grid) f-ini c-ini)) 
                (setf (aref (matrix grid) f-ini c-ini) (make-array 2))) 
              (setf (aref (aref (matrix grid) f-ini c-ini) (if (> (- f-obj f-ini) 0) 0 1)) (subseq vec 0 ncosts)))))
        grid))))

#|---------------------------------------------------------------------
  Create 2d-grid problems with start node in the center of the grid
and goal node in the diagonal.
Parameters:
- ncosts: Number of costs for each arc
- psizes: Size of the grid, i.e. (100 100)
- nproblems: Number of problems for each depth solution given
- Depth solution: States the depth where the goal node is located, i.e. 
start node (0,0), depth solution: 10 -> goal node in (5,5)
- 1st-prob-num: first number to start numbering problems  
-----------------------------------------------------------------------|#
(defun create-grid-problems (ncosts psizes nproblems depth-solution path &optional (1st-prob-num 0))
  "Create grid problems. The number of costs, size of the grid and the number of problems are given as parameters "
  (let ((cont 1st-prob-num))
    (dolist (s psizes 'GRID-PROBLEMS-CREATED)
      (dotimes (n nproblems)
        (let* ((global-path cl-user::*root*)
               (dim s)
               (grid nil)
               (center-x (1- (floor (/ (car dim) 2)))) ;1- since nodes are numbered from 0
               (center-y (1- (floor (/ (cadr dim) 2))))
               (goal-x (+ center-x (/ depth-solution 2)))
               (goal-y (+ center-y (/ depth-solution 2))))
          (setf grid (create-grid (make-matrix (car dim) (cadr dim) :nc ncosts) 
                                  ncosts
                                  (make-state center-x center-y)
                                  (make-state goal-x goal-y)))
          (write-grid grid (concatenate 'string global-path path 
                                                (concatenate 'string "Grid-Problem" (write-to-string cont) ".txt")))
          (incf cont))))))

#|---------------------------------------------------------------------
  Function to create a test set of 50 problems used for phD chapter 
  This set has 5 problems for each solution depth, from 10 to 100, grid size
  is 100x100.
  Notice that problems with 100 solution depth, goal state should be located in (0,0)
  since coordinate 100x100 does not exist. This has to be changed manually!!
  Parameters:
  - ncosts 
  - grid-size
  - nproblems
  - ini-depth
  - last-depth
  - depth-step
  - path: Folder to write files. It will be concatenated to the global path.
  It is created the same kind of test set that was used in experiments by default.
  Note: The folder in path must exist previously. 
-----------------------------------------------------------------------|#
(defun create-full-test-set-50 (&optional (ncosts 3) (grid-size (list '(100 100))) (nproblems 5) 
                                          (ini-depth 10) (last-depth 100) (depth-step 10)
                                          (path "Test-cases\\Grids\\")) 
  "Function to create a test set of 50 problems with 5 problems for each solution depth (10-100)"
  (let ((problem 0))
    (loop for s-depth from ini-depth to last-depth by depth-step do
          (create-grid-problems ncosts grid-size nproblems s-depth path problem)
          (incf problem nproblems))
    'PROBLEM-FILES-CREATED))
; (create-full-test-set-50)

#|---------------------------------------------------------------------
  Function to create 4 different kinds of grid problems.
- 1. Corner to corner of the grid.
- 2. Side to side 
- 3. diagonal "without matrix limits"
- 4. on a line "without matrix limits"
-----------------------------------------------------------------------|#            
(defun create-different-grid-problems (ncosts psizes nproblems sort-of-problem &optional 
                                              (path "Test-cases\\Grids\\") (first-problem-number 0) depth-solution)
  "Function to create 4 different kinds of grid problems."
  (let ((cont (+ -1 first-problem-number)))
    (dolist (s psizes)
      (dotimes (n nproblems)
        (incf cont)
        (let ((global-path cl-user::*root*)
              (dim s)
              (grid nil))
          (cond ((= sort-of-problem 1) ; corner to corner
                 (setf grid (create-grid (make-matrix (car dim) (cadr dim)) ncosts
                                         (make-state 0 0) 
                                         (make-state (1- (car dim)) (1- (cadr dim))))))    
                ((= sort-of-problem 2) 
                ; side to side, similar to corner to corner but both, start and goal, are located in the half 
                ; of opposite sides of square matrix
                 (setf grid (create-grid (make-matrix (car dim) (cadr dim)) ncosts
                                         (make-state 0 (1- (floor (/ (car dim) 2)))) 
                                         (make-state (1- (cadr dim)) (1- (floor (/ (car dim) 2)))))))
                ((= sort-of-problem 3) 
                ; diagonal "without matrix limits" (it means that search shouldn't reach limits of the matrix). 
                ; Start node is located in matrix center and goal node is depth-solution arcs away from start (increasing axes)
                 (let* ((center-x (1- (floor (/ (car dim) 2))))
                        (center-y (1- (floor (/ (cadr dim) 2))))
                        (goal-x (+ center-x (floor (/ depth-solution 2))))
                        (goal-y (+ center-y (floor (/ depth-solution 2)))))
                   (when (oddp depth-solution)
                     (error "La profundidad de la solución con los extremos situados en diagonal debe ser par.~%"))
                   (setf grid (create-grid (make-matrix (car dim) (cadr dim)) ncosts
                                           (make-state center-x center-y) 
                                           (make-state goal-x goal-y)))))
                ((= sort-of-problem 4) 
                ; on a line "without matrix limits" (it means that search shouldn't reach limits of the matrix)
                ; Start node is located in matrix center and goal node is depth-solution arcs away from start (increasing axes)
                 (let* ((center-x (1- (floor (/ (car dim) 2))))
                        (center-y (1- (floor (/ (cadr dim) 2))))
                        (goal-x (+ center-x depth-solution))
                        (goal-y (+ center-y depth-solution)))
                   (setf grid (create-grid (make-matrix (car dim) (cadr dim)) ncosts
                                           (make-state center-x center-y)
                                           (make-state goal-x goal-y))))))               
          ; write problems in files
          (write-grid grid (concatenate 'string global-path path 
                                                (concatenate 'string "Grid-Problem" (write-to-string cont) ".txt"))))))
    'PROBLEM-FILES-CREATED))
