#|----------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
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

Description:   File management for Dimacs problems
-------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------|#

(in-package :DIMACS)

(defparameter *DIMACS-FILES-PATH* (concatenate 'string cl-user::*FILES-PATH* "dimacs/"))
(defparameter *DIMACS-PROBLEMS-PATH* (concatenate 'string *DIMACS-FILES-PATH* "problems/"))
(defparameter *DIMACS-COORDS-PATH* (concatenate 'string *DIMACS-FILES-PATH* "coordinates/"))
(defparameter *DIMACS-DISTANCES-PATH* (concatenate 'string *DIMACS-FILES-PATH* "distances graphs/"))
(defparameter *DIMACS-TTIMES-PATH* (concatenate 'string *DIMACS-FILES-PATH* "travel times graphs/"))
(defparameter *DIMACS-ECOSTS-PATH* (concatenate 'string *DIMACS-FILES-PATH* "economic costs graphs/"))
(defparameter *DIMACS-MAP-PROBLEMS* NIL) 
(defparameter *DIMACS-LOADED-MAP* NIL)

#|---------------------------------------------------------------------------------------
Functions to read DIMACS problems and related files         
The general idea is to preload A map in AN EXE file in order to avoid loading map files for
each execution, since execution times for hard problems can be affected by the garbage
collector executed between executions of different queries 
Parameters:
- file-gr1: economic-cost file
- file-gr2: distances file
- file-gr3: travel times file
- last-node-index: in case the map to read corresponds to a cut of an original one, the last node
  index of the original map must be provided
- print-stats: print on screen some information about the process of reading the files
----------------------------------------------------------------------------------------|#
(defun read-dimacs-files (file-gr1 file-gr2 file-gr3 file-co &optional last-node-index (print-stats)) 
  "Read arc costs from DIMACS files."
  (let* ((gr1-ht  (read-gr file-gr1 print-stats)) ; m-file
         (gr2-ht  (read-gr file-gr2 print-stats)) ; d-file
         (gr3-ht  (read-gr file-gr3 print-stats)) ; t-file 
         (coords-ht (coords-file-to-ht file-co))
         (nnodes (hash-table-count coords-ht))
         (narcs (hash-table-count gr1-ht))
         (nodes)
         (rep-arcs 0)
         (dif-arcs 0)
         (realnarcs 0))
    (setf nodes (make-array (if last-node-index last-node-index nnodes)))
    (when (or (null (equal-hash-tables-keys gr1-ht gr2-ht))
              (null (equal-hash-tables-keys gr1-ht gr3-ht))) 
      (format t "Error detected when checking that all arcs in files are the same.~%")
      (abort))
    (maphash #'(lambda (arc c1) 
                 (let ((c2 (gethash arc gr2-ht))
                       (c3 (gethash arc gr3-ht))
                       (v1 (first arc))
                       (v2 (second arc)))
                   (when (or (null c1) (null c2) (null c3))           
                     (format t "Error detected. Arc (~s ~s) is missing.~%" v1 v2)
                     (abort))
                   (let ((is-there?)
                         (position)
                         (n 0))
                     (dolist (c (aref nodes (1- v1)))
                       (if (= (car c) (1- v2)) (setf is-there? (car (cdr c))
                                                     position n)
                         (incf n)))
                     (if is-there?  ; Repeated
                         (if (equalp is-there? (make-array 3 :initial-contents 
                                                           (list c1 c2 c3)
                                                           :element-type '(unsigned-byte 32)))
                             (incf rep-arcs)
                           (incf dif-arcs))
                       (incf realnarcs))                            
                     (if (null is-there?) ; not repeated
                         (setf (aref nodes (1- v1))
                               (concatenate 'list (aref nodes (1- v1)) 
                                            (list (list (1- v2) 
                                                        (make-array 3 :initial-contents 
                                                                    (list c1 c2 c3)
                                                                    :element-type '(unsigned-byte 32))))))
                       ;repeated, change
                       (setf (nth position (aref nodes (1- v1))) (list (1- v2) is-there?))))))
             gr1-ht)
    (when print-stats
      (format t "~%Repeated arcs with same value:        ~S" rep-arcs)
      (format t "~%Repeated arcs with different values:  ~S" dif-arcs)
      (format t "~%Real number of arcs processed:        ~S" realnarcs))
    ; Return SS object
    (make-instance 'ss-dimacs :nnodes nnodes :narcs narcs :nodes nodes)))

(defun read-problem-file-line (line)
  "Number of problem, initial and destination nodes are returned."
  (multiple-value-bind (p pos1) (read-from-string line nil nil)
      (multiple-value-bind (ini pos2) (read-from-string line nil nil :start pos1)
            (let ((end (read-from-string line nil nil :start pos2)))
              (values p ini end)))))

#|
1. ecosts
2. distances
3. times
|#

#|
1. ecosts
2. times
3. distances
|#

#|
1. distances
2. times
3. ecosts
|#

(defun read-problems-file (map-string &optional 
                                      (realnnodes) ; to create a bigger array whenever we are dealing with a trimmed map
                                      (print-stats) 
                                      (distances-gr (concatenate 'string *DIMACS-DISTANCES-PATH* "USA-road-d." map-string ".gr"))
                                      (ecosts-gr (concatenate 'string *DIMACS-ECOSTS-PATH* "USA-road-m." map-string ".gr"))
                                      (times-gr (concatenate 'string *DIMACS-TTIMES-PATH* "USA-road-t." map-string ".gr"))
                                      (coords-f (concatenate 'string *DIMACS-COORDS-PATH* "USA-road-d." map-string ".co")))
  "Read problems of a map from files in disk and they are returned in an array"
  (let ((file (concatenate 'string *DIMACS-PROBLEMS-PATH* map-string ".problems.txt")))
    (with-open-file (f file)  
      ; read number of problems and create an array of problems with that size
      (let ((ss (read-dimacs-files times-gr distances-gr  ecosts-gr coords-f realnnodes print-stats))
;      (let ((ss (read-dimacs-files distances-gr times-gr ecosts-gr coords-f realnnodes print-stats))
            (array-prob (make-array (read-problem-file-line (read-line f nil :eof))
                                    :element-type (class-of 'dimacs))))
        (do ((line (read-line f nil :eof)
                   (read-line f nil :eof)))
            ((eq line :eof))
          (multiple-value-bind (p ini end) (read-problem-file-line line)
            (setf (aref array-prob (1- p)) 
                  (make-instance 'dimacs
                                 :ss ss
                                 :ini-s ini
                                 :end-s end
                                 :ncost 3))))
        array-prob))))

(defun create-file-with-random-problems (co-file nproblems map-string)
  "Write a given number of problems in a file according to the arcs in gr-file, preventing an inexistent node is selected"
  (let* ((nodes-ht (coords-file-to-ht co-file))
         (nnodes (hash-table-count nodes-ht))
         (v1) (v2))
    (with-open-file (f (concatenate 'string (directory-namestring co-file) map-string ".problems.txt")
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (format f "~s~%" nproblems)
      (loop for n from 1 to nproblems do
            ; random start and goal are generated taken into account that they are in nodes-ht (to prevent errors in trimmed maps)
            (loop do (setf v1 (random nnodes))
                  while (null (gethash v1 nodes-ht)))
            (loop do (setf v2 (random nnodes))
                  while (null (gethash v2 nodes-ht))) 
            (let ((start v1)
                  (goal v2))
              (format f "~s ~s ~s ~%" n start goal))))))
; (create-file-with-random-problems (capi:prompt-for-file "") 20 "NY")
; (create-file-with-random-problems (capi:prompt-for-file "") 50 "DE")

#|---------------------------------------------------------------------
LOAD MAPS IN MEMORY
Parameters: 
- map-key: keyword corresponding to the map to read from file and load
-----------------------------------------------------------------------|#
(defun load-map (map-key)
  "load the map corresponding to map-key into memory"
  (unless (equalp map-key *DIMACS-LOADED-MAP*)
    (cond ((equalp map-key :VT2) (setf *DIMACS-MAP-PROBLEMS* (read-problems-file "VT2" 97975)
                                       *DIMACS-LOADED-MAP* :VT2))
          ((equalp map-key :NY2) (setf *DIMACS-MAP-PROBLEMS* (read-problems-file "NY2" 264346)
                                       *DIMACS-LOADED-MAP* :NY2))
          (T
           (setf *DIMACS-MAP-PROBLEMS* (read-problems-file (symbol-name map-key))
                                      *DIMACS-LOADED-MAP* map-key)))))

; (load-map :VT2)
; (load-map :NY)
; (load-map :NY2)
; (load-map :DE)
; (load-map :DC)
; (load-map :NJ)

(defun matlab-pareto (list)
  (format t "x=[")
  (loop for x in list
        do (format t "~A " (aref x 0)))
  (format t "];~%y=[")
  (loop for x in list
        do (format t "~A " (aref x 1)))
  (format t "];"))
