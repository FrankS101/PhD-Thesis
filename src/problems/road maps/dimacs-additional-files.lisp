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

Description:   Functions to create additional files to DIMACS maps.
               For instance, an economic-cost graph, road-type graph,
               correlations between objectives. To do so, invoke
               function (generate-additional-files)
-------------------------------------------------------------------
-----------------------------------------------------------------|#

(in-package :DIMACS)

(defconstant *economic-cost-tolls* 1.86)
(defconstant *litres-km-10* 0.0871)
(defconstant *litres-km-8* 0.0784)
(defconstant *litres-km-6* 0.0784)
(defconstant *litres-km-4* 0.0811)

(defun read-dimacs-file-line (line)
  "if line belongs to the definition of a vertex (v), (vertex coord1 coord2) is returned, if it is an arc (a), (v1 v2 cost) is returned,
otherwise NIL."
  (multiple-value-bind (desc pos1) (read-from-string line nil nil)
    (when (or (equal (write-to-string desc) "V") 
              (equal (write-to-string desc) "A"))
      (multiple-value-bind (e1 pos2) (read-from-string line nil nil :start pos1)
          (multiple-value-bind (e2 pos3) (read-from-string line nil nil :start pos2)
            (let ((e3 (read-from-string line nil nil :start pos3)))
              (values e1 e2 e3)))))))


(defun read-gr (gr-file &optional (print-stats))
  "Returns a hash table with values corresponding to the arcs stored in the provided file and arcs positions in file"
  (let ((gr-filename (pathname-name gr-file))
        (ht (make-hash-table :test #'equal))
        (arc-pos) ; to keep the same arc order when printing new files
        (rep  0))
    (with-open-file (f gr-file :direction :input)
      (do ((line (read-line f nil :eof)
                 (read-line f nil :eof)))
          ((eq line :eof))
        (multiple-value-bind (node1 node2 cost) (read-dimacs-file-line line)
          (when cost
            (multiple-value-bind (value exists?) (gethash (list node1 node2) ht)
              (declare (ignore value))
              (cond (exists?      ; it is already in the hash table
                     (incf rep))  ; (first appearance in file is stored)
                    (t
                     (push (list node1 node2) arc-pos)
                     (setf (gethash (list node1 node2) ht) cost))))))))
    (when print-stats
      (format t "~% File ~S read." gr-filename)
      (format t "~% -) # of repeated arcs in file:     ~s" rep))
    (values ht (reverse arc-pos))))

(defun calculate-economic-cost (distance road-type)
  (floor (* distance (+ (economic-cost-tolls road-type)
                         (economic-cost-gas road-type)))))

(defun economic-cost-tolls (road-type)
  (if (= road-type 10)
      *economic-cost-tolls*
    0))

(defun economic-cost-gas (road-type)
  (case road-type
    (10 *litres-km-10*)
    (8 *litres-km-8*)
    (6 *litres-km-6*)
    (4 *litres-km-4*)))

(defun categorize-road (distance time)
  "Approximate the corresponding factor to road type: 10, 8, 6, 4"
(let ((value (if (zerop time) 0
               (round (* 10  (/ distance time))))))
    (cond ((> value 9) 10)
          ((and (>= 9 value) (> value 7)) 8)
          ((and (>= 7 value) (> value 5)) 6)
          (T 4))))

(defun avg-gr (gr-file)
  "Returns values avg, max, and min of costs stored in gr-file"
  (let ((avg 0)
        (max   most-negative-long-float)
        (min   most-positive-long-float)
        (n 0))
    (with-open-file (f gr-file :direction :input)
      (do ((line (read-line f nil :eof)
                 (read-line f nil :eof)))
          ((eq line :eof) 
           (format t "~%~6,2f" (/ avg n))
           (values (/ avg n) min max))
        (multiple-value-bind (node1 node2 cost) (read-dimacs-file-line line)
          (declare (ignore node1 node2))
          (when cost
            (incf avg cost)
            (setf min (min min cost)
                  max (max max cost))
            (incf n)))))
    (values max min avg)))

(defun pearson (gr-file1 gr-file2)
  "Calculate Pearson correlation coefficient of gr-file1 and gr-file2"
  (let ((ht1 nil)
        (ht2 nil))   
    (format t "~%Reading gr-file1...")
    (setf ht1 (read-gr gr-file1))
    (format t "~%Reading gr-file2...")
    (setf ht2 (read-gr gr-file2))
    (pearson-ht ht1 ht2)))

(defun road-type (d-file t-file r-file r-filename distances-ht times-ht arcs-order nnodes)
  ""
  (let ((narcs (length arcs-order)))
    (format t "~%Writing road type file ~a.gr" r-filename)
    (with-open-file (fout r-file 
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
      (format fout "c Road types file generated from files:")
      (format fout "~%c Spatial distances ~a" d-file)
      (format fout "~%c Travel times ~a" t-file)
      (format fout "~%p sp ~s ~s" nnodes narcs)
      (format fout "~%c graph contains ~s nodes and ~s arcs" nnodes narcs)
      (format fout "~%c")
      (let ((n-arcs-road-types-ht (make-hash-table :test #'eql))  ; # of arcs of each road type
            (arcs-road-type-ht (make-hash-table :test #'equal)))  ; For each arc its road type is stored
        (dolist (arc arcs-order)
          (let* ((distance (gethash arc distances-ht))
                 (time (gethash arc times-ht)))
            (cond ((and distance time)
                   (let ((road-type (categorize-road distance time)))
                     (format fout "~%a ~d ~d ~d" (car arc) (cadr arc) road-type)
                     (setf (gethash arc arcs-road-type-ht) road-type)
                     (setf (gethash road-type n-arcs-road-types-ht) (if (gethash road-type n-arcs-road-types-ht)
                                                                        (incf (gethash road-type n-arcs-road-types-ht))
                                                                      1))))
                  (t (format t "~% x) Arc is missing: ~s Distance: ~s Time:~s" arc distance time)))))
        (format t "~% -) Road-types found:")
        (maphash #'(lambda (key value) 
                     (format t "~%    ~s : ~s" key value))
                 n-arcs-road-types-ht)
        arcs-road-type-ht))))
    

(defun road-ecost (d-file fichero-road-types m-file m-filename distances-ht arcs-road-type-ht arcs-order nnodes)
    ""
    (format t "~%Writing economic cost file ~a.gr" m-filename)
    (let ((ecost-ht (make-hash-table :test #'equalp))  ; For each arc its economic cost is stored
          (narcs (length arcs-order)))
      (with-open-file (fout m-file 
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
        (format fout "c Economic costs file generated from files:")
        (format fout "~%c Distances ~a" d-file)
        (format fout "~%c Road-types ~a" fichero-road-types)
        (format fout "~%c File generated with cost of driving as cents of Dollar * 10000")
        (format fout "~%p sp ~s ~s" nnodes narcs)
        (format fout "~%c graph contains ~s nodes and ~s arcs" nnodes narcs)
        (format fout "~%c")
        (dolist (arc arcs-order)
          (let* ((distance (gethash  arc distances-ht))
                 (road-type (gethash arc arcs-road-type-ht)))
            (cond ((and distance road-type)
                   (let ((ecost (calculate-economic-cost distance road-type)))
                     (setf (gethash arc ecost-ht) ecost)
                     (format fout "~%a ~d ~d ~d" (car arc) (cadr arc) ecost)))
                  (t (format t "~% x) Arc is missing: ~s Distance: ~s Road-type:~s" arc distance road-type))))))
      ecost-ht))

(defun coords-file-to-ht (&optional (fco (capi::prompt-for-file "Select a DIMACS file (.co)")))
  "Reads a DIMACS coordinates file and returns a hash table which contains for each node a list with its coordinates. Nodes are numbered from 1 to hash-table-count"
  (let ((ht (make-hash-table :test #'eql)))
    (with-open-file (fin fco :direction :input)
      (do ((line (read-line fin nil :eof)
                    (read-line fin nil :eof)))
            ((eq line :eof) T)
        (multiple-value-bind (node coord1 coord2) (read-dimacs-file-line line)
          (when node
            (setf (gethash node ht) (list coord1 coord2))))))
    ht))

(defun generate-additional-files (&optional (co-file (capi::prompt-for-file "Coordinates File")) (nprob 50))
  "Generates road-types and economic costs files. Moreover, stats and matlab files to draw the maps are provided.x"
  (let* ((co-filename    (pathname-name co-file))    ; coordinates
         (d-filename     co-filename)                ; spatial distances
         (t-filename     (copy-seq d-filename))      ; travel times
         (r-filename     (copy-seq d-filename))      ; road type
         (m-filename     (copy-seq d-filename))      ; economic cost
         (c-filename     (copy-seq d-filename))      ; correlation between costs
         (position       (search "-d." co-filename))   
         (matlab-filename  (subseq co-filename (+ position 3))))  
    ;generate all filenames 
    (setf (subseq t-filename position) "-t.")        ; travel time
    (setf (subseq r-filename position) "-r.")        ; road type
    (setf (subseq m-filename position) "-m.")        ; economic cost
    (setf (subseq c-filename position) "-c.")        ; correlations  
    (let ((d-file   (make-pathname :name d-filename :type "gr" :defaults co-file))
          (t-file   (make-pathname :name t-filename :type "gr" :defaults co-file))
          (r-file   (make-pathname :name r-filename :type "gr" :defaults co-file))
          (m-file   (make-pathname :name m-filename :type "gr" :defaults co-file))
          (log-file (make-pathname :name c-filename :type "txt" :defaults co-file)))
        ;0) Read in hash tables file data
      (let* ((times-ht) 
             (roads-ht) 
             (costs-ht) 
             (coords-ht)
             (nnodes))          
          (format t "Reading spatial distances from file ~a.gr ..." d-filename)
          (multiple-value-bind (distances-ht arcs-order) 
              (read-gr d-file T)
            (format t "~% --> ~s arcs stored~%" (hash-table-count distances-ht))
            (format t "======================================================================")
            (format t "~%Reading travel times from file ~a.gr ..." t-filename)
            (setf times-ht (read-gr t-file T))
            (format t "~% --> ~s arcs stored~%" (hash-table-count times-ht))
            (format t "======================================================================")
            (format t "~%Checking arcs (distances and times)... ")
            (when (equal-hash-tables-keys distances-ht times-ht)
              (format t " OK.~%" ))
            (format t "======================================================================")
            (format t "~%Reading coordinates from file ~a.co ..." co-filename)
            (setf coords-ht (coords-file-to-ht co-file))
            (setf nnodes (hash-table-count coords-ht)) 
            (format t "~% --> ~s nodes stored~%" (hash-table-count coords-ht))
            (format t "======================================================================")
          ; 1) Generate road type file
            (setf roads-ht (road-type d-file t-file r-file r-filename distances-ht times-ht arcs-order nnodes))
            (format t "~% --> ~s road type arcs generated~%" (hash-table-count roads-ht))
            (format t "======================================================================")
            (format t "~%Checking arcs (distances y road types)...")
            (when (equal-hash-tables-keys distances-ht roads-ht)
              (format t " OK.~%" ))
            (format t "======================================================================")         
            ; 2) Generate cost file
            (setf costs-ht (road-ecost d-file r-file m-file m-filename distances-ht roads-ht arcs-order nnodes))
            (format t "~% --> ~s economic cost arcs generated~%" (hash-table-count costs-ht))
            (format t "======================================================================")
            (format t "~%Checking arcs (distances and economic costs)...")
            (when (equal-hash-tables-keys distances-ht costs-ht)
              (format t " OK.~%" ))
            ; 3) Write maps to matlab
            (format t "======================================================================")
            (format t "~%Writing matlab files to visualize map roads ")
            (write-maps-to-matlab r-file matlab-filename co-file coords-ht)
            ; 4) Calculate correlations
            (with-open-file (flog log-file :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
              (format t "~%======================================================================")
              (format t "~% -) Calculating correlations and writing in log file (.txt)")
              (format t "~%   -> Travel time - Spatial distance")
              (format flog "Correlation between ~%~s~%~s~%   c = ~s~%~%" 
                      t-file d-file (pearson-ht times-ht distances-ht))
              (format t "~%   -> Travel time - Economic cost")
              (format flog "Correlation between ~%~s~%~s~%   c = ~s~%~%" 
                      t-file m-file (pearson-ht times-ht costs-ht))
              (format t "~%   -> Spatial distance - Economic cost~%")
              (format flog "Correlation between ~%~s~%~s~%   c = ~s~%~%" 
                      d-file m-file (pearson-ht distances-ht costs-ht)))
            (when nprob
              (format t "Creating file with ~d random problems~%" nprob)
              (let* ((sub (subseq co-filename (1+ (search "." co-filename))))
                     (map-string (subseq sub 0 (search "." sub))))
                (create-file-with-random-problems co-file nprob map-string)))
            (format t "GENERATION OF FILES COMPLETED."))))))
