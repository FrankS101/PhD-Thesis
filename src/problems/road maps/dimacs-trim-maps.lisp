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

Description:   Functions to create trimmed map files depending on the
               parameters given to the function trim-map-files. The idea
               is cutting the map by giving the minimum (or maximum) 
               coordinate for the axe (x and y)
-------------------------------------------------------------------
-----------------------------------------------------------------|#

(in-package :DIMACS)

(defun fcomp-to-string (fcomp)
  "Convert a comparison function to string"
  (cond ((equalp fcomp #'<) "<")
        ((equalp fcomp #'>) ">")
        ((equalp fcomp #'>=) ">=")
        ((equalp fcomp #'<=) "<=")
        (T (format t "Unable to identify comparison function provided. Aborting ...") (abort))))

(defun trim-nodes (nodes-ht tv1 tv2 fcomp1 fcomp2 fout f-from)
  "Trim a file with information regarding the nodes and write to file fout the result trimmed file"
  (let ((trimmed-ht (make-hash-table :test #'equalp))
        (count 0)
        (nodes-order))
    (format t "~%Storing new coordinates ...")
    (maphash #'(lambda (node coords)
                 (let ((c1 (first coords))
                       (c2 (second coords)))
                   (incf count)
                   (when (and (funcall fcomp1 c1 tv1)
                              (funcall fcomp2 c2 tv2))
                     (setf (gethash node trimmed-ht) coords)
                     (push node nodes-order))))
             nodes-ht)
    (setf nodes-order (reverse nodes-order))
    (format t "~%Writing trimmed file with new nodes and their coordinates ...")
    (with-open-file (f fout 
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (format f "c Trimmed file corresponding to ~S with all nodes which coordinates (c1 c2) must satisfy that (c1 ~A ~S) && (c2 ~A ~S)" 
              f-from (fcomp-to-string fcomp1) tv1 (fcomp-to-string fcomp2) tv2)
      (format f "~%c Original graph contains ~S nodes" count)
      (format f "~%c New graph contains ~S nodes, i.e. ~3,2f% of the original nodes" (length nodes-order) (* 100 (/ (length nodes-order) count)))
      (format f "~%c")
      ; print now the new nodes and their coordinates
      (dolist (node nodes-order)
        (let ((coords (gethash node trimmed-ht)))
          (format f "~%v ~S ~S ~S" node (first coords) (second coords)))))
    trimmed-ht))

(defun trim-costs (costs-ht nodes-ht arcs-order nnodes totnnodes tv1 tv2 fcomp1 fcomp2 fout f-from)
  "Trim a file with costs (distances or times) and write to file fout the result trimmed file"
  (let ((trimmed-ht (make-hash-table :test #'equalp))
        (count 0))
    (format t "~%Storing new arcs ...")
    (maphash #'(lambda (arc cost)
                 (let ((a1 (first arc))
                       (a2 (second arc)))
                   (incf count)
                   (when (and (gethash a1 nodes-ht)
                              (gethash a2 nodes-ht))
                     (setf (gethash arc trimmed-ht) cost))))
             costs-ht)
    (format t "~%Writing trimmed file with new arcs and their costs ...")
    (with-open-file (f fout 
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (format f "c Trimmed file corresponding to ~S with all arcs belonging to nodes which coordinates (c1 c2) must satisfy that (c1 ~A ~S) && (c2 ~A ~S)" 
              f-from (fcomp-to-string fcomp1) tv1 (fcomp-to-string fcomp2) tv2)
      (format f "~%c Original graph contains ~S nodes and ~S arcs" totnnodes count)
      (format f "~%c New graph contains ~S nodes (~3,2f%) and ~S arcs (~3,2f%)" 
              nnodes (* 100 (/ nnodes totnnodes)) (hash-table-count trimmed-ht) (* 100 (/ (hash-table-count trimmed-ht) count)))
      (format f "~%c")
      ; print now the new nodes and their coordinates
      (dolist (arc arcs-order)
        (let ((cost (gethash arc trimmed-ht)))
          (when cost 
            (format f "~%a ~S ~S ~S" (first arc) (second arc) cost)))))))

(defun trim-map-files (map tv1 tv2 fcomp1 fcomp2 
                           &optional (nprob 20) (co-file (capi::prompt-for-file "Coordinates File")))
  "Trim all coordinates and arcs which not satisfy being within the given parameters, e.g. tv1= 10, tv2= 20, fcomp1 #'> fcomp2 #'<, arc ((c11,c12),(c21 c22)) must satisfy that (c11 > tv1 and c12 < tv2) and (c21 > 10 and c22 < 20))"
  (let* ((co-filename    (pathname-name co-file))    ; coordinates
         (d-filename     co-filename)                ; spatial distances
         (t-filename     (copy-seq d-filename))      ; travel times
         (co2-filename   co-filename)                 
         (d2-filename    d-filename)                  
         (t2-filename    t-filename)                 
         (position       (search map co-filename)))
    ;generate all filenames 
    (setf (subseq t-filename (search "-d" co-filename)) "-t.")        ; travel times
    (setf t2-filename (concatenate 'string 
                                   (subseq t2-filename 0 position) 
                                   map "2" 
                                   (subseq t2-filename (+ 2 position))))
    (setf d2-filename (concatenate 'string 
                                   (subseq d2-filename 0 position) 
                                   map "2" 
                                   (subseq d2-filename (+ 2 position))))
    (setf co2-filename (concatenate 'string 
                                    (subseq co2-filename 0 position) 
                                    map "2" 
                                    (subseq co2-filename (+ 2 position))))  
    (let* ((d-file   (make-pathname :name d-filename :type "gr" :defaults co-file))
           (t-file   (make-pathname :name t-filename :type "gr" :defaults co-file))
           (co2-file (make-pathname :name co2-filename :type "co" :defaults co-file))
           (d2-file  (make-pathname :name d2-filename :type "gr" :defaults co-file))
           (t2-file  (make-pathname :name t2-filename :type "gr" :defaults co-file))) 
        ;0) Read in hash tables file data
      (let* ((nnodes) (totnnodes) 
             (times-ht) 
             (coords-ht)
             (trim-coords-ht))     
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
          (format t "~% --> ~s nodes stored~%" (hash-table-count coords-ht))
          (format t "======================================================================") 
          (format t "~%Starting to trim files according to coordinates given by parameters.")
          (setf trim-coords-ht (trim-nodes coords-ht tv1 tv2 fcomp1 fcomp2 co2-file co-filename))
          (setf nnodes (hash-table-count trim-coords-ht))
          (setf totnnodes (hash-table-count coords-ht))
          (trim-costs times-ht trim-coords-ht arcs-order nnodes totnnodes tv1 tv2 fcomp1 fcomp2 t2-file t-filename)
          (trim-costs distances-ht trim-coords-ht arcs-order nnodes totnnodes tv1 tv2 fcomp1 fcomp2 d2-file d-filename)
          (format t "~%GENERATION OF TRIMMED FILES COMPLETED.")
          (format t "~%Proceeding now to generate additional files ... ~%")
          (generate-additional-files co2-file nprob))))))
; (trim-map-files "NY" -74000000 40800000 #'<= #'>=))
