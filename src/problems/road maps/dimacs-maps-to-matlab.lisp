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

Description:   Functions to create MATLAB files to render DIMACS maps.
-------------------------------------------------------------------
-----------------------------------------------------------------|#

(in-package :DIMACS)

(defun dimacs-to-matlab (lvalues &optional (gr-file (capi::prompt-for-file "Gr data file"))
                               (m-file (capi::prompt-for-file "Matlab output file"))
                               (coord-ht (coords-file-to-ht)))
  "Generate a .m file with all arcs from gr-file which costs are in lvalues (or all when nil)"
    (with-open-file (fin gr-file :direction :input)
      (with-open-file (fout m-file :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
        (let ((arcs-ht (make-hash-table :test #'equalp)))
          (format fout "figure;~%")
          (format fout "hold on;~%")
          (format fout "xlabel('Longitude','FontSize',16);~%")
          (format fout "ylabel('Latitude','FontSize',16);~%")
          (do ((line (read-line fin nil :eof)
                      (read-line fin nil :eof))
               (total 0)
               (draw 0))
              ((eq line :eof))
            (multiple-value-bind (node1 node2 cost) (read-dimacs-file-line line)
              (when node1
                (incf total)
                (when (and (not (gethash (list node2 node1) arcs-ht))   ; inverse arc has not been drawn 
                           (or (null lvalues) (member cost lvalues)))   ; shall be drawn
						   (setf (gethash (list node1 node2) arcs-ht) T)
                  (incf draw)
                  (let ((c1 (gethash node1 coord-ht))
                        (c2 (gethash node2 coord-ht)))
                    (format fout "~%plot([~d,~d],[~d,~d]);" (cadr c1) (cadr c2) (car c1) (car c2) ))))))))))

(defun write-maps-to-matlab (r-file matlab-filename co-file coords-ht)
  "Writes maps in matlab for road types 10, 8, 6, 10 8 6, and all"
  (format t ".")
  (dimacs-to-matlab '(10) r-file
                    (make-pathname :name (concatenate 'string matlab-filename "_10")
                                   :type "m"
                                   :defaults co-file)
                    coords-ht)

  (format t ".")
  (dimacs-to-matlab '(8) r-file
                    (make-pathname :name (concatenate 'string matlab-filename "_8")
                                   :type "m"
                                   :defaults co-file)
                    coords-ht)

  (format t ".")
  (dimacs-to-matlab '(6) r-file
                    (make-pathname :name (concatenate 'string matlab-filename "_6")
                                   :type "m"
                                   :defaults co-file)
                    coords-ht)

  (format t ".")
  (dimacs-to-matlab '(10 8 6) r-file
                    (make-pathname :name (concatenate 'string matlab-filename "_1086")
                                   :type "m"
                                   :defaults co-file)
                    coords-ht)


  (format t ".")
  (dimacs-to-matlab '(10 8 6 4) r-file
                    (make-pathname :name (concatenate 'string matlab-filename "_all")
                                   :type "m"
                                   :defaults co-file)
                    coords-ht))

