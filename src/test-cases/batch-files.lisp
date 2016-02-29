#|----------------------------------------------------------------------
------------------------------------------------------------------------
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

Description:   Several functions to create batch files to launch benchmarks 
               Batch files used in phD experiments for all variants of 
               lexgo* and namoa*. Files are located in:
               *files-path*//Grids//Batch files
------------------------------------------------------------------------
----------------------------------------------------------------------|#

(in-package :TEST-CASES)

#|---------------------------------------------------------------------
Create a batch file to launch experiments with namoa* algorithm
-----------------------------------------------------------------------|#
(defun create-batch-file-namoa (exe-name algor l-tie-breaking ncosts
                                         &optional (fbatch (capi::prompt-for-file "Batch file to write"))
                                         (scenario)
                                         (number-of-problems 50) 
                                         (number-of-executions 1)
                                         )
  "Write to a file a batch with experiments according to the parameters given"
  (with-open-file (f fbatch :direction :output :if-exists :append :if-does-not-exist :create)
    (dolist (tie l-tie-breaking)
      (dotimes (n number-of-problems)
        (dotimes (j number-of-executions)
          (format f "~S ~S ~S" exe-name algor tie)
          (when scenario
            (format f " ~S" scenario))
          (format f " ~A ~A~%"  (if scenario (1+ n) n) ncosts))))))
; (create-batch-file-namoa 'grids.exe 50 1 :namoa '(:lex :lin :te) 3)

#|---------------------------------------------------------------------
Create a batch file to launch experiments with lexgo* algorithm
Goals are added as paremeters. There will be two kinds of experiments:
- same goals for both priority levels, with values 0, 0.25, 0.5, 0.75 and 1.
- different goals for each priority level, the first level can have this values:
0.5 and 0.75, while the second is the multiplication of that value and one of
these: 0.25, 0.5, 0.75, 1
-----------------------------------------------------------------------|#
(defun print-goals-level1 (file exe-file algorithm tie-breaking ncosts l-goals n-prob 
                                &optional (scenario) (number-of-executions 1))
  "auxiliary function to print a batch file with experiments involving class I lexgo experiments"
  (dotimes (n n-prob)
    (dotimes (j number-of-executions)
      (dolist (meta l-goals)
        (format file "~S ~S ~S" exe-file algorithm tie-breaking)
        (when scenario
          (format file " ~S" scenario))
        (format file " ~A ~A ~A ~A~%" (if scenario (1+ n) n) ncosts meta meta)))))

(defun print-goals-level2 (file exe-file algorithm tie-breaking ncosts l-goals l-goals2 n-prob 
                                &optional (scenario) (level-fixed 1) (number-of-executions 1))
  "auxiliary function to print a batch file with experiments involving class II lexgo experiments"
  (dotimes (n n-prob)
    (dotimes (j number-of-executions)
      (dolist (meta l-goals)
          (dolist (meta2 l-goals2)
            (format file "~S ~S ~S" exe-file algorithm tie-breaking)
            (when scenario
              (format file " ~S" scenario))
             (if (= level-fixed 1) 
                 (format file " ~A ~A ~A ~A~%" (if scenario (1+ n) n) ncosts meta (* meta meta2))
               (if (= level-fixed 2)
                   (format file " ~A ~A ~A ~A~%" (if scenario (1+ n) n) ncosts (* meta meta2) meta2))))))))

(defun create-batch-file-lexgo-level1 (exe-file algorithm tie-breaking ncosts &optional 
                                                (file-out (capi::prompt-for-file "Batch file to write"))
                                                (scenario)
                                                (nprobs 50) 
                                                (level1-goals (list 0 0.25 0.5 0.75 1)))
  "Creates a batch file with experiments involving class I lexgo experiments"
  (with-open-file (fout file-out :direction :output :if-exists :append :if-does-not-exist :create)
    (print-goals-level1 fout exe-file algorithm tie-breaking ncosts level1-goals nprobs scenario)))
; (create-batch-file-lexgo-level1 'grids.exe :lexgo :lex 3)

(defun create-batch-file-lexgo-level2 (exe-file algorithm tie-breaking ncosts &optional 
                                                (file-out (capi::prompt-for-file "Batch file to write"))
                                                (scenario)
                                                (nprobs 50) 
                                                (level1-goals (list 0.5 0.75)) 
                                                (level2-goals (list 0.25 0.5 0.75 1))
                                                (fixed-level 1))
  "Creates a batch file with experiments involving class II lexgo experiments"
  (with-open-file (fout file-out :direction :output :if-exists :append :if-does-not-exist :create)
    (print-goals-level2 fout exe-file algorithm tie-breaking ncosts level1-goals level2-goals nprobs scenario fixed-level)))
; (create-batch-file-lexgo-level2 'grids.exe :lexgo :lex 3)

#|---------------------------------------------------------------------
Create a file with a complete set of bechmarks on grids and all the algorithms
The number of objectives is given by parameter
-----------------------------------------------------------------------|#
(defun create-batch-file-grids (n-obj)
  "Create a file with a complete set of bechmarks on grids and all the algorithms"
  (let* ((msg)
         (message (format msg "Batch file to write over ~S objectives" n-obj))
         (file-out (capi::prompt-for-file message)))
    (create-batch-file-namoa        'grids.exe :namoa '(:lex :lin) n-obj file-out)
    (create-batch-file-namoa        'grids.exe :namoa-dr '(:lex)   n-obj file-out)
    (create-batch-file-lexgo-level1 'grids.exe :lexgo :lex         n-obj file-out)
    (create-batch-file-lexgo-level1 'grids.exe :lexgo :lex-linear  n-obj file-out)
    (create-batch-file-lexgo-level1 'grids.exe :lexgo-dr :lex      n-obj file-out)
    (create-batch-file-lexgo-level2 'grids.exe :lexgo :lex         n-obj file-out nil 50 (list 0.5 0.75) (list 0.25 0.5 0.75))
    (create-batch-file-lexgo-level2 'grids.exe :lexgo :lex-linear  n-obj file-out nil 50 (list 0.5 0.75) (list 0.25 0.5 0.75))
    (create-batch-file-lexgo-level2 'grids.exe :lexgo-dr :lex      n-obj file-out nil 50 (list 0.5 0.75) (list 0.25 0.5 0.75))))

(defun create-batch-file-dimacs (l-maps l-exes &optional (l-files))
  "Create files with all experiments over the list of maps provided by parameter"
  (dolist (map l-maps)
    (let* ((msg)
           (message (format msg "Batch file to write experiments of map ~S" map))
           (file-out))
      (if (null l-files)
          (setf file-out (capi::prompt-for-file message))
        (setf file-out (nth (position map l-maps) l-files)))
      (create-batch-file-namoa        (nth (position map l-maps) l-exes) :namoa '(:lex :lin) 3 file-out map 20)
      (create-batch-file-namoa        (nth (position map l-maps) l-exes) :namoa-dr '(:lex)   3 file-out map 20)
      (create-batch-file-lexgo-level1 (nth (position map l-maps) l-exes) :lexgo :lex         3 file-out map 20)
      (create-batch-file-lexgo-level1 (nth (position map l-maps) l-exes) :lexgo :lex-linear  3 file-out map 20)
      (create-batch-file-lexgo-level1 (nth (position map l-maps) l-exes) :lexgo-dr :lex      3 file-out map 20)
      (create-batch-file-lexgo-level2 (nth (position map l-maps) l-exes) :lexgo :lex         3 file-out map 20 (list 0.5 0.75) (list 0.25 0.5 0.75))
      (create-batch-file-lexgo-level2 (nth (position map l-maps) l-exes) :lexgo :lex-linear  3 file-out map 20 (list 0.5 0.75) (list 0.25 0.5 0.75))
      (create-batch-file-lexgo-level2 (nth (position map l-maps) l-exes) :lexgo-dr :lex      3 file-out map 20 (list 0.5 0.75) (list 0.25 0.5 0.75)))))

(defun create-batch-file-dimacs2 (l-maps l-exes &optional (l-files))
  "Create files with all experiments over the list of maps provided by parameter"
  (dolist (map l-maps)
    (let* ((msg)
           (message (format msg "Batch file to write experiments of map ~S" map))
           (file-out))
      (if (null l-files)
          (setf file-out (capi::prompt-for-file message))
        (setf file-out (nth (position map l-maps) l-files)))
      (create-batch-file-namoa        (nth (position map l-maps) l-exes) :namoa '(:lex :lin) 3 file-out map 9)
     ; (create-batch-file-namoa        (nth (position map l-maps) l-exes) :namoa-dr '(:lex)   3 file-out map 9)
      (create-batch-file-lexgo-level1 (nth (position map l-maps) l-exes) :lexgo :lex         3 file-out map 9 (list 0 0.125 0.25 0.375 0.5))
      (create-batch-file-lexgo-level1 (nth (position map l-maps) l-exes) :lexgo :lex-linear  3 file-out map 9 (list 0 0.125 0.25 0.375 0.5))
     ; (create-batch-file-lexgo-level1 (nth (position map l-maps) l-exes) :lexgo-dr :lex      3 file-out map 9 (list 0 0.125 0.25 0.375 0.5))
      (create-batch-file-lexgo-level2 (nth (position map l-maps) l-exes) :lexgo :lex         3 file-out map 9 (list 1) (list 0.125 0.25))
      (create-batch-file-lexgo-level2 (nth (position map l-maps) l-exes) :lexgo :lex         3 file-out map 9 (list 0.125 0.25) (list 1) 2)
      (create-batch-file-lexgo-level2 (nth (position map l-maps) l-exes) :lexgo :lex-linear  3 file-out map 9 (list 1) (list 0.125 0.25))
      (create-batch-file-lexgo-level2 (nth (position map l-maps) l-exes) :lexgo :lex-linear  3 file-out map 9 (list 0.125 0.25) (list 1) 2) )))
     ; (create-batch-file-lexgo-level2 (nth (position map l-maps) l-exes) :lexgo-dr :lex      3 file-out map 9 (list 1) (list 0.125 0.25))
     ; (create-batch-file-lexgo-level2 (nth (position map l-maps) l-exes) :lexgo-dr :lex      3 file-out map 9 (list 0.125 0.25) (list 1) 2))))

#|
(create-batch-file-dimacs '(:DE :NY :NY2 :VT2) '(DE.exe NY.exe NY2.exe VT2.exe)
                          (list (concatenate 'string *DIMACS-FILES-PATH* "Batch files\\DE.bat")
                                (concatenate 'string *DIMACS-FILES-PATH* "Batch files\\NY.bat")
                                (concatenate 'string *DIMACS-FILES-PATH* "Batch files\\NY2.bat")
                                (concatenate 'string *DIMACS-FILES-PATH* "Batch files\\VT2.bat")))
|#
