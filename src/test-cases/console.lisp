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

Description:   Command console to interact with the code, launch experiments...
------------------------------------------------------------------------
----------------------------------------------------------------------|#

(in-package :TEST-CASES)

(defun console ()
  "console to launch experiments, create batch files or check solutions of algorithms"
  (let ((option (read-from-menu #'show-first-menu 0 4))
        (goals-n))
    (setf *show-stats* T)
    (loop until (equalp option 0) do
          (if (or (null (integerp option)) (minusp option) (> option 4))
              (format t "Please choose a correct option between 0 and 4.~%")
            (cond ((equalp option 1)
                   (let* ((n-alg (read-from-menu #'show-algorithms-menu 1 6))
                          (alg (get-algorithm n-alg))
                          (tie (get-tie-breaking n-alg))
                          (nobj (if (and (>= n-alg 3) (>= 6 n-alg)) 3 (read-from-menu #'show-nobj 3 5)))
                          (goals-hash (create-hash-goals *goals-grids*))
                          (lv1) (lv2))
                     (when (and (>= n-alg 4) (>= 6 n-alg)) ; lexgo
                       (setf goals-n (read-from-menu #'show-goals-menu 1 11))
                       (setf lv1 (get-lv1 goals-n))
                       (setf lv2 (get-lv2 goals-n)))
                     (multiple-value-bind (minn maxn)
                         (read-problem-n 0 49)
                       (when (or minn maxn)
                         (if (null minn) ; launch a single problem
                             (run-grid-problem maxn nobj alg tie (gethash (list maxn (list lv1 lv2)) goals-hash) (list lv1 lv2))
                         ; launch a set of problems
                           (loop for i from minn to maxn do
                                 (run-grid-problem i nobj alg tie (gethash (list i (list lv1 lv2)) goals-hash) (list lv1 lv2))))))))
                   ((equalp option 2)
                   (let* ((n-alg (read-from-menu #'show-algorithms-menu 1 6))
                          (alg (get-algorithm n-alg))
                          (tie (get-tie-breaking n-alg))
                          (n-map (read-from-menu #'show-maps-menu 1 4))
                          (map (get-map n-map))
                          (nobj 3)
                          (goals-hash (create-hash-goals (get-dimacs-goal-file map)))
                          (lv1) (lv2) )
                     (when (and (>= n-alg 4) (>= 6 n-alg)) ; lexgo
                       (setf goals-n (read-from-menu #'show-goals-menu 1 11))
                       (setf lv1 (get-lv1 goals-n))
                       (setf lv2 (get-lv2 goals-n)))
                     (multiple-value-bind (minn maxn)
                         (read-problem-n 1 20)
                       (when (or minn maxn)
                         (if (null minn) ; launch a single problem
                             (run-dimacs-problem map maxn nobj alg tie (gethash (list maxn (list lv1 lv2)) goals-hash) (list lv1 lv2))
                         ; launch a set of problems
                           (loop for i from minn to maxn do
                                 (run-dimacs-problem map i nobj alg tie (gethash (list i (list lv1 lv2)) goals-hash) (list lv1 lv2))))))))
                  ((equalp option 3)
                   (setf option (read-from-menu #'show-solutions-check-menu 0 2))
                   (loop until (equalp option 0) do
                         (cond ((equalp option 1)
                                (check-namoa-solutions))
                               ((equalp option 2)
                                (check-lexgo-solutions)))
                         (setf option (read-from-menu #'show-solutions-check-menu 0 2))))
                  ((equalp option 4)
                   (setf option (read-from-menu #'show-batch-files-menu 0 2))
                   (loop until (equalp option 0) do
                         (cond ((equalp option 1)
                                (format t "Ready to create batch file for experiments with 3 objectives ...~%")
                                (create-batch-file-grids 3)
                                (format t "Ready to create batch file for experiments with 4 objectives ...~%")
                                (create-batch-file-grids 4)
                                (format t "Ready to create batch file for experiments with 5 objectives ...~%")
                                (create-batch-file-grids 5))
                               ((equalp option 2)
                                (format t "Ready to create batch files for maps (NY, NY2, VT2) on path: \\FILES\\Dimacs\\Batch files ...~%")
                                (create-batch-file-dimacs '(:DE :NY :NY2 :VT2) '(DE.exe NY.exe NY2.exe VT2.exe)
                                                          (list (concatenate 'string *DIMACS-FILES-PATH* "Batch files\\NY.bat")
                                                                (concatenate 'string *DIMACS-FILES-PATH* "Batch files\\NY2.bat")
                                                                (concatenate 'string *DIMACS-FILES-PATH* "Batch files\\VT2.bat")))))))))
          (setf option (read-from-menu #'show-first-menu 0 4)))
    (setf *show-stats* nil)))

(defun read-from-menu (menu-func min-op max-op)
  "The menu presented by menu-func will be showed until a valid option is entered."
  (let ((op-selected))
    (funcall menu-func)
    (setf op-selected (read))
    (loop until (and (integerp op-selected)
                     (<= min-op op-selected)
                     (>= max-op op-selected)) do
          (format t "!!! Please choose a correct option between ~A and ~A.~%" min-op max-op)
          (funcall menu-func)
          (setf op-selected (read)))
    op-selected))

(defun get-map (n)
  "get the keyword corresponding to n"
  (cond ((equalp n 1) :VT2)
        ((equalp n 2) :NY)
        ((equalp n 3) :NY2)
        (T (format t "Error encountered when getting the map keyword.~%") (abort))))

(defun get-algorithm (n)
  "get the keyword corresponding to n"
  (cond ((or (equalp n 1) (equalp n 2)) :namoa)
        ((equalp n 3) :namoa-dr)
        ((or (equalp n 4) (equalp n 5)) :lexgo)
        ((equalp n 6) :lexgo-dr)
        (T (format t "Error encountered when getting the algorithm keyword.~%") (abort))))

(defun get-tie-breaking (n)
  "get the keyword corresponding to n"
  (cond ((or (equalp n 1) (equalp n 3) (equalp n 4) (equalp n 6)) :lex)
        ((equalp n 2) :lin)
        ((equalp n 5) :lex-linear)
        (T (format t "Error encountered when getting the tie-breaking keyword.~%") (abort))))

(defun get-lv1 (n)
  "get level1 goals corresponding to n"
  (cond ((equalp n 1) 0)
        ((equalp n 2) 0.25)
        ((or (equalp n 3) (equalp n 4) (equalp n 5) (equalp n 6)) 0.5)
        ((or (equalp n 7) (equalp n 8) (equalp n 9) (equalp n 10)) 0.75)
        ((equalp n 11) 1)
        (T (format t "Error encountered when getting level1 goal.~%") (abort))))

(defun get-lv2 (n)
  "get level2 goals corresponding to n"
  (cond ((equalp n 1) 0)
        ((equalp n 3) 0.125)
        ((equalp n 7) 0.1875)
        ((or (equalp n 2) (equalp n 4)) 0.25)
        ((or (equalp n 5) (equalp n 8)) 0.375)
        ((equalp n 6) 0.5)
        ((equalp n 9) 0.5625)
        ((equalp n 10) 0.75)
        ((equalp n 11) 1)
        (T (format t "Error encountered when getting level2 goal.~%") (abort))))

(defun show-first-menu ()
  "Show the first menu presented by the console function"
  (format t "===========================================~%")
  (format t "| Select an option to continue:           |~%")
  (format t "| 1. Experiments over random grids        |~%")
  (format t "| 2. Experiments over realistic road maps |~%")
  (format t "| 3. Check solutions                      |~%")
  (format t "| 4. Create batch files                   |~%")
  (format t "|                                         |~%") 
  (format t "| 0. Exit                                 |~%")
  (format t "===========================================~%")
  (format t "===> "))

(defun show-maps-menu ()
  "Show the menu with the different maps"
  (format t "===== ROAD MAP =========~%")
  (format t "| 1. Vermont           |~%")
  (format t "| 2. New York city     |~%")
  (format t "| 3. Trimmed NY city   |~%")
  (format t "========================~%")
  (format t "===> "))

(defun show-algorithms-menu ()
  "Show all the available algorithms to solve a problem"
  (format t "==== ALGORITHMS ===================================~%")
  (format t "| 1. NAMOA*_lex -- NAMOA* lexicographic selection |~%")
  (format t "| 2. NAMOA*_lin -- NAMOA* linear selection        |~%")
  (format t "| 3. NAMOA*_dr  -- NAMOA* dimensinality reduction |~%")
  (format t "| 4. LEXGO*_lex -- LEXGO* lexicographic selection |~%")
  (format t "| 5. LEXGO*_lin -- LEXGO* linear selection        |~%")
  (format t "| 6. LEXGO*_dr  -- LEXGO* dimensionality reduction|~%")
  (format t "===================================================~%")
  (format t "===> "))

(defun show-nobj ()
  "Show the message to read from console the number of objectives to consider"
  (format t "==== CRITERIA =====================================================~%")
  (format t "| Please write the number of objectives to consider (from 3 to 5) |~%")
  (format t "===================================================================~%")
  (format t "===> "))

(defun show-goals-menu ()
  "Show the menu to select the goals to apply to LEXGO*"
  (format t "==== GOALS (Lv1 - Lv2) ===~%")
  (format t "| 1.  (0    - 0)         |~%")
  (format t "| 2.  (0.25 - 0.25)      |~%")
  (format t "| 3.  (0.5  - 0.125)     |~%")
  (format t "| 4.  (0.5  - 0.25)      |~%")
  (format t "| 5.  (0.5  - 0.375)     |~%")
  (format t "| 6.  (0.5  - 0.5)       |~%")
  (format t "| 7.  (0.75 - 0.1875)    |~%")
  (format t "| 8.  (0.75 - 0.375)     |~%")
  (format t "| 9.  (0.75 - 0.5675)    |~%")
  (format t "| 10. (0.75 - 0.75)      |~%")
  (format t "| 11. (1    - 1)         |~%")
  (format t "==========================~%")
  (format t "===> "))

(defun show-solutions-check-menu ()
  "Show the menu to check solutions from file"
  (format t "==== CHECK SOLUTIONS ==================~%")
  (format t "| 1. Check NAMOA* solutions from file |~%")
  (format t "| 2. Check LEXGO* solutions from file |~%")
  (format t "|                                     |~%")
  (format t "| 0. Go back to Menu                  |~%")
  (format t "=======================================~%")
  (format t "===> "))

(defun show-batch-files-menu ()
  "Show the menu to create batch files"
  (format t "==== BATCH FILES =============================================~%")
  (format t "| 1. Create a batch file with all experiments over grids     |~%")
  (format t "| 2. Create a batch file with all experiments over road maps |~%")
  (format t "|                                                            |~%")
  (format t "| 0. Go back to Menu                                         |~%")
  (format t "==============================================================~%")
  (format t "===> "))

(defun read-problem-n (minn maxn)
  "Read from console the problem number or range of problems to solve "
  (format t "==== PROBLEMS ==============================================~%")
  (format t "| Please write problem number to solve (from ~A to ~A)      |~%" minn maxn)
  (format t "| If you want to solve a range of problems use the format: |~%")
  (format t "| x-y, e.g. 1-10 will solve problems from #1 to #10        |~%")
  (format t "============================================================~%")
  (format t "===> ")
  (let* ((p (read-line))
         (n (parse-integer p :junk-allowed t)))
    (if (and (integerp n) (<= minn n) (>= maxn n) (null (find #\- p)))
        (values nil n)
      (if (and (integerp n) (or (> minn n) (< maxn n)))
          (progn (format t "Sorry but the problem number ~A is not correct.~%" n)
            (values nil nil))
        (if (find #\- p)
            (let ((min (parse-integer (subseq p 0 (position #\- p)) :junk-allowed t))
                  (max (parse-integer (subseq p (1+ (position #\- p))) :junk-allowed t)))
              (if (and (integerp max) (integerp min))
                  (if (= min max)
                      (values nil min)
                    (if (and (< min max) (<= minn min) (>= maxn max))
                        (values min max)
                      (progn (format t "Sorry but the range [~A - ~A] is not correct.~%" min max)
                        (values nil nil))))
                (progn (format t "Sorry but the range format is not correct.~%")
                  (values nil nil))))
          (progn (format t "Sorry but the range format is not correct.~%")
            (values nil nil)))))))

(in-package cl-user)
(import 'test-cases::console)
