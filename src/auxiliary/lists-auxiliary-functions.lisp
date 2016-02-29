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

Description:   List functions    
-------------------------------------------------------------------
-----------------------------------------------------------------|#

(in-package :AUXILIARY-FUNCTIONS)

#|-----------------------------------------------------------------
Function to insert an element in an ordered list.
Parameters:
- l: list
- e: element
------------------------------------------------------------------|#
(defun insert-ordered (l e)
  "insert an element e in an ordered list l"
  (if (null l) 
      (cons e l)
    (if (< (car e) (car (car l)))
        (cons e l)
      (cons (car l) (insert-ordered (cdr l) e)))))

#|-----------------------------------------------------------------
Function to calculate standard deviation of a list with numerical values
Parameters:
- samples: list with numbers
------------------------------------------------------------------|#
(defun std-dev (samples)
	"Calculates the standard deviation of elements in samples"
  (let* ((n (length samples))
	 (mean (/ (reduce #'+ samples) n))
	 (tmp (mapcar (lambda (x) (expt (- x mean) 2)) samples)))
    (sqrt (/ (reduce #'+ tmp) n))))

#|-----------------------------------------------------------------
Function to convert an string to a list
- str:   string to convert
------------------------------------------------------------------|#
(defun string-to-list (str)
  "Function which converts string to list"
 (do* ((stringstream (make-string-input-stream str))
       (result nil (cons next result))
       (next (read stringstream nil 'eos)
             (read stringstream nil 'eos)))
      ((equal next 'eos) (reverse result))))
