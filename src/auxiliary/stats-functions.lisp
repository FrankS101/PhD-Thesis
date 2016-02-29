#|-------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
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

Description:   Generic hash table functions  
---------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------|#

(in-package :AUXILIARY-FUNCTIONS)

#|-----------------------------------------------------------------
Function to return average and typical deviation of values in hast table
Parameters:
- ht: hash table
------------------------------------------------------------------|#
(defun avg-ht (ht)
  "Returns average and typical deviation of values in hast table (ht)"
  (let ((avg 0)
        (acvariance 0)
        (dev 0)
        (n 0))
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (incf avg value)
                 (incf n))
             ht)
    (setf avg (/ avg n))
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (incf acvariance (expt (- value avg) 2)))
             ht)
    (setf dev (sqrt (/ acvariance n)))
    (values avg dev)))

#|-----------------------------------------------------------------
Function to return the covariance of values of two hash tables
Parameters:
- ht1: hash table 1
- avg1: average of values in ht1
- ht2: hash table 2
- avg2: average of values in ht2
------------------------------------------------------------------|#
(defun covariance-ht (avg1 ht1 avg2 ht2)
  "Calculate the covariance of values of two hash tables"
  (let ((acum 0))
    (maphash (lambda (key v1)
               (let ((v2 (gethash key ht2)))
                 (incf acum (* (- v1 avg1) (- v2 avg2)))))
             ht1)
    (/ acum (hash-table-count ht1))))

#|-----------------------------------------------------------------
Function to calculate Pearson correlation coefficient of values 
in ht1 and ht2
Parameters:
- ht1: hash table 1
- ht2: hash table 2
------------------------------------------------------------------|#
(defun pearson-ht (ht1 ht2)
  "Calculate Pearson correlation coefficient of values in ht1 and ht2"
  (multiple-value-bind (avg1 dev1) (avg-ht ht1)
      (multiple-value-bind (avg2 dev2) (avg-ht ht2)
        (let ((covariance (covariance-ht avg1 ht1 avg2 ht2)))
          (/ covariance (* dev1 dev2))))))

#|-----------------------------------------------------------------
Function to compare all hash keys of two hash tables
Parameters:
- ht1: hash table 1
- ht2: hash table 2
------------------------------------------------------------------|#
(defun equal-hash-tables-keys (ht1 ht2)
  "Return T whenever all hash keys of ht1 and ht2 are equal"
  (when (and (= (hash-table-count ht1) (hash-table-count ht2)))
    (maphash (lambda (key value)
               (declare (ignore value))
               (multiple-value-bind (v2 correct) (gethash key ht2)
                 (declare (ignore v2))
                 (unless correct
                   (format t "~%Key ~s is not in hash table 2." key)
                   (return-from equal-hash-tables-keys NIL))))
             ht1)
    (maphash (lambda (key value)
               (declare (ignore value))
               (multiple-value-bind (v2 correct) (gethash key ht1)
                 (declare (ignore v2))
                 (unless correct
                   (format t "~%Key ~s is not in hash table 1." key)
                   (return-from equal-hash-tables-keys NIL))))
             ht2)
    T))
