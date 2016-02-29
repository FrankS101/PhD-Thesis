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

Description:       Generic interface for heuristic search problems
-----------------------------------------------------------------------
-----------------------------------------------------------------------|#

(in-package :GENERIC-PROBLEM)

#|----------------------------------------------
The size of cost vectors to represent states of the problem
is defined in this file.
Positive integer values are assumed. 
This parameter has a big influence on the memory
requirements of the algorithms. If this is too small,
an overflow can occurr whenever the length of the paths
grows. If this is too big, some memory will be wasted. 
Lisp upgrades data types when arrays are built. Real 
types employed can be checked by 
(upgraded-array-element-type *int-type*).
In our case we will use unsigned-byte 16 for grid problems
and unsigned-byte 32 for road maps problems
 ----------------------------------------------|#

(defparameter *int-type* '(unsigned-byte 32))

#|------------------------------------------------------------------------------------------
Interface of a generic search problem
------------------------------------------------------------------------------------------|#

(defgeneric equal? (s s2)
  (:documentation "T if s and s2 are from the same class and their keys are equalp")) 
  
(defgeneric expand (s p)
  (:documentation "List of states generated when expanding s in problem p")) 
  
(defgeneric key (s)
  (:documentation "key of state s to use as index of an equalp hash table")) 
  
(defgeneric cost (s s2 p)
  (:documentation "Cost of the arc s -> s2 in problem p")) 
  
(defgeneric h (s gamma)
  (:documentation  "Lower bound (or heuristic estimate) of the cost to reach gamma from s")) 

(defgeneric goal? (s p) 
  (:documentation "T if s is the destination node in problem p, otherwise NIL."))

#|------------------------------------------------------------------------|#
  
(defmethod equal? ((obj1 T) (obj2 T))
  (and (eq (type-of obj1) (type-of obj2))
       (equalp (key obj1) (key obj2))))

