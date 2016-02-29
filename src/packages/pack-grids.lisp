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

Description:       Package with a 2d-grid implementation with random
                   costs. There are also functions to create random 
                   problems and read/write them from/to files
-------------------------------------------------------------------
------------------------------------------------------------------|#

(defpackage :GRIDS
  (:use :COMMON-LISP :GENERIC-PROBLEM :VECTORS)
  (:export 
    ; CLASSES AND CONSTRUCTORS
   :grid
   :start
   :goal
   :nc
   :state
   :create-grid
   :make-state
   ; SHOW METHODS AND SPACE STATE FUNCTIONS
   :show-state   
   :show-grid
   :show-grid-start-goal
   :key
   :cost
   :expand
   :goal?
   :h
   ; READ AND WRITE GRID PROBLEMS
   :write-grid-problem
   :read-grid-problem
   ; CREATE TEST SET OF RANDOM PROBLEMS
   :create-grid-problems
   :create-full-test-set-50
   :create-different-grid-problems
   ))
