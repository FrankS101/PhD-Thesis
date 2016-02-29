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

Description:   System to define all general functions needed to perform 
               experiments over random grids and realistic road maps using 
               NAMOA*, NAMOA*_dr, LEXGO* and LEXGO*_dr
---------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------|#

#|---------------------------------------------------------------------
Global Variables
-----------------------------------------------------------------------|#
(defvar *root*)
(setq *root* (directory-namestring (current-pathname)))
(defvar *files-path*)
(setq *files-path* (concatenate 'string *root* "files/"))
;(setq *files-path* (concatenate 'string (subseq *root* 0 (search "code" *root*)) "files\\"))
(defvar *fasl-directory*)
(setq *fasl-directory* (concatenate 'string *root* "fasl"))

#|---------------------------------------------------------------------
System definition
-----------------------------------------------------------------------|#
(defsystem packages
  (:default-pathname  *root*
   :default-type  :lisp-file
   :object-pathname *fasl-directory*)
   :members (
             "packages/pack-auxiliary-functions"
             "packages/pack-generic-problem"
             "packages/pack-vectors"
             "packages/pack-sorted-array"
             "packages/pack-tree-search"
             "packages/pack-grids"
             "packages/pack-dimacs"
             "packages/pack-djikstra"
             "packages/pack-tc-heuristic"
             "packages/pack-mo-search"
             "packages/pack-namoa"
             "packages/pack-namoa2"
             "packages/pack-namoa-dr-3"
             "packages/pack-namoa-dr"
             "packages/pack-lexgo"
             "packages/pack-lexgo-dr"
             "packages/pack-test-cases"
              )
   :rules ((:in-order-to :compile :all
            (:requires (:load :previous)))))

(defsystem auxiliary-functions
  (:default-pathname  *root*
   :default-type  :lisp-file
   :object-pathname *fasl-directory*)
   :members (
              "auxiliary/lists-auxiliary-functions"
              "auxiliary/stats-functions"
              )
   :rules ((:in-order-to :compile :all
            (:requires (:load :previous)))))

(defsystem structures
  (:default-pathname  *root*
   :default-type  :lisp-file
   :object-pathname *fasl-directory*)
   :members (
              "structures/vectors"
              "structures/sorted-array"
              )
   :rules ((:in-order-to :compile :all
            (:requires (:load :previous)))))

(defsystem tree-search
  (:default-pathname  *root*
   :default-type  :lisp-file
   :object-pathname *fasl-directory*)
   :members (
              "tree-search/interface-priority-queue"
              "tree-search/interface-search-tree"
              "tree-search/priority-queue"
              "tree-search/search-tree"
              )
   :rules ((:in-order-to :compile :all
            (:requires (:load :previous)))))

(defsystem problems
  (:default-pathname  *root*
   :default-type  :lisp-file
   :object-pathname *fasl-directory*)
   :members (
             "problems/interface-generic-problem"
             "problems/2d grids/grids"
             "problems/2d grids/grid-problems-and-files"
             "problems/road maps/dimacs"
             "problems/road maps/dimacs-maps-to-matlab"
             "problems/road maps/dimacs-additional-files"
             "problems/road maps/dimacs-trim-maps"
             "problems/road maps/dimacs-problems-and-files"
              )
   :rules ((:in-order-to :compile :all
            (:requires (:load :previous)))))

(defsystem algorithms
  (:default-pathname  *root*
   :default-type  :lisp-file
   :object-pathname *fasl-directory*)
   :members (
             "algorithms/djikstra"
             "heuristics/tc-heuristic"
             "algorithms/mo-search-statistics"
             "algorithms/multiobjective-search-classes"
             "algorithms/namoa"
             "algorithms/namoa2"
             "algorithms/namoa-dr-3"
             "algorithms/namoa-dr"
             "algorithms/deviation"
             "algorithms/lexgo"
             "algorithms/lexgo-dr"
             "auxiliary/auxiliary-functions-lexgo"
              )
   :rules ((:in-order-to :compile :all
            (:requires (:load :previous)))))   

(defsystem test-cases
  (:default-pathname  *root*
   :default-type  :lisp-file
   :object-pathname *fasl-directory*)
   :members (
             "test-cases/batch-files"
             "test-cases/trace-files"
             "test-cases/check-solutions"
             "test-cases/creating-exe-grids"
             "test-cases/creating-exe-dimacs"
             "test-cases/console"
              )
   :rules ((:in-order-to :compile :all
            (:requires (:load :previous)))))  
  		
(defsystem system
  (:default-pathname  *root*
   :default-type  :system
   :object-pathname *fasl-directory*)
   :members (
             "packages"
             "auxiliary-functions"
             "structures"
             "tree-search"
             "problems"
             "algorithms"
             "test-cases"
              )
   :rules ((:in-order-to :compile :all
            (:requires (:load :previous)))))

(compile-system "system")
(load-system "system")
