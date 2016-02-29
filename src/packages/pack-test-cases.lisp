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

Description:       Package to generate benchmarks to test algorithms
                   over different kinds of problems
-------------------------------------------------------------------
------------------------------------------------------------------|#

(defpackage :TEST-CASES
  (:use :COMMON-LISP :GENERIC-PROBLEM :DJIKSTRA :TC-HEURISTIC :MO-SEARCH)
  (:export 
   :create-batch-file-grids
   :create-batch-file-dimacs
   :check-namoa-solutions
   :check-lexgo-solutions
   :console)
  (:import-from :GRIDS
   :start
   :goal
   :nc
   :read-grid-problem)
  (:import-from :DIMACS
   :dimacs
   :ss
   :ini-s
   :end-s
   :ncost
   :nnodes
   :*DIMACS-FILES-PATH*
   :*DIMACS-MAP-PROBLEMS*
   :*DIMACS-LOADED-MAP*
   :load-map)
  (:import-from :NAMOA
   :new-sgraph-namoa
   :namoa)
  (:import-from :NAMOA2
   :namoa2)
  (:import-from :namoa-dr-3
   :new-sgraph-namoa-dr-3
   :namoa-dr-3)
  (:import-from :namoa-dr
   :new-sgraph-namoa-dr
   :namoa-dr)
  (:import-from :LEXGO
   :new-sgraph-lexgo
   :lexgo
   :create-hash-goals
   :deviation)
  (:import-from :LEXGO-TE
   :new-sgraph-lexgo-dr
   :lexgo-dr)
  (:import-from :VECTORS
   :b-lex
   :b-lin
   :b-lex-linear))

