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

Description:       Multiobjective search package with common classes and methods
                   to multiobjective (multicriteria) search algorithms
-------------------------------------------------------------------
------------------------------------------------------------------|#

(defpackage :MO-SEARCH
  (:use :COMMON-LISP :VECTORS)
  (:import-from :GENERIC-PROBLEM
   :key)
  (:import-from :GRIDS
   :show-state)
  (:import-from :TREE-SEARCH
   :insert-pq
   :make-pq)
  (:import-from :GENERIC-PROBLEM
   :h)
  (:export 
;  Bi-objective search
   :node-labels-size
   :node-labels
   :first-open
;  Cost vector classes
   :c-vector
   :c-vector-f
   :c-vector-lexgo
;  C-vector
   :vec
   :parent-list
;  Node
   :node
   :node-te
   :node-lexgo
   :node-lexgo-dr
   :g-op
   :g-cl
   :vec-h
   :vec-f
   :loc
   :t-costs
   :t-gcl
;  Lexgo node
   :vec-dev
   :sat?
;  Mo-graph
   :mo-graph
   :mo-graph-te
   :costs*
   :dest-nodes
   :nodes-table
   :open-l
;  Mo-graph-lexgo
   :mo-graph-lexgo
   :mo-graph-lexgo-dr
   :d*
   :d0
   :goals
   :ncosts
   :nlevels
;  Methods
   :show
   :best-open-vector
   :add-parent
   :get-vector
   :in-graph?
;  Functions
   :create-generic-sgraph
   :print-label
   :l-state
   :label
   :f
   :dominated-or-equal-g-op
   :get-ideal-and-nadir
;  STATS
   :*show-stats*
   :*tot-closed* 
   :*max-open* 
   :*cur-open* 
   :*tot-eq* 
   :*tot-pruned-cl* 
   :*tot-pruned-op* 
   :*tot-dev-pruned* 
   :*tot-filtered* 
   :*tot-dev-filtered* 
   :*t-gcl* 
   :*t-costs* 
   :*filt-comp* 
   :*prun-comp* 
   :*dev-filt-comp* 
   :*dev-prun-comp* 
   :*time-ef-prun* 
   :*regular-prun* 
   :*execution-start*
   :*time-limit*
   :reset-stats
))
