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

Description:       Package to define algorithm LEXGO*
-------------------------------------------------------------------
------------------------------------------------------------------|#

(defpackage :LEXGO
  (:use :COMMON-LISP :GENERIC-PROBLEM :VECTORS :MO-SEARCH) 
  (:import-from :djikstra
   :djikstra)
  (:import-from :tree-search
   :make-pq
   :insert-pq
   :pull-highest-pq
   :pq-n
   :decrease-key-pq)
  (:import-from :namoa
   :dominated-or-equal-g-cl
   :not-dominated-by-C*?)
  (:export 
   :new-sgraph-lexgo
   :lexgo
   :g-op-d-management
   :g-op-management
   :update-g-op
   :store-solution
   :best-open-label
   :create-new-node
   :check-deviation
   :deviation
   :vd
   :vg
   :vf
   :n-priority-levels
   :prune-by-deviation?
   :prune-by-deviation?-l
   :prune-by-deviation?-l-cl
   :create-label
   :create-hash-goals
   :calculate-goals-from-pareto-frontier-by-level
   :get-goal-optimal-costs))
