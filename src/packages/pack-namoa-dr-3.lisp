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

Description:       NAMOA*_dr algorithm optimized for 3-objective searches
-------------------------------------------------------------------
------------------------------------------------------------------|#
 
(defpackage :namoa-dr-3
  (:use :COMMON-LISP :GENERIC-PROBLEM :VECTORS :SORTED-ARRAY :MO-SEARCH :TC-HEURISTIC) 
  (:import-from :tree-search
   :make-pq
   :insert-pq
   :pull-highest-pq
   :pq-n
   :decrease-key-pq)
  (:import-from :namoa
   :create-new-node
   :close-label
   :best-open-label
   :store-solution
   :insert-and-filter-g-op
   :update-g-op)
  (:export
   :show-stats-namoa-dr
   :new-sgraph-namoa-dr-3
   :namoa-dr-3
   :not-dominated-by-t-gcl?
   :not-dominated-by-tc*?))
