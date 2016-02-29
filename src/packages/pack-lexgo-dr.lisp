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

Description:       LEXGO*_dr algorithm (Time efficient version)
-------------------------------------------------------------------
------------------------------------------------------------------|#
 
(defpackage :LEXGO-TE
  (:use :COMMON-LISP :GENERIC-PROBLEM :VECTORS :SORTED-ARRAY :MO-SEARCH :TC-HEURISTIC) 
  (:import-from :tree-search
   :make-pq
   :insert-pq
   :pull-highest-pq
   :pq-n
   :decrease-key-pq)
  (:import-from :lexgo
   :prune-by-deviation?-l
   :prune-by-deviation?-l-cl
   :vg
   :vf
   :vd
   :deviation
   :n-priority-levels
   :create-label
   :store-solution
   :best-open-label
   :g-op-d-management
   :g-op-management
   :update-g-op
   :create-new-node)
  (:import-from :namoa
   :not-dominated-by-C*?
   :dominated-or-equal-g-cl)
  (:import-from :namoa-dr-3
   :not-dominated-by-tc*?
   :not-dominated-by-t-gcl?)
  (:export
   :new-sgraph-lexgo-dr
   :lexgo-dr))
