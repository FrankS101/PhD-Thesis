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

Description:       Implementation of Djikstra's algorithm slightly 
                   modified to be used in the calculation of the 
                   lower bound function to be employed in Multiobjective
                   Shortest Path Search 
-------------------------------------------------------------------
------------------------------------------------------------------|#

(defpackage :djikstra
  (:use :COMMON-LISP :GENERIC-PROBLEM) 
  (:import-from :tree-search
   :make-pq
   :insert-pq
   :decrease-key-pq
   :pull-highest-pq
   :tree
   :node
   :locator
   :g
   :make-search-tree
   :insert
   :make-node
   :exists?)
  (:export 			
   :djikstra))


