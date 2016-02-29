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

Description:       Package with priority queue and search tree
                   implementations
-------------------------------------------------------------------
------------------------------------------------------------------|#

(defpackage :TREE-SEARCH
  (:use :COMMON-LISP)
  (:import-from :GENERIC-PROBLEM
   :key)
  (:export 
;  NODE 
   :node
   :make-node
   :locator
   :elem
   :cost
   :tree
   :parent  
;  SEARCH TREE             
   :tree
   :make-search-tree
   :insert
   :exists?
   :root?
   :path
   :g
   :size
   :print-search-tree	   
;  BINARY HEAP
   :heap	   
   :make-binary-heap
   :print-binary-heap
;  PRIORITY QUEUE
   :make-pq
   :insert-pq
   :get-highest-pq
   :pull-highest-pq
   :decrease-key-pq
   :pq-n
   :print-pq		   
;  HEAP PRIORITY QUEUE ITEM          
   :make-pq-item
   ))

