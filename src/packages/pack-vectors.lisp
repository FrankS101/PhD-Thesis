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

Description:       This package comprises the basic operations with
                   vectors, cost vectors and sets of cost vectors
-------------------------------------------------------------------
------------------------------------------------------------------|#

(defpackage :VECTORS
  (:use :COMMON-LISP)
  (:import-from :generic-problem
   :*int-type*)
  (:export
   :*int-type*
   :make-vector0
   :make-vector-inf
   :add-v
   :sub-v
   :dominates
   :non-dominated
   :dominate-or-equals
   :dominate-or-equals-2v
   :dominance
   :trunc-v
   :nondominated-lex-list
   :b-lex
   :b-lin
   :b-lex-linear
   ))
