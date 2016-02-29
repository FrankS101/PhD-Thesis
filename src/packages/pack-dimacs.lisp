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

Description:       Package with DIMACS challenge implementation files
-------------------------------------------------------------------
------------------------------------------------------------------|#

(defpackage :DIMACS
  (:use :COMMON-LISP :GENERIC-PROBLEM :VECTORS)
  (:import-from :auxiliary-functions
   :string-to-list
   :avg-ht
   :covariance-ht
   :pearson-ht
   :equal-hash-tables-keys)
  (:export 
   :dimacs
   :ss-dimacs
   :ss
   :ini-s
   :end-s
   :ncost
   :nnodes
;  MAP FILES
   :generate-additional-files
   :trim-map-files
;  GLOBAL VARIABLES
   :load-map
   :*DIMACS-FILES-PATH*
   :*DIMACS-MAP-PROBLEMS*
   :*DIMACS-LOADED-MAP*))
