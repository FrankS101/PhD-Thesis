#|---------------------------------------------------------------------
-----------------------------------------------------------------------
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

Description:       Variables to store statistics of the results and performance
                   of the algorithms
-----------------------------------------------------------------------
-----------------------------------------------------------------------|#

(in-package :MO-SEARCH)

(defparameter *show-stats* nil)
(defparameter *tot-closed* 0)       ; also called expanded or permanent labels
(defparameter *max-open* 0)         ; maximum number of open labels at the same time
(defparameter *cur-open* 0)         ; current number of open labels
(defparameter *tot-eq* 0)           ; total number of paths reaching the same node with equal cost
(defparameter *tot-pruned-cl* 0)    ; total number of pruned labels by G_cl sets
(defparameter *tot-pruned-op* 0)    ; total number of pruned labels by G_op sets
(defparameter *tot-dev-pruned* 0)   ; total number of pruned labels by deviation
(defparameter *tot-filtered* 0)     ; total number of filtered labels
(defparameter *tot-dev-filtered* 0) ; total number of filtered labels by deviation
(defparameter *t-gcl* 0)            ; total number of labels in truncated G_cl sets (T(G_cl))
(defparameter *t-costs* 0)          ; total number of labels in truncated C* set (T(C*))
(defparameter *filt-comp* 0)        ; total number of filtering comparisons performed by namoa algorithm
(defparameter *prun-comp* 0)        ; total number of pruning comparisons performed by namoa algorithm
(defparameter *dev-filt-comp* 0)    ; total number of filtering by deviation comparisons by lexgo algorithm
(defparameter *dev-prun-comp* 0)    ; total number of pruning by deviation comparisons by lexgo algorithm
(defparameter *time-ef-prun* 0)     ; total number of times the time-efficient pruning was invoked by lexgo-dr algorithm
(defparameter *regular-prun* 0)     ; total number of times the regular pruning was invoked by lexgo-dr algorithm
(defparameter *execution-start* 0)  ; to calculate the time elapsed since the algorithm was called (it is used to compare to the time limit)
(defparameter *time-limit* 28800)       ; time-limit to stop the execution of the current algorithm (in seconds)

(defun reset-stats ()
  "Reset the value of all stats variables"
  (setf *tot-closed* 0
        *max-open* 0
        *cur-open* 0
        *tot-eq* 0
        *tot-pruned-cl* 0
        *tot-pruned-op* 0
        *tot-filtered* 0
        *t-gcl* 0
        *t-costs* 0
        *filt-comp* 0
        *prun-comp* 0
        *dev-filt-comp* 0
        *dev-prun-comp* 0
        *time-ef-prun* 0
        *regular-prun* 0 ))
