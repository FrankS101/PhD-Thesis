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

Description:   Implementation of several functions regarding to 
               deviation meassures employed in LEXGO*
-------------------------------------------------------------------
------------------------------------------------------------------|#

(in-package :LEXGO)

(defvar *accuracy* 10000000) ; states the necessary accuracy to calculate deviation withouth losing precision

#|---------------------------------------------------------------------
Function to return the number of priority levels in a list of goals
Parameters:
- goals: list with goals grouped in preemptive priority levels
-----------------------------------------------------------------------|#
(defun n-priority-levels (goals)
  "Return the number of priority levels from a list with goals"
  (length goals))

#|---------------------------------------------------------------------
Function to check if goals are given in an appropiate format
Parameters:
- ncosts: Number of attributes
- goals:  List of lists with goals grouped in priority levels
-----------------------------------------------------------------------|#
(defun check-deviation (ncosts goals)
  "check if goals are given in an appropiate format"
  (let ((ngoals 0))
    (dolist (level goals)
      (let ((level-sum 0))
        (dolist (goal-def level)
          (let* ((goal (car goal-def))
                 (weight (cadr goal-def)))
            (if (or (minusp goal) (<= weight 0) (> weight 1)) 
                (return nil)
              (incf level-sum weight)))
          (incf ngoals))
        (if (/= level-sum 1) (return nil))))
    (if (/= ncosts ngoals) nil T)))

#|---------------------------------------------------------------------
Function to calculate the deviation vector
Parameters:
- costs:   Vector to calculate the deviation from goals
- goals:   Goals given by the decission maker
e.g. (deviation '(15 12 20) '(((10 0.5) (10 0.5)) ((10 1))))
costs is a vector of costs and goals is defined in the following way: (((20 0.8) (10 0.2)) ((10 1))) which means 
the first level of priority has two goals with weights 0.8 and 0.2, while the second level has just
one goal. 
Level1: c1 <= 20   0.8  ;  c2 <= 10   0.2      Level2: c3 <= 10   1
-----------------------------------------------------------------------|#
(defun deviation (costs goals)
  "Function to calculate the deviation vector (d) from the given lexicographic goals"
  (let ((dev (make-array (length goals) :initial-element 0)) ; deviation vector
        (cont 0)
        (lv-cont 0))
    (dolist (level goals dev)
      (dolist (goal-def level)
        (let* ((goal (car goal-def))
               (weight (cadr goal-def))
               (cost (elt costs cont))
               (dif (- cost goal)))
          (when (> dif 0) (incf (aref dev lv-cont) (* dif weight))))
        (incf cont))
      (incf lv-cont))))

#|---------------------------------------------------------------------
Function to establish whenever a vector prunes another according to some goals.
This is the pruning by deviation defined in LEXGO paper.
Parameters:
- d1:     deviation vector of d_P(node)
- d2:     deviation vector of estimate to expand
- f1:     estimate vector of d_P(node)
- f2:     estimate vector to expand
- goals:  given goals by the decission maker
-----------------------------------------------------------------------|#
(defmethod prune-by-deviation? ((d1 vector) (d2 vector) (f1 vector) (f2 vector) goals) 
  "T whenever f1 has a deviation better enough to prune f2. Here is taken into account when there is slack"
  (let ((obj 0))                                               ; to access the right attribute
    (incf *dev-prun-comp*)                                     
    (dotimes (i (array-dimension d1 0) nil)                    ; for each dimension of deviation vector   
      (if (> (aref d1 i) (aref d2 i))                          ; if that component is already greater
          (return-from prune-by-deviation? nil)                ; this deviation is not better -> no pruning
        (let ((slack 0)
              (goals-level (nth i goals)))
          (dolist (goal goals-level)
            (let ((s1 (max 0 (- (car goal) (aref f1 obj))))    ; slack variable of f1
                  (s2 (max 0 (- (car goal) (aref f2 obj)))))   ; slack variable of f2
              (incf slack (* (cadr goal)                       ; w_k 
                             (max 0 (- s2 s1))))
              (incf obj)))
          (if (= (aref d1 i) (aref d2 i))                      ; if components are equal
              (if (/= slack 0)                                 ; slack /= 0 -> return nil
                  (return-from prune-by-deviation? nil)        ; otherwise can happen that d1 < d2 or that slack=0 
                nil)                                           ; continue loop
            (if (< slack (- (aref d2 i) (aref d1 i)))          ; d1 < d2 and slack is smaller than the difference 
                                                               ; between deviation components
                (return-from prune-by-deviation? T)
              (return nil))))))))

#|---------------------------------------------------------------------
Function to filter all estimates pruned_P by a given vector.
This function uses the pruning by deviation defined in LEXGO paper.
Parameters:
- d2:           deviation vector of estimate already discovered
- f2:           estimate vector already discovered
- vector-list:  G_op to be analyzed
- goals:        given goals by the decission maker
-----------------------------------------------------------------------|#
(defmethod prune-by-deviation?-l ((d2 vector) (f2 vector) vector-list goals) 
  "Function to filter all estimates pruned_P by a given vector on g_op list."
  (if (null vector-list)
      nil
    (let* ((vector (car (last vector-list)))                         
           (f1 (vec-f vector))
           (d1 (vec-dev vector)))
      (cond ((not (b-lex d1 d2))
             (return-from prune-by-deviation?-l nil))
            ((prune-by-deviation? d1 d2 f1 f2 goals)
             T)
            (T                                                           
             (prune-by-deviation?-l d2 f2 (butlast vector-list) goals))))))      

#|---------------------------------------------------------------------
Function to filter all estimates pruned_P by a given vector.
This function uses the pruning by deviation defined in LEXGO paper.
Parameters:
- d2:           deviation vector of estimate already discovered
- f2:           estimate vector already discovered
- vector-list:  G_cl to be analyzed
- goals:        given goals by the decission maker
-----------------------------------------------------------------------|#              
(defmethod prune-by-deviation?-l-cl ((d2 vector) (f2 vector) vector-list goals) 
  "Function to filter all estimates pruned_P by a given vector on g_cl list."
  (if (null vector-list)
      nil
    (let* ((vector (car vector-list))                              
           (f1 (vec-f vector))
           (d1 (vec-dev vector)))
      (cond ((not (b-lex d1 d2))
             (return-from prune-by-deviation?-l-cl nil))
            ((prune-by-deviation? d1 d2 f1 f2 goals)
             T)
            (T                                                      
             (prune-by-deviation?-l-cl d2 f2 (cdr vector-list) goals))))))

