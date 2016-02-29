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

Description:       Djikstra's algorithm slightly modified to be a 
                   one-to-all algorithm. It returns three values, the
                   optimum cost to reach each node, T if the goal node
                   was found and the number of nodes reached
-----------------------------------------------------------------------
-----------------------------------------------------------------------|#

(in-package :DJIKSTRA)

#|---------------------------------------------------------------------
Djikstra's algorithm is employed to obtain TC heuristic in multiobjective
search problems (optimal point with h* for each objective independently).
Parameters:
- s: initial state
- gamma: goal state
- problem: state space
- c: cost vector component to consider
-----------------------------------------------------------------------|#

(defun djikstra (s gamma problem c &optional (tree (make-search-tree s)) (open (make-pq :tie-breaking #'<)) (solved))
  "Single objective djikstra's algorithm. Return three values: the optimum cost to reach each node, T if the goal node was found and the number of nodes reached."
  (let ((solution? (or solved (goal? s gamma))))
    (cond ((null s) (values tree solved (hash-table-count (tree tree))))
          (T  
           (dolist (s2 (expand s problem))
           (let ((g2 (+ (g tree s) (aref (cost s s2 problem) c)))
                   (loc nil))
               (when (or (not (exists? tree s2))
                         (< g2 (g tree s2)))
                 (when (exists? tree s2)
                   (setf loc (decrease-key-pq open (locator (node tree s2)) g2))
                   (insert tree s2 (make-node s g2 loc)))
                 (when (not (exists? tree s2))
                   (let ((local (insert-pq open g2 s2)))
                     (insert tree s2 (make-node s g2 local)))))))
           (djikstra (second (pull-highest-pq open)) gamma problem c tree open solution?)))))



