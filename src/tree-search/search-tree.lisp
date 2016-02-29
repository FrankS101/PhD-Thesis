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

Description:       Implementation of Classes Node and Search Tree
-----------------------------------------------------------------------
-----------------------------------------------------------------------|#

(in-package :TREE-SEARCH)
  
(defmethod make-node (parent g &optional locator)
  (make-instance 'node :parent parent :cost g :locator locator))

#|------------------------------------------
Methods to implement Search Tree interface
--------------------------------------------|#
(defmethod make-search-tree (root)
  "Constructor"
  (let ((st (make-instance 'tree)))   
    (insert st root (make-node nil 0))
    st))

(defmethod insert ((st tree) elem node) 
  "Inserts node in search tree, employing elem's key"
  (setf (gethash (key elem) (tree st)) node)
  (values)) 

(defmethod node ((st tree) e) 
  "Returns node of elem e stored in tree"
  (gethash (key e) (tree st)))

(defmethod exists? ((st tree) e)
  "T whenever e already exists in tree"
  (node st e))

(defmethod parent-exists? ((st tree) e)
  "T if parent of e exists in tree"
  (parent (node st e)))

(defmethod root? ((st tree) e)
  "T whenever e is the root of the tree"
  (and (exists? st e)
       (null (parent-exists? st e))))

(defmethod path ((st tree) e &optional l)
  "Returns the path to e in tree"
  (cond ((root? st e)  (cons e l)) 
        (t (path st (parent-exists? st e) (cons e l)))))
		  
(defmethod g ((st tree) e)
  "Returns the g value (or cost) stored in node e"
  (cost (node st e)))

(defmethod size ((st tree))
  "Returns the number of elements in the tree"
  (hash-table-count (tree st)))

(defmethod print-search-tree ((st tree)) 
  "Print on screen the search tree elements"
  (maphash #'(lambda (k v) (print (list k (cost v)))) (tree st)))
  


