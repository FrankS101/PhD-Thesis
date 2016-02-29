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

Description:       Generic definitions of Node and Search Tree 
                   (single-objective search)
-----------------------------------------------------------------------
-----------------------------------------------------------------------|#

(in-package :TREE-SEARCH)

#|------------------------------------------
Node class (single objective search)
--------------------------------------------|#
(defclass node ()
  ((parent  :accessor parent 
            :initarg :parent)
   (locator :accessor locator
            :initarg :locator)
   (cost    :reader cost 
            :initarg :cost))
  (:documentation "Basic node to use in a search tree"))
    
#|------------------------------------------
Search Tree class (single objective search)
--------------------------------------------|#
(defclass tree ()
  ((tree :accessor tree
              :initform (make-hash-table :test #'equalp)))
  (:documentation "For each (key value) keeps at least the parent node in the tree"))

#|------------------------------------------
Node interface
--------------------------------------------|#
(defgeneric make-node (parent g &optional locator)
  (:documentation "Constructor"))
  
#|------------------------------------------
Search Tree interface
--------------------------------------------|#
(defgeneric make-search-tree (raiz)
  (:documentation "Constructor, single objective node"))

(defgeneric insert (tree elem node) 
  (:documentation "Inserts node in search tree, employing elem's key"))

(defgeneric node (tree e) 
  (:documentation "Returns node of elem e stored in tree"))

(defgeneric exists? (tree e)
  (:documentation "T whenever e already exists in tree"))

(defgeneric parent-exists? (st e)
  (:documentation "T if parent of e exists in tree"))

(defgeneric root? (tree e)
  (:documentation "T whenever e is the root of the tree"))

(defgeneric path (tree e &optional l) 
  (:documentation "Returns the path to e in tree"))

(defgeneric g (tree e)  
  (:documentation "Returns the g value (or cost) stored in node e"))
  
(defgeneric size (tree)
  (:documentation "Returns the number of elements in the tree"))

(defgeneric print-search-tree (tree)
  (:documentation "Print on screen the search tree elements"))
  



