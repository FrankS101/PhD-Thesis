#|-------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
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
---------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------|#

(load-all-patches)

(proclaim '(optimize (speed 3) (safety 0) (debug 0)))

(defvar *root* "C:\\Users\\Francis\\Google Drive\\CODE\\")

(defvar *delivered-image-name* (concatenate 'string *root* "grids.exe"))

(compile-file-if-needed (current-pathname "defsys") :load t
                        :output-file (pathname-location *delivered-image-name*))

#+cocoa
(when (save-argument-real-p)
  (compile-file-if-needed (sys:example-file   "configuration/macos-application-bundle") :load t)
  (setq *delivered-image-name*
        (write-macos-application-bundle "~/Hello.app"   
            :document-types nil)))  

;;; Deliver the application
; Here we introduce as a parameter the function in the source code to read parameters from console and
; solve the problems with the algorithms given 

(deliver 'test-cases::solve-grid-problem *delivered-image-name* 4)
