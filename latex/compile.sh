#!/bin/bash

#---------------------------------------------------------------------
#
#                          clean.sh
#
#---------------------------------------------------------------------
#
# clean.sh
# Copyright 2015 Dr. Francisco J. Pulido
#
# This file belongs to the PhD titled "New Techniques and Algorithms for Multiobjective and Lexicographic Goal-Based Shortest Path Problems", distributed under the Creative Commons Licence Attribution-NonCommercial-NoDerivs 3.0, available in http://creativecommons.org/licenses/by-nc-nd/3.0/. The complete PhD dissertation is freely accessible from http://www.lcc.uma.es/~francis/
#
# This thesis has been written adapting the TeXiS template, a LaTeX template for writting thesis and other documents. The complete TeXiS package can be obtained from http://gaia.fdi.ucm.es/projects/texis/. TeXis is distributed under the same conditions of the LaTeX Project Public License (http://www.latex-project.org/lppl.txt). The complete license is available in http://creativecommons.org/licenses/by-sa/3.0/legalcode

# script to compile the document and generate the references

pdflatex Tesis
bibtex Tesis1
pdflatex Tesis
pdflatex Tesis

