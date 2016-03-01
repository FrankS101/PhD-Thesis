#---------------------------------------------------------------------
#
#                          Makefile
#
#---------------------------------------------------------------------
#
# Makefile
# Copyright 2015 Dr. Francisco J. Pulido
#
# This file belongs to the PhD titled "New Techniques and Algorithms for Multiobjective and Lexicographic Goal-Based Shortest Path Problems", distributed under the Creative Commons Licence Attribution-NonCommercial-NoDerivs 3.0, available in http://creativecommons.org/licenses/by-nc-nd/3.0/. The complete PhD dissertation is freely accessible from http://www.lcc.uma.es/~francis/
#
# This thesis has been written adapting the TeXiS template, a LaTeX template for writting thesis and other documents. The complete TeXiS package can be obtained from http://gaia.fdi.ucm.es/projects/texis/. TeXis is distributed under the same conditions of the LaTeX Project Public License (http://www.latex-project.org/lppl.txt). The complete license is available in http://creativecommons.org/licenses/by-sa/3.0/legalcode
#
# In order to create the PDF file, just type
#
# make 
#

# Variables
MAINFILE=Tesis
BIBFILE=Tesis1
DATETIME = $(shell date +"%Y%m%d_%H%M%S")

# Default target
all:
	make clean
	make pdf
	make clean

pdf:	$(MAINFILE).tex    
	@echo "Creating PDF file " $(MAINFILE).pdf
	pdflatex $(MAINFILE)

	# Run BibTex if necessary
	@if(grep "Citation" $(MAINFILE).log >/dev/null);\
	then \
	    bibtex $(BIBFILE);\
	    pdflatex $(MAINFILE); \
	fi

	# Recompile if necessary
	@if(grep "Rerun" $(MAINFILE).log > /dev/null);\
	then \
		pdflatex $(MAINFILE); \
	fi
	@echo

snapshot:
	@echo
	@echo "  Making a snapshot of all files and folders"
	mkdir -p Snapshots/$(DATETIME)
	rsync -a --exclude '.git' --exclude 'Snapshots' ./ Snapshots/$(DATETIME)/
	cd Snapshots/ ; zip -qr $(DATETIME).zip $(DATETIME)
	cd Snapshots/ ; rm -rf $(DATETIME)
	@echo

clean:
	@echo
	@echo "  Removing temporary files"
	-rm -f *.log *.lof *.lot *.out *.bbl *.blg *.toc *.cdx *.idx Tesis1*
	find . -type f -name '*.aux' -delete
	@echo

