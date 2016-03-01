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
# In order to create the PDF file, just type
#
# make 
#

# Variables
MAINFILE=PhD_Pulido
NOTES=PhD_Pulido_Notes
DATETIME = $(shell date +"%Y%m%d_%H%M%S")

all: $(MAINFILE).pdf $(NOTES).pdf; make clean

.PHONY: all clean

## Slides compilation
$(MAINFILE).pdf: $(MAINFILE).tex
	echo "\isnotesfalse">Temp.tex
	@echo "Creating PDF file " $(MAINFILE).pdf
	# Initial run
	pdflatex $(MAINFILE)

	# Recompile if necessary
	@if(grep "Rerun" $(MAINFILE).log > /dev/null);\
	then \
		pdflatex $(MAINFILE); \
	fi

## Notes compilation
$(NOTES).pdf: $(MAINFILE).tex
	echo "\isnotestrue">Temp.tex
	# Initial run
	pdflatex -jobname $(NOTES) $(MAINFILE) 

	# Recompile if necessary
	@if(grep "Rerun" $(NOTES).log > /dev/null);\
	then \
		pdflatex -jobname $(NOTES) $(MAINFILE); \
	fi
	
## Create a snapshot 	
snapshot:
	@echo
	@echo "  Making a snapshot of all files and folders"
	mkdir -p Snapshots/$(DATETIME)
	rsync -a --exclude '.git' --exclude 'Snapshots' ./ Snapshots/$(DATETIME)/
	cd Snapshots/ ; zip -qr $(DATETIME).zip $(DATETIME)
	cd Snapshots/ ; rm -rf $(DATETIME)
	@echo

## Remove unnecessary files
clean:                   
	-rm -f *.log *.aux *.out *.bbl *.blg *.bbl *.nav *.snm *.toc Temp.tex
