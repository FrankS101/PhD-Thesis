TEXFILE=PhD_Pulido
NOTES=PhD_Pulido_Notes

all: $(TEXFILE).pdf $(NOTES).pdf; make clean

.PHONY: all clean

## Slides compilation
$(TEXFILE).pdf: $(TEXFILE).tex
	echo "\isnotesfalse">Temp.tex
	# Initial run
	pdflatex $(TEXFILE)

	# Recompile if necessary
	@if(grep "Rerun" $(TEXFILE).log > /dev/null);\
	then \
		pdflatex $(TEXFILE); \
	fi

## Notes compilation
$(NOTES).pdf: $(TEXFILE).tex
	echo "\isnotestrue">Temp.tex
	# Initial run
	pdflatex -jobname $(NOTES) $(TEXFILE) 

	# Recompile if necessary
	@if(grep "Rerun" $(NOTES).log > /dev/null);\
	then \
		pdflatex -jobname $(NOTES) $(TEXFILE); \
	fi

## Remove unnecessary files
clean:                   
	-rm -f *.log *.aux *.out *.bbl *.blg *.bbl *.nav *.snm *.toc Temp.tex
