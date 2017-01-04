TARGET=loc

${TARGET}.pdf: ${TARGET}.tex
	latexmk -pdf ${TARGET}.tex

.PHONY: clean
clean:
	latexmk -c ${TARGET}.tex
