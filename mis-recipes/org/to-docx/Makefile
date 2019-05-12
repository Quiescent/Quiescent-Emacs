#______________________________________________________________________________


DIRORG = $(shell dir *.org)

DIRDOCX = $(DIRORG:.org=.docx)

all: clean Makefile $(DIRDOCX)

%.docx: %.org
	pandoc -f org -t docx $^ -o $@
	echo $@ >> provide

clean:
	rm -f provide

install-tools:
	echo "Please install pandoc"

.PHONY: all install-tools clean
