USER=dlichteblau

all: index.html installation.html example.html

%.html: %.xml index.xsl
	xsltproc index.xsl $< >$@.tmp
	rm $@
	mv $@.tmp $@
	chmod -w $@

.PHONY: push
push: all
	scp index.html index.css $(USER)@common-lisp.net:/project/xuriella/public_html/xuriella/
