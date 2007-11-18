USER=dlichteblau

all: index.html installation.html

%.html: %.xml index.xsl
	xsltproc index.xsl $< >$@.tmp
	mv $@.tmp $@
	chmod -w $@

.PHONY: push
push: all
	scp index.html index.css $(USER)@common-lisp.net:/project/xuriella/public_html/xuriella/
