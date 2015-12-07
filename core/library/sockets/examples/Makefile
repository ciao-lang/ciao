CIAOCMD ?= ciao

compile:
	$(CIAOCMD) comp -x client
	$(CIAOCMD) comp -x server

clean:
	$(CIAOCMD) clean_tree "."
	rm -f client server

realclean distclean: clean
	/bin/rm -f *~
