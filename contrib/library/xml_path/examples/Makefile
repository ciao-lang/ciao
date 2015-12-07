CIAOCMD ?= ciao

compile:
	$(CIAOCMD) comp xml_path_test

clean:
	$(CIAOCMD) clean_tree "."
	-rm -f xml_path_test *.cpx

distclean realclean: clean
	-rm -f *~ xml_indexed.pl
