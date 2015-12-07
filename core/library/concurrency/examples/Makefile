CIAOCMD ?= ciao

compile:
	$(CIAOCMD) comp examples
	$(CIAOCMD) comp -x foo
	$(CIAOCMD) comp -x goo

clean:
	$(CIAOCMD) clean_tree "."
	rm -f examples foo goo

distclean realclean: clean
	rm -f *~
