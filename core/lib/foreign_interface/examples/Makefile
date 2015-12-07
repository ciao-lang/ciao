CIAOCMD ?= ciao

phony:

test:
	$(CIAOCMD) comp test_all
	./test_all

clean: 
	$(CIAOCMD) clean_tree "."

realclean distclean: clean
	/bin/rm -f *~ test_all

