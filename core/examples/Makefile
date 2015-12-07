# TODO: Use ciao_builder instead

CIAOCMD ?= ciao

phony:

clean:
	$(CIAOCMD) clean_tree "."

realclean: clean
	/bin/rm -rf hw hw.cpx
	cd misc && $(MAKE) realclean

distclean: realclean
	/bin/rm -rf *~
	cd misc && $(MAKE) distclean
