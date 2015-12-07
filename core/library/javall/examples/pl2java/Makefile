# TODO: Use ciao_builder instead

PROLOGSOURCES=*.pl

all: prolog

prolog:
	$(CIAOCMD) comp -x $(PROLOGSOURCES)

clean realclean:
	/bin/rm -f $(JAVACLASSES)
	$(CIAOCMD) clean_tree "."

distclean: realclean
	/bin/rm -f *~ calculator



