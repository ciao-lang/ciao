# TODO: Use ciao_builder instead

PROLOGSOURCES=*.pl

all: prolog

prolog:
	$(CIAOCMD) comp -x $(PROLOGSOURCES)

clean realclean:
	/bin/rm -f $(JAVACLASSES)
	if [ -x $(CIAOCMD) ]; then $(CIAOCMD) clean_tree "."; fi

distclean: realclean
	/bin/rm -f *~ calculator



