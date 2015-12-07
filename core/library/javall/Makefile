CIAOCMD ?= ciao

# TODO: Use ciao_builder instead

# TODO: build from java.hooks.pl
PROLOGSERVER=plserver

all: build docs

build:
	$(CIAOCMD) comp -x $(PROLOGSERVER)
	cd CiaoJava && $(MAKE) all
	cd examples/java2pl && $(MAKE) all
	cd examples/pl2java && $(MAKE) all

docs:
	cd javadoc && $(MAKE) all

clean realclean:
	$(CIAOCMD) clean_tree "."
	cd CiaoJava && $(MAKE) clean
	cd examples/java2pl && $(MAKE) clean
	cd examples/pl2java && $(MAKE) clean
	cd javadoc && $(MAKE) clean

distclean: realclean
	/bin/rm -f *~ plserver
	cd CiaoJava && $(MAKE) distclean
	cd examples/java2pl && $(MAKE) distclean
	cd examples/pl2java && $(MAKE) distclean
	cd javadoc && $(MAKE) distclean
