# TODO: Use ciao_builder instead

CIAOCMD ?= ciao

JAVASOURCES=*.java
JAVACLASSES=*.class
JAVAC=javac
JAVAFLAGS=

all: classes

classes:
	-$(JAVAC) $(JAVAFLAGS) $(JAVASOURCES)

clean realclean:
	/bin/rm -f $(JAVACLASSES)
	if [ -x $(CIAOCMD) ]; then $(CIAOCMD) clean_tree "."; fi

distclean: realclean
	/bin/rm -f *~ *.class
