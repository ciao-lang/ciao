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
	$(CIAOCMD) clean_tree "."

distclean: realclean
	/bin/rm -f *~ *.class
