CIAOCMD ?= ciao

# TODO: Use ciao_builder instead

# Known suffixes.  This allos make to take care of time dependencies.
.SUFFIXES: .java .class

# Java sources, etc.
JAVAC=javac
JAVAEXECUTE=java
JAVAFLAGS=-classpath ./:../../library/javall/
# We need to list them explicitly because there are no .class file
# in a clean distribution
JAVACLASSES=PhoneInsert.class PhoneList.class PhoneSearch.class
JAVASOURCES=*.java

# Main Prolog file
PROLOGMAIN=agenda.pl

# How to compile a .java into a .class
.java.class:
	$(JAVAC) $(JAVAFLAGS) $<

all: classes prolog

classes: $(JAVACLASSES)

prolog:
	$(CIAOCMD) comp $(PROLOGMAIN)

clean realclean:
	/bin/rm -f *.class
	$(CIAOCMD) clean_tree "."

distclean: realclean
	/bin/rm -f *~ 


