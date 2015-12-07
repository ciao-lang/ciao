CIAOCMD ?= ciao

# TODO: Use ciao_builder instead

JAVASOURCES=*.java
PROLOGSOURCES=*.pl
JAVACLASSES=*.class
JAVAC=javac
JAVAEXECUTE=java
# TODO: ask 'ciao' about the class paths
JAVALL=../..
JAVAFLAGS=-cp $(JAVALL):./
JAVACFLAGS=-classpath $(JAVALL)
LOGFILES=plserver.out plserver.err
PROLOGSERVER=$(JAVALL)/plserver

all: classes prolog

run:
	$(JAVAEXECUTE) $(JAVAFLAGS) queens $(PROLOGSERVER)

classes:
	-$(JAVAC) $(JAVACFLAGS) $(JAVASOURCES)

prolog:
	$(CIAOCMD) comp -c $(PROLOGSOURCES)

clean realclean:
	$(CIAOCMD) clean_tree "."
	/bin/rm -f $(JAVACLASSES)
	/bin/rm -f $(LOGFILES)

distclean: realclean
	/bin/rm -f *~ 


