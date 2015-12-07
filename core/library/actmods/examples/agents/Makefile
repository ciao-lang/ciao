CIAOCMD ?= ciao

build: 
	$(CIAOCMD) comp -a actmods/webbased_publish agent1
	agent1 &
	$(CIAOCMD) comp -a actmods/webbased_publish agent2
	agent2 &
	$(CIAOCMD) comp test

distclean:
	$(CIAOCMD) clean_tree "."
	/bin/rm -f *~ *.addr agent1 agent2 test

clean:	distclean
	/bin/rm -rf traces/agent1 traces/agent2
