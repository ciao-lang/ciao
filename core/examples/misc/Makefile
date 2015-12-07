CIAOCMD ?= ciao

# TODO: Use ciao_builder instead

suite:
	$(CIAOCMD) comp suite

exec_suite: suite
	./suite

clean:
	$(CIAOCMD) clean_tree "."

realclean: clean
	/bin/rm -rf suite suite.cpx

distclean: realclean
	/bin/rm -rf *~
