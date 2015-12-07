CIAOCMD ?= ciao

webbased:
	$(CIAOCMD) comp -a actmods/webserver_publish webbased_server
	webbased_server &

distclean:
	$(CIAOCMD) clean_tree "."
	/bin/rm -f *~ *.addr webbased_server
