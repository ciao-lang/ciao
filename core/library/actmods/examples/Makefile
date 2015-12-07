CIAOCMD ?= ciao

build: simple_server.pl simple_client_with_main.pl
	$(CIAOCMD) comp -a actmods/filebased_publish simple_server
	$(CIAOCMD) comp simple_client_with_main

webbased:
	cd webbased_server; gmake
	$(CIAOCMD) comp -a actmods/webbased_publish simple_server

distclean:
	$(CIAOCMD) clean_tree "."
	/bin/rm -f *~ *.addr simple_server simple_client_with_main
# webbased_server
