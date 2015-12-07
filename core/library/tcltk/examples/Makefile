CIAOCMD ?= ciao

# TODO: Use ciao_builder instead

compile:
	$(CIAOCMD) comp -x phone_book
	$(CIAOCMD) comp -x tcl_calculator
	$(CIAOCMD) comp -x tcl_factorial
	$(CIAOCMD) comp -x tcl_queens
	$(CIAOCMD) comp -x canvas

clean:
	$(CIAOCMD) clean_tree "."

distclean: clean
	rm -f *~ phone_book queens tcl_calculator tcl_factorial canvas
