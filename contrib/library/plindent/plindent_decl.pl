:- package(plindent_decl).
:- use_package(argnames).

:- argnames functordesc(name, tokentype, arg, indentlevel, pos, argdescs,
	    parlevel).

:- argnames functorproc(name, tokentype, arg, argt, indentlevel, pos,
	    argdescs, argdescst, parlevel).

:- argnames argdesc(arg, pos, bookmark).

:- argnames token(type, value).

:- argnames pliconfig(max_length_line, indentation_style).
