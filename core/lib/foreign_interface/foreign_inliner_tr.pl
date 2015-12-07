:- module(foreign_inliner_tr,[foreign_inliner_tr/3],[assertions]).
:- use_module(library(write), [write/2]).
:- use_module(library(strings), [write_string/2]).
:- use_module(library(compiler/c_itf), [defines_module/2]).


:- data opened/0.
:- data inliner_dest/2.

foreign_inline_file(Module,InlineFile):-
	defines_module(Base,Module),
	atom_concat(Base,'_inline.c',InlineFile).

do_inliner_init(Clause, M, S) :-
	(   inliner_dest(S,M) ->
	    Clause = []
	;
	    % The next will obtain the Destination Stream for the inline functions
	    foreign_inline_file(M,InlineFile),
	    open(InlineFile,write,S),
	    assertz_fact(opened),
	    write(S,'/\* Warning: This file has been automatically generated from '),
	    write(S,M),
	    write(S,'.pl \*/\n'),
	    assertz_fact(inliner_dest(S,M)),
	    atom_concat(M,'_inline',MI),
	    Clause = [(:- use_foreign_source([MI]))]
	).

write_inline_entry(S,Term,Text) :-
	write(S,'\n/\* '),
	write(S,Term),
	write(S,' \*/\n'),
	write_string(S,Text).

foreign_inliner_tr(end_of_file, end_of_file, M) :-
	(   opened ->
	    inliner_dest(S,M),
	    close(S),
	    retractall_fact(opened)
	;
	    true
	),
	retractall_fact(inliner_dest(_,M)).

foreign_inliner_tr((:- foreign_inline(Term,Text)), Clause, M) :-
	do_inliner_init(Clause, M, S),
	write_inline_entry(S,Term,Text).

foreign_inliner_tr((:- foreign_inline(Text)), Clause, M) :-
	do_inliner_init(Clause, M, S),
	write_inline_entry(S,'Global declaration',Text).
