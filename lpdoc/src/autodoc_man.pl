:- module(autodoc_man, [], [assertions, regtypes, fsyntax]).
% (Nothing is exported, because everything works using hooks)

:- doc(title,"Man Pages (man) Backend").
:- doc(author,"Jose F. Morales").
:- doc(author,"Manuel Hermenegildo").

:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_doctree)).
:- use_module(lpdoc(autodoc_images)).
:- use_module(lpdoc(autodoc_aux), [ascii_blank_lines/2]).
:- use_module(lpdoc(comments), [stringcommand/1]).
%:- use_module(lpdoc(autodoc), 
%     [version_date/2, version_numstr/2]).

:- use_module(library(lists), [list_concat/2]).
:- use_module(library(format_to_string), [format_to_string/3]).

% ======================================================================

:- multifile autodoc_rw_command_hook/4.

:- pred autodoc_rw_command_hook(Backend, DocSt, Command, NewCommand)
	: backend_id * docstate * doctree * doctree.

autodoc_rw_command_hook(man, DocSt, Command, NewCommand) :- !,
	rw_command(Command, DocSt, NewCommand).

% ......................................................................

rw_command(sp(NS), _, NewAll) :- !,
	number_codes(N, NS),
	N1 is N+1,
	ascii_blank_lines(N1, NewCommand),
	NewAll = raw(NewCommand).
rw_command(p(""),                  _, [raw_fc, raw_nleb]) :- !.
rw_command(noindent(""),           _, []) :- !.
rw_command(mathenv(S),             _, [raw(S)]) :- !. % TODO: not supported
rw_command(mathenv(_,S),           _, [raw(S)]) :- !. % TODO: not supported
rw_command(defmathcmd_(_,_,_),     _,    []) :- !. % TODO: Not supported
rw_command(newblock(""),           _, [raw_nl]) :- !. % TODO: remove? just for bibrefs
rw_command(env_('itemize', X),     _, [raw_fc, raw(".(l F"), raw_nleb, X, raw_fc, raw(".)l"), raw_nleb]) :- !.
rw_command(env_('enumerate', X),   _, [raw_fc, raw(".(l F"), raw_nleb, X, raw_fc, raw(".)l"), raw_nleb]) :- !.
rw_command(env_('description', X), _, [raw_fc, raw(".(l F"), raw_nleb, X, raw_fc, raw(".)l"), raw_nleb]) :- !.
rw_command(env_('cartouche', X),   _, [raw_fc, raw_nleb, X, raw_fc, raw_nleb]) :- !.
rw_command(env_('alert', X),       _, [raw_fc, raw_nleb, X, raw_fc, raw_nleb]) :- !.
rw_command(env_('verbatim', X),    _, [raw_fc, raw(".DS"), raw_nleb, X, raw_fc, raw(".DE"), raw_nleb]) :- !.
rw_command(item(S), _, NBody) :- !, % (items for lists and descriptions)
	( doctree_is_empty(S) ->
	    NBody = [raw_fc, raw("* ")]
	; NBody = [raw_fc, raw("* "), S, raw(": ")]
	).
rw_command(item_num(S), _, NBody) :- !, % (items for enumerations)
	( S = "" ->
	    NBody = [raw_fc] % TODO: Wrong, not supported
	; NBody = [raw_fc, S, raw(". ")]
	).
rw_command(footnote(Text), _DocSt, NBody) :- !,
	NBody = [raw_fc, raw(".B Note: "), raw_nl, Text, raw_nl, raw_nleb].
rw_command('}',                    _, raw("}")) :- !.
rw_command('{',                    _, raw("{")) :- !.
rw_command('@',                    _, raw("@")) :- !.
rw_command(today(""),              _, raw("\*(td")) :- !.
rw_command(hfill(""),              _, []) :- !.
rw_command('`'([X]),               _, raw([X, 0'\\, 0'*, 0'`])) :- !.
rw_command(''''([X]),              _, raw([X, 0'\\, 0'*, 0''])) :- !.
 % NOTE: Escaped ^ due to fsyntax!
rw_command(^'^'([X]),               _, raw([X, 0'\\, 0'*, 0'^])) :- !.
rw_command('..'([X]),              _, raw([X])) :- !.
rw_command('"'([X]),               _, raw([X])) :- !.
 % NOTE: Escaped ~ due to fsyntax!
rw_command(^'~'([X]),               _, raw([X, 0'\\, 0'*, 0'~])) :- !.
rw_command('='([X]),               _, raw([X])) :- !.
rw_command('.'([X]),               _, raw([X])) :- !.
rw_command('u'([X]),               _, raw([X, 0'\\, 0'*, 0':])) :- !.
rw_command('v'([X]),               _, raw([X, 0'\\, 0'*, 0'v])) :- !.
rw_command('H'([X]),               _, raw([X])) :- !.
rw_command('t'([X, Y]),            _, raw([X, Y])) :- !.
rw_command('c'([X]),               _, raw([X, 0'\\, 0'*, 0',])) :- !.
rw_command('d'([X]),               _, raw([X])) :- !.
rw_command('b'([X]),               _, raw([X])) :- !.
rw_command('oe'(""),               _, raw("oe")) :- !.
rw_command('OE'(""),               _, raw("OE")) :- !.
rw_command('ae'(""),               _, raw("ae")) :- !.
rw_command('AE'(""),               _, raw("AE")) :- !.
rw_command('aa'(""),               _, raw("aa")) :- !.
rw_command('AA'(""),               _, raw("AA")) :- !.
rw_command('o'(""),                _, raw("o")) :- !.
rw_command('O'(""),                _, raw("O")) :- !.
rw_command('l'(""),                _, raw("l")) :- !.
rw_command('L'(""),                _, raw("L")) :- !.
rw_command('ss'(""),               _, raw("ss")) :- !.
rw_command('?'(""),                _, raw("?")) :- !.
rw_command('!'(""),                _, raw("!")) :- !.
rw_command('i'(""),                _, raw("i")) :- !.
rw_command('j'(""),                _, raw("j")) :- !.
rw_command(copyright(""),          _, raw("(c)")) :- !.
rw_command(iso(""),                _, raw("[*ISO*]")) :- !.
rw_command(bullet(""),             _, [raw_fc, raw("* ")]) :- !.
rw_command(result(""),             _, raw("=>")) :- !.
rw_command(href(URL),              _, raw(URL)) :- !.
rw_command(href(URL, Text), _DocSt, NBody) :- !,
	NBody = [Text, raw(" ("), raw(URL), raw(")")].
rw_command(email(Address), _DocSt, Address) :- !.
rw_command(email(Text, Address), _DocSt, NBody) :- !,
	NBody = [Text, raw(" ("), Address, raw(")")].
rw_command(image_auto(IFile, _), DocSt, NBody) :- !,
	locate_and_convert_image(IFile, ['txt'], DocSt, IFile2),
	NBody = [raw("[Image: "), raw(IFile2), raw("]")].
%% Commands with a more or less direct translation to a man command
rw_command(Command, _DocSt, NewAll) :-
	rw_command_body(Command, NewCommand, RBody),
	!,
	NewAll = [raw_fc, raw("."), raw(NewCommand), raw(" "), RBody, raw_nleb].
% .......... (icmd) ..........
rw_command(section_env(SecProps, _SectLabel, TitleR, Body), _DocSt, R) :- !,
	fmt_structuring(SecProps, TitleR, SectR),
	R = [SectR, Body].
rw_command(bibitem(Label,_Ref), _DocSt, R) :- !,
	R = [item(bf([string_esc("["), string_esc(Label), string_esc("]")]))].
rw_command(idx_anchor(_, _, _, _, R0), _, R) :- !, R = R0.
rw_command(simple_link(_,_,_,_), _, nop) :- !.
rw_command(man_page(TitleR, Version, AuthorRs, AddressRs, SummaryR, UsageR, CopyrightR), DocSt, R) :- !,
        fmt_man(TitleR, Version, AuthorRs, AddressRs, SummaryR, UsageR, CopyrightR, DocSt, R).
rw_command(X, _DocSt, _R) :- !,
	throw(error(domain_error, rw_command/3-env(['X'=X]))).

rw_command_body(bf(Body),    "B", Body) :- !.
rw_command_body(em(Body),    "I", Body) :- !.
rw_command_body(tt(Body),    "B", Body) :- !.
%% rw_command_body(tt(Body),"r",Body) :- !.
rw_command_body(key(Body), "B",          Body) :- !.
rw_command_body(var(Body), "I",          Body) :- !.
rw_command_body(missing_link(Text), "I", R) :- !, R = raw(Text).
rw_command_body(ref_link(_Link, Text), "I", R) :- !, R = raw(Text).

fmt_man(TitleR, Version, AuthorRs, AddressRs, SummaryR, UsageR, CopyrightR, DocSt, R) :-
	( version_date(Version, Date),
	  version_numstr(Version, VerStr) ->
	    true
	; Date = ' '
	),
	format_to_string("~w", [Date], DateStr),
	%	
	( doctree_is_empty(TitleR) ->
	    TitleR2 = []
	; TitleR2 = [raw("\\- "), TitleR, raw("."), raw_nl]
	),
	( Version = [] ->
	    VersionR = []
	; VersionR = [raw_nl,
	              raw_nl,
		      raw(".SH VERSION"), raw_nl,
		      raw("This man page corresponds to version "),
		      raw(VerStr), raw(" ("), raw(DateStr), raw(")."),
		      raw_nl]
	),
	( UsageR = [] ->
	    UsageR2 = UsageR
	; UsageR2 = [raw_nl,
	             raw_nl,
		     raw(".SH SYNOPSIS"), raw_nl,
		     UsageR, raw_nl]
        ),
	sep_nl(AuthorRs, AuthorRs2),
	( AddressRs = [] ->
	    AddressRs2 = []
	; sep_nl([string_esc("")|AddressRs], AddressRs2)
	),
	docst_modname(DocSt, ModName),
	atom_codes(ModName, ModNameS),
	R = [raw(".TH "), raw(ModNameS),
	     raw(" l """), raw(DateStr), raw(""""), raw_nl,
	     raw_nl,
	     raw(".SH NAME"), raw_nl,
	     raw(".B "), raw(ModNameS), raw_nl,
	     TitleR2,
	     raw(".IX "), raw(ModNameS), raw_nl,
	     raw_nl,
	     raw_nl,
	     raw(".SH DESCRIPTION"), raw_nl,
	     SummaryR,
	     raw_nl,
	     VersionR,
	     UsageR2,
	     raw_nl,
	     raw_nl,
	     raw(".SH MORE INFO"), raw_nl,
	     raw("This man page has been generated "), raw_nl,
	     raw("automatically by the lpdoc autodocumenter. "), raw_nl,
	     raw("The "), raw(ModNameS), raw(" reference manual "), raw_nl,
	     raw("provides further information. Versions of the "), raw_nl,
	     raw("manual are available on-line and in printable form "), raw_nl,
	     raw("(info/html/dvi/ps/...) - see the source or your "), raw_nl,
	     raw("local installation information."), raw_nl,
	     raw(".SH COPYRIGHT"), raw_nl,
	     CopyrightR,
	     raw_nl,
	     raw_nl,
	     raw(".SH AUTHOR"), raw_nl,
	     AuthorRs2,
	     AddressRs2].

sep_nl([],                 []).
sep_nl([X|Xs], Rs) :-
	Rs = [raw(".br"),raw("\n"),X,raw("\n")|Rs0],
	sep_nl(Xs, Rs0).

fmt_structuring(SecProps, TitleR, R) :-
	( section_prop(level(Level), SecProps) ->
	    ( Level = 2 -> R = [raw_fc, raw(".SH 1 "), TitleR, raw_nleb]
	    ; Level = 3 -> R = [raw(".SH 2 "), TitleR, raw_nleb]
	    ; Level = 4 -> R = [raw(".SH 3 "), TitleR, raw_nleb]
	    ; throw(error(bad_level(Level), fmt_structuring/3))
	    )
	; throw(error(missing_level_prop(SecProps), fmt_structuring/3))
	).

% ===========================================================================

:- multifile autodoc_finish_hook/1.
autodoc_finish_hook(man) :- true.

:- multifile autodoc_gen_alternative_hook/2.
% note: no alternative formats here
autodoc_gen_alternative_hook(man, _) :- fail.

