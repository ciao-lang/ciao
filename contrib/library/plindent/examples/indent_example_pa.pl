:- use_package(res_giunif(res_giunif_decl)).
:- use_package(res_gounif(res_gounif_decl)).
:- use_package(res_wamcode(res_wamcode_comp_decl_auto)).

% :- doc(author, "Edison Mera").

% Note: This file is not auto indented, use the auto indenter over this file
% to test it and to generate indent_example_pa.pl

% Bug: space between <&& and . must be preserved
Handler <& :-
	Handler <&&.

% This term have a bug that was fixed
p(a
	    % LOCATORS
	    , loc_unknown/1).

% A bug discovered recently, in the iso_tests.pl:
p(0''').
oo('hola').

% A nasty bug that took me a lot of time to find:
getdeg([node(__)|_], 1,  2,  3).
getdeg([_de|G],      I1, I2, 3).

% If you implement idfunctors:pos_bookmarks/3 in a naive way, the
% following clauses will be indented in a very long (exponential?)
% time:
ciao_config_entry(
	    [
		depend_on([('INSTYPE', InsType), ('SYSAVAIL', SysAvail)]),
		set_value((InsType == 'src', bundle_src(ciaode, Value)),
		    Value),
		default(get_prefix(SysAvail, InsType, DefValue), DefValue),
		query("Specify the directory to perform the installation",
		    [minimum, extended]),
		show("Prefix is", [default])
	    ]).
ciao_config_entry(
	    [
		depend_on([('INSTYPE', InsType), ('SYSAVAIL', SysAvail)]),
		valid_values(['yes', 'no']),
		set_value(\+((InsType == 'src', SysAvail == 'user')), no),
		default('no'),
		query("Specify if you want to install the optimizing compiler",
		    [minimum, extended]),
		show("Install optimizing compiler", [default])
	    ]).
ciao_config_entry(
	    [
		default('yes'),
		valid_values(['yes', 'no']),
		query(
		    "",
		    [extended]),
		show("Stop if error", [default])
	    ]).


copy_option(overwrite). % overwrite
copy_option(timestamp). % preserve time stamp
copy_option(symlink). % create a symbolic link (in windows a shorcut)
copy_option(append). % If the target file exists, append the source to it

a :-
	(
	    f(1,       1),
	    f(222,     22),
	    f(3333,    cccc),
	    f(g(A|[]), cccc),
	    f([],      []),
	    f(4,       dd)
	).

f(a(bbbb, c), [d/e]).
f(a(b,    c), [e/e]).


o :-
	a([e|e],  W),
	a([ed|e], W).


a([],     []).
a(patata, patata).

b :-
	(
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaagnd %
	).

:- a
	+ cost(lb_giunif_length____), cost(lb_gounif_length____), cost(
	    lb_nargs, 3*length(Xs) +3), cost(lb_res_steps, length(Xs) +1), cost(
	    lb_steps, length(Xs) +1).

a :-
	( b -> true
	; false
	),
	(A+B) > (B-D),
	X is ( A+(B+(C
		    +D)) ).

bbb :-
	member(X, [
		a,
		b,
		c]).

i :-
	(
	    b == TopBase, %
	;
	    true
	),
	(
	    caaaaaaa,
	    caaaaaaa,
	    true,
	    true,
	    true
	),
	fail.


r :-
	(
	    (
		q(aa, b),
		q(aa, bbb),
		q(b,  cc)
	    )
	).

bench_file :=
	eight_queen
	|fib
	%	    | lfib
	|hanoi
	%            | insert_stores
	%            | perm
	%            | file
	%            | qsort
	|send_files
	%            | subst_exp
	%            | zebra
	.

a(b,  c).
a(bb, cc).

copy_package_file_ext(PackageExtensionXXPackageNameVersion,
	    PackageTypeSuffixxxxx, CopyFrom).

p(X, Y) :-
	X is 2'1010,
	Y is 0x123456789ABCDEF0,
	Z is 0o12245670.


p :-
	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX ->
	true.

add_factor([factor(F1, C1)|Fs1], factor(F2, C2), [factor(F1, C1)|Fs
	    ]).

:- module(_, _, [assertions]).

conj_unfoldable((_:-Body), BT, ST, UC) :-
	(Val1-GPSol1),
	(true;spy_1, fail),
	unfold_body(Body, BT, ST, UC).

instantiate_pos_bookmarks([],
	    [],
	    _Min, _,
	    [],
	    []).
instantiate_pos_bookmarks([Offset0|Offsets0],
	    [[PosBookmark|PosBookmarks]|PosBookmarkss0],
	    Min,  Max,
	    [offs(OffsetChar, pos(LastCol0, LastLine0))|Offsets],
	    [PosBookmarks|PosBookmarkss]).

p([],     []) :- !.
p([A|As], [B|Bs]) :-
	q(A, B),
	p(As, Bs).

p([], []) :- !.

p([A|As], [B|Bs]) :-
	q(A, B),
	p(As, Bs).

a :-
	b
    ;
	c.

create_autospace(IndentLevel0_, IndentLevel, Lines0, TokenType, Value_,
	    Tokens111, Pos,   Spaces, Arg1,   Arg2).
create_autospace(IndentLevel0,  IndentLevel, Lines0, TokenType, Value,
	    Tokens,    Pos__, Spaces, Arg111, Arg222).

pos_bookmarks_(a(b), _Level, PB,            PB).
pos_bookmarks_(a(b), Level,  PosBookmarks0, PosBookmarks) :-
	pos_bookmarks__(Argdescs, Level, PosBookmarks0, PosBookmarks1),
	pos_bookmarks_(Functors, Level, PosBookmarks1, PosBookmarks).

a.

a(b)

a(b)

b.

funca(aaa(b(ccc, d),    d(e)), e).
funca(aaa(b(c, ddd),    d(e)), e).
funca(aaa(b(c, ddd, e), d(e)), e).

funcb(eee(b(ccc, d)),   e).
funcb(eee(b(c,   ddd)), e).
funcb(eee(b(c,   ddd)), e).

/* Edison's test */

p(0.Inf).
p(-0.Inf).
q(0.Nan).
q(-0.Nan).

abc(1.55, 2.43).
abc(aaa,  bbbd).

funct(a, b, c) :-
	funct(aaaa, b, c).
funct :-
	funct(aaaa, b, c).
funct(a,   bbbbb, c).
funct(a,   b,     cccc).
funct(aaa, bbb,   ccc).

bbbb("""").

cccc('''').

dddd(^(b)).

eeee('').

ffff("").

rrrr("a \". ").

gen_deep_term(X, E) :-
	E = f(
	    a(b(c(d(e(f(g(h(i(j(
					   k(
					    b(
					     c(
					      d(
					       e(
						f(
						g(
						h(
						i(
						j(
						l(b(c(d(e(f(g(h(i(j(
						m(b(c(d(e(f(g(h(i(j(
						n(b(c(d(e(f(g(h(i(j(X
						)
						))))))))))
						))))))))))
						))))))))))
						))))))))))
					 )))))))))).

create_autospace(IndentLevel0, IndentLevel, Lines0, TokenType, Value, Tokens,
	    Pos, Spaces) :-
	true.

p :-
	l234567890l234567890l234567890l234567890l234567890l234567890l2345
    ->
	true ;
	fail.

p :-
l234567890l234567890l234567890l234567890l234567890l234567890l234567890l23
    ->
	true ;
	fail.

p :-
	X = ( ( ( ( ( (
				create_autospace(IndentLevel1, IndentLevel2,
				    DLines, TokenType2, Value2, Tokens2, Pos1,
				    Spaces
				)
			    ) ) ) ) ) ).


:- reexport(inferres(top_res/error_res), [error_message/3]).
:- reexport(inferres(top_res/utility_res),
	    [
		add/3,
		addition/3,
		time_addition/3, % EMM
		append/3,
		close_list/1,
		compound/1,
		empty_queue/2,
		get_queue/3,
		init_queue/2,
		intersection/3,
		ith_list_element/3,
		list/1,
		maximum/3,
		member/2,
		minimum/3,
		minus/2,
		multiply/3,
		time_multiply/3, % EMM
		noncompound/1,
		nonempty_queue/2,
		noninteger/1,
		nonlist/1,
		nonsequence/1,
		opened_set_equivalent/2,
		opened_set_inclusion/2,
		opened_set_insertion/2,
		opened_set_member/2,
		opened_set_union/2,
		pop/3,
		push/3,
		set_put_queue/3,
		sub/3,
		subterm/2,
		subtraction/3,
		union/3
	    ]).

testbuiltin :-
	(
	    testbuiltin2.
