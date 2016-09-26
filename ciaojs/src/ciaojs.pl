:- module(ciaojs, [], [assertions, doccomments]).

%! \title A simple inteface for Ciao.js
%
%  \module This defines a `query_fs/0` predicate that executes queries
%  and returns solutions using a minimalistic text-based
%  interface. Internally this is using the filesystem emulation.

:- doc(bug, "Remove main/0 (need main-less executables)").
:- doc(bug, "Suspend findall, ").

:- use_module(library(compiler)). % allow use_module
:- use_module(library(read), [read_term/3]).
:- use_module(library(write)).
:- use_module(library(aggregates)).

:- export(main/0).
main :- true. % TODO: Allow main-less executables for emscripten

:- export(query_fs/0).
% Execute a query and get all solutions
% (input and output from reserved files on file system)
query_fs :-
	read_query(Query),
	query(Query, Sols),
	write_sols(Sols).

read_query(Query) :-
	open('/.ciaojs-in.pl', read, In),
	Opts = [variable_names(Vs)],
	catch(read_term(In, Query0, Opts), _E, Err=yes),
	close(In),
	( Err == yes ->
	    Query = malformed
	; Query0 = notmpl(Goal) -> % No template, use variable names
	    Query = tmpl(Vs, Goal)
	; Query0 = tmpl(_,_) ->
	    Query = Query0
	; Query = malformed
	).

% Write one solution per line
write_sols(Sols) :-
	open('/.ciaojs-out.pl', write, Out),
	( member(Sol, Sols),
	  writeq(Out, Sol), nl(Out),
	  fail
	; true
	),
	close(Out).

query(tmpl(Template, Goal), Sols) :-
	% TODO: do not do findall; allow blocking execution
	findall(Sol, query_(Goal,Template,Sol), Sols).
query(malformed, Sols) :-
	Sols = [malformed].

% Execute `Goal` and get one solution (`success(Template)` or `exception(_)`)
query_(Goal, Template, Sol) :-
	catch(port_query_(Goal, Template, Sol), E, Sol=exception(E)).

port_query_(Goal, Template, success(Template)) :- call(Goal).
