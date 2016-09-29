%%---------------------------------------------------------------------
%%
%% TCL CANVAS WIDGET
%%
%%---------------------------------------------------------------------

:- class(canvas_class,[],[objects]).

:- use_class(library(class/examples/geometry/shape_class)).

:- use_module(library(concurrency)).
:- use_module(library(lists), [append/3]).
:- use_module(library(strings)).
:- use_module(library(system)).

:- data cmd_stream/1.
:- data item/2.

:- inheritable item/2.

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

canvas_class :-
	new_tcl_interpreter,
	self_codes(ID),
	command(['canvas .canvas',ID]),
	command(['pack .canvas',ID]),
	true.

canvas_class([]) :-
	canvas_class.

canvas_class([Item|Next]) :-
	( add_item(Item) ; true ),
	!,
	canvas_class(Next).

:- set_prolog_flag(multi_arity_warnings,on).

destructor :-
	self(Canvas),
%	command(["destroy .canvas",CanvasID),
	retract_fact(item(Shape,_)),
	Shape:remove_owner(Canvas),
	fail.
destructor :-
	kill_interpreter.

%%---------------------------------------------------------------------
%% ADD/REMOVE ITEMS
%%---------------------------------------------------------------------

:- export(add_item/1).
:- export(remove_item/1).
:- export(item_changed/1).

add_item(Shape) :-
	\+ item(Shape,_),
	Shape instance_of shape_class,
	assertz_fact(item(Shape,hidden)),
	self(Canvas),
	Shape:add_owner(Canvas),
	!.
add_item(_).

remove_item(Shape) :-
	hide_item(Shape),
	retract_fact(item(Shape,_)),
	Shape instance_of shape_class,
	self(Canvas),
	Shape:remove_owner(Canvas),
	!.
remove_item(_).


item_changed(Shape) :-
	hide_item(Shape),
	show_item(Shape).

%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE CANVAS
%%---------------------------------------------------------------------

:- export(show/0).

show :-
	item(Shape,hidden),
	show_item(Shape),
	fail.
show.

:- export(hide/0).

hide :-
	item(Shape,shown),
	hide_item(Shape),
	fail.
hide.

%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC ITEMS
%%---------------------------------------------------------------------

:- export(show_item/1).

show_item(Shape) :-
	self_codes(Canvas),
	item(Shape,hidden),
	Shape instance_of shape_class,
	Shape:tcl_name(ItemName),
	Shape:creation_options(Opts),
	instance_codes(Shape,ShapeID),
	codify([".canvas",Canvas," create ",ItemName],Command),
	codify(Opts,Options),
	codify([Command," ",Options," -tags tag",ShapeID],Aux),
	command_string(Aux),
%	write_string(Aux),nl,
	retract_fact(item(Shape,hidden)),
	asserta_fact(item(Shape,shown)).

:- export(hide_item/1).

hide_item(Shape) :-
	self_codes(Canvas),
	retract_fact(item(Shape,shown)),
	instance_codes(Shape,ShapeID),
	command([".canvas",Canvas," delete tag",ShapeID]),
	asserta_fact(item(Shape,hidden)).

%%---------------------------------------------------------------------
%% TCL SUPPORT
%%---------------------------------------------------------------------

command(Cmd) :-
	codify(Cmd,Str),
	command_string(Str).

command_string(Str) :-
	lock_atom(canvas_class),
	cmd_stream(Stream),
	write_string(Stream,Str),
	flush_output(Stream),
	nl(Stream),
	flush_output(Stream),
	unlock_atom(canvas_class).

%%---------------------------------------------------------------------

new_tcl_interpreter :-
	( kill_interpreter ; true ),
	!,
	popen(wish,write,Strm), % TODO: zombie process! (use process_call, close, and process_join)
	asserta_fact(cmd_stream(Strm)).

kill_interpreter :-
	cmd_stream(Strm),
	command_string("uplevel 0 exit"),
	retract_fact(cmd_stream(_)),
	close(Strm).

%%---------------------------------------------------------------------
%% MACROS
%%---------------------------------------------------------------------

self_codes(S) :-
	self(Me),
	instance_codes(Me,S).

:- inheritable(self_codes/1).

%%---------------------------------------------------------------------


codify([],"").

codify([''|Next],[C,C|CNext]) :-
	!,
	atom_codes('\"',[C]),
	codify(Next,CNext).

codify([[]|Next],[C,C|CNext]) :-
	!,
	atom_codes('\"',[C]),
	codify(Next,CNext).

codify([X|Next],Str) :-
	atom(X),
	!,
	atom_codes(X,XCodes),
	codify(Next,CNext),
	append(XCodes,CNext,Str).

codify([X|Next],Str) :-
	number(X),
	number_codes(X,XCodes),
	!,
	codify(Next,CNext),
	append(XCodes,CNext,Str).

codify([Term|Next],Str) :-
	ground(Term),
	functor(Term,_,1),
	arg(1,Term,Arg),
	atom(Arg),
	atom_codes(Arg,X),
	!,
	codify(Next,CNext),
	append(X,CNext,Str).

codify([X|Next],Str) :-
	!,
	codify(Next,CNext),
	append(X,CNext,Str).
