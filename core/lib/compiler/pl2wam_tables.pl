:- module(_,
	[
	    builtin_heap_usage/2,
	    inline_codable/1,
	    eval_builtin/1,
	    name_of_builtin/3,
	    name_of_builtin/4,
	    name_of_builtin/5,
	    name_of_function/3,
	    name_of_function/4
	], []).

%-----------------------------------------------------------------------------
%                              TABLES
%-----------------------------------------------------------------------------
% The following predicates are compiled using special instructions.

/*** B_GAUGE
inline_codable('PROFILE POINT'(_)) :- !.
E_GAUGE ***/
inline_codable('basiccontrol:CHOICE IDIOM'(_)) :- !.
inline_codable('basiccontrol:CUT IDIOM'(_)) :- !.
inline_codable('arithmetic:is'(_,_)) :- !.
inline_codable('term_basic:='(_,_)) :- !.
inline_codable('term_basic:C'(_, _, _)) :- !.
inline_codable(X) :- name_of_builtin(X, _, _), !.
inline_codable(X) :- name_of_builtin(X, _, _, _), !.
inline_codable(X) :- name_of_builtin(X, _, _, _, _), !.

% The following predicates are trusted to preserve all arguments.
name_of_builtin('term_typing:atom'(X), 20, X).
name_of_builtin('term_typing:atomic'(X), 21, X).
name_of_builtin('term_typing:float'(X), 22, X).
name_of_builtin('term_typing:integer'(X), 23, X).
name_of_builtin('term_typing:nonvar'(X), 24, X).
name_of_builtin('term_typing:number'(X), 25, X).
name_of_builtin('term_typing:var'(X), 26, X).
name_of_builtin('basiccontrol:IF BUILTIN'(X), 39, X).

name_of_builtin('attributes:detach_attribute'(X), 48, X).	% DMCAI -- ATTRVARS

name_of_builtin('term_compare:=='(X,Y), 27, X, Y).
name_of_builtin('term_compare:\\=='(X,Y), 28, X, Y).
name_of_builtin('term_compare:@<'(X,Y), 29, X, Y).
name_of_builtin('term_compare:@>='(X,Y), 30, X, Y).
name_of_builtin('term_compare:@>'(X,Y), 31, X, Y).
name_of_builtin('term_compare:@=<'(X,Y), 32, X, Y).
name_of_builtin('arithmetic:=:='(X,Y), 33, X, Y).
name_of_builtin('arithmetic:=\\='(X,Y), 34, X, Y).
name_of_builtin('arithmetic:<'(X,Y), 35, X, Y).
name_of_builtin('arithmetic:>='(X,Y), 36, X, Y).
name_of_builtin('arithmetic:>'(X,Y), 37, X, Y).
name_of_builtin('arithmetic:=<'(X,Y), 38, X, Y).
name_of_builtin('term_basic:=..'(X,Y), 40, X, Y).	

% Note: hole in numbers because builtins 41 to 43 appear below

name_of_builtin('term_typing:type'(X,Y), 44, X, Y).            % DMCAI - ATTRVARS
name_of_builtin('attributes:get_attribute'(X,Y), 45, X, Y).    % DMCAI - ATTRVARS
name_of_builtin('attributes:attach_attribute'(X,Y), 46, X, Y). % DMCAI - ATTRVARS
name_of_builtin('attributes:update_attribute'(X,Y), 47, X, Y). % DMCAI - ATTRVARS

% These builtins appear here because they have arity 5 - we avoid
% compiler warnings about discontiguous clauses.

name_of_builtin('term_basic:arg'(X,Y,Value), 41, X, Y, Value).
name_of_builtin('term_compare:compare'(Value,X,Y), 42, X, Y, Value).
name_of_builtin('term_basic:functor'(X,Y,Z), 43, X, Y, Z).



builtin_heap_usage(40, H) :- !, H=512.		% max arity is 255
builtin_heap_usage(43, H) :- !, H=256.		% max arity is 255
builtin_heap_usage(_, 0).

eval_builtin('arithmetic:=:='(_,_)).
eval_builtin('arithmetic:=\\='(_,_)).
eval_builtin('arithmetic:<'(_,_)).
eval_builtin('arithmetic:>='(_,_)).
eval_builtin('arithmetic:>'(_,_)).
eval_builtin('arithmetic:=<'(_,_)).


name_of_function(-(X), 0, X).
name_of_function(+(X), 1, X).
name_of_function(--(X), 2, X).      %% shorthand for 'SUB1 FUNCTION'
name_of_function(++(X), 3, X).      %% shorthand for 'ADD1 FUNCTION'
name_of_function(integer(X), 4, X).
name_of_function(truncate(X), 4, X). /* alias of integer/1 (ISO)*/
name_of_function(float(X), 5, X).
name_of_function(\(X), 6, X).
name_of_function(abs(X), 19, X).
name_of_function(sign(X), 49, X).
name_of_function(float_integer_part(X),50, X).
name_of_function(float_fractional_part(X),51,X).
name_of_function(floor(X),52,X).
name_of_function(round(X),53,X).
name_of_function(ceiling(X),54,X).
name_of_function(X+Y, 7, X, Y).
name_of_function(X-Y, 8, X, Y).
name_of_function(X*Y, 9, X, Y).
name_of_function(X/Y, 10, X, Y).
name_of_function(X//Y, 11, X, Y).
name_of_function(X rem Y, 12, X, Y).
name_of_function(X#Y, 13, X, Y).
name_of_function(X/\Y, 14, X, Y).
name_of_function(X\/Y, 15, X, Y).
name_of_function(X<<Y, 16, X, Y).
name_of_function(X>>Y, 17, X, Y).
name_of_function(X mod Y, 18, X, Y).
name_of_function(X**Y, 55, X, Y).
