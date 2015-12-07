:- module(_,_,[assertions,regtypes,nativeprops,fsyntax,doccomments,expander]).

:- doc(title, "").
:- doc(author, "").

:- use_package(.(foomodes)).

%! rb_lookup(Key, Value, T): 
%
%  Backtrack through all elements with key `Key` in the Red-Black tree
%  `T`, returning for each the value `Value`.
%
%  rb_lookup(+, -, +): produces `Value`.
%  rb_lookup(+, +, -): produces `T`.

rb_lookup(_, _, _).

%! len_mt(+list,-int): Computes the length of the first argument
%   (*showing*: modes and types).

len_mt(_,_).

:- pred age( Name :: atom,  Age :: integer) + is_det. % OK
:- pred age(+Name :: atom, -Age :: integer) + is_det. % OK
:- pred age(+atom, -integer) + is_det. % OK
:- pred age(atom, integer) + is_det. % OK
%% :- pred age(+, -) :: atom * integer + is_det. % OK
%% :- pred age(?, ?) :: atom * integer + is_det. % OK

age(_,_).

:- pred p(+list(list(list))). 

p(_).

:- pred age2(+atom, +integer) is is_det # "This call is deterministic".
age2(_,_).

% TODO: Recognize this with a final period.

%! age3(+list(atom), +integer) is is_det.
%    I promise you that this call is determinisic. Really, no joking.
%
%  Just look at the code.
age3(_,_).

%! delete_non_ground(L1,_E,L2) => (list(L1), list(L2)).
% 
%  # "L2 is L1 without the ocurrences of E. E can be a nonground term so
%    that all the elements in L1 it unifies with will be deleted."

:- test nonsingle(A) : ( A = [a] ) + fails # "nonsingle fails.".

:- test nonsingle(A) : ( A = [a, b] ) # "nonsingle succeeds.".

:- pred nonsingle(X) # "@var{X} is not a singleton.".

nonsingle([_]) :- !, fail.
nonsingle(_).

:- true pred nonsingle/1 # "dummy".
:- false pred nonsingle/1 # "dummy".
:- check pred nonsingle/1 # "dummy".
:- checked pred nonsingle/1 # "dummy".
:- trust pred nonsingle/1 # "dummy".

% (doccomments with tabs)

%!	aggregate(+Template, -Goal, -Result):
%
%         Aggregate bindings in `Goal` according to `Template`.  The
%         `aggregate/3` version performs `bagof/3` on `Goal`.
aggregate(_, _, _).

%!	agg(+Template, Goal, -Result) is nondet.
%
%       Aggregate bindings in Goal according to Template.  The
%       aggregate/3 version performs bagof/3 on Goal.
agg(_,_,_).

