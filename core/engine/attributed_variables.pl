% ===========================================================================
% :- doc(title, "Internal hooks for attributed variables.").
% 
% Those internal hooks are called from the engine, and provide an
% interface with user-level hooks for attributed variables.
% ===========================================================================

%:- use_module(library(term_basic), [functor/3]).

:- multifile verify_attribute/2.
:- multifile combine_attributes/2.
:- multifile '$check_attr'/3.
:- multifile '$combine_attr'/4.

% :- use_module(library(attr/attr_rt), ['$verify_attributes_loop'/2]).
:- multifile '$verify_attributes_loop'/2. % (avoids a dependency to attr_rt)
:- use_module(engine(attributes)).

% called from the emulator
% if there is just one single pending unification
%
uvc(A, B) :- 
%D	display(['DEBUG: uvc: vars: ', A, B]),nl,
	get_attribute(A, AT),
	%%% Here we mix old and new version
        ( AT = att(_, _, As) ->
            %%% NEW VERSION
	    detach_attribute(A),
	    A = B,
	    '$verify_attributes_loop'(As, B)
	; %%% OLD VERSION
	  verify_attribute(AT, B)
	).

ucc(A, B) :-
%D	display(['DEBUG: ucc: vars: ', A, B]),nl,
	get_attribute(A, AT),
	( AT = att(_, _, As) ->
	    %%% NEW VERSION
	    detach_attribute(A),
	    A = B,
	    '$verify_attributes_loop'(As, B)
	; %%% OLD VERSION
	  get_attribute(B, BT),
	  combine_attributes(AT, BT)
	).

% there are more pending unifications (relatively rare)
%
pending_unifications([]).
pending_unifications([[V1|V2]|Cs]) :-
	pending_unification(V1, V2),
	pending_unifications(Cs).

pending_unification(A, B) :-
	( get_attribute(A, _) ->
	    ( get_attribute(B, _) ->
	        ucc(A, B)
	    ; uvc(A, B)
	    )
	; ( get_attribute(B, _) ->
	      uvc(B, A)
	  ; A = B % reduced to syntactic unification
	  )
	).

combine_global_attributes(simple_attr(LA), simple_attr(LB), Ret) :- !,
	int_combine_attr(LA, LB, Ret).

% ---------------------------------------------------------------------------
%%% --- From mattr_global.pl

%% Insert in order. If it exists, replace it.

% the same => rewrite
internal_insert_attr(A, [P|R], [A|R]) :-
	functor(A, N, _),
	functor(P, N, _), 
	!.
% less => insert
internal_insert_attr(A, [P|R], [A,P|R]) :-
	A @< P, 
	!.
% keep it
internal_insert_attr(A, [P|R], [P|LR]) :-	
	internal_insert_attr(A, R, LR), 
	!.

% last or empty => add
internal_insert_attr(A, [] ,[A]).

int_attach_attr(X, A) :-
	( attributes:get_attribute(X, simple_attr(Attr)) ->    
            internal_insert_attr(A, Attr, New_AttList),
	    attributes:update_attribute(X, simple_attr(New_AttList))
	; attributes:attach_attribute(X, simple_attr([A]))
	).

% --- this attributes should cobine if there are same kind-of
% attributes in the variable.
int_attach_attr_list([], _).
int_attach_attr_list([A|As], Ret) :-
	( has_same_attr(A, Ret, SameAt) ->
	    local_combine_attr(A, SameAt, Ret)
	; int_attach_attr(Ret, A)
	),
	int_attach_attr_list(As, Ret).

has_same_attr(At, Var, SameAt) :-
	functor(At, F, A),
	attributes:get_attribute(Var, simple_attr(Attr)),
	has_same_attr__(Attr, F, A, RestOfAttr, SameAt),
	update_attribute(Var, simple_attr(RestOfAttr)).

has_same_attr__([At|Ats], F, A, Rest, SameAt) :-
	functor(At, F1, A1),
	( F1 = F -> 
	     A      = A1,
	     SameAt = At,
	     Rest   = Ats
	; F1 @< F,
	  Rest = [At|R],
	  has_same_attr__(Ats, F, A, R, SameAt)
	).

int_combine_attr([], B, Ret) :- !, int_attach_attr_list(B, Ret).
int_combine_attr(B, [], Ret) :- !, int_attach_attr_list(B, Ret).
int_combine_attr([A|AR], [B|BR], Ret) :-
	!,
	functor(A, FA, _),
	functor(B, FB, _),
	( FA @< FB ->
	    % A belongs to a module before than B => Keep A
	    int_attach_attr(Ret, A),
	    int_combine_attr(AR, [B|BR], Ret)
	; ( FA == FB ->
	      % belongs to the same module
	      local_combine_attr(A, B, Ret),
	      ( nonvar(Ret) ->
	          merge_attr(AR, BR, Rest),
		  int_verify_attr(Rest , Ret)
	      ; int_combine_attr(AR, BR, Ret)
	      )
	  ; % B belongs to a module before than A => Keep B
	    int_attach_attr(Ret, B),
	    int_combine_attr([A|AR], BR, Ret)
	  )
	).

merge_attr([], B, B) :- !.
merge_attr(B, [], B) :- !.
merge_attr([A|AR], [B|BR], [Which|RR]) :-
	functor(A, FA, _),
	functor(B, FB, _),
	( FA @< FB ->
	    Which = A,
	    merge_attr(AR, [B|BR], RR)
	; Which = B,
	  merge_attr([A|AR], BR, RR)
	).
	
local_combine_attr(A, B, Sol) :-
	functor(A, Key, _),
	arg(1, A, XA),
	arg(1, B, XB),
	'$combine_attr'(Key, XA, XB, Sol), 
	!.

int_verify_attr([], _X).
int_verify_attr([At|Ats], Value) :- 
	functor(At, Key  , _),
	arg(1, At, Attr),
	'$check_attr'(Key, Attr, Value),
	!,
	int_verify_attr(Ats, Value).

% ---------------------------------------------------------------------------
%
% TODO: Document this code; keep if useful, remove otherwise
%
% int_combine_attr([], B, _, B, _) :- !.
% int_combine_attr(B, [], _, B, _) :- !.
% int_combine_attr([A|AR], [B|BR], Bef, BefX, Ret) :-
% 	!,
% 	functor(A, FA, _),
% 	functor(B, FB, _),
% 	(
% 	  FA @< FB 
% 	->
% 	  % A belongs to a module before than B => Keep A
% 	  BefX = [A|Next],
% 	  int_combine_attr(AR, [B|BR], Bef, Next, Ret)
% 	;
% 	  (
% 	      FA == FB
% 	  ->
% 	      % belongs to the same module
% 	      local_combine_attr(A, B, SOL),
% 	      (
% 		  nonvar(SOL)
% 	      ->
% 		  BefX = [], Ret = SOL,
% 	          int_verify_attr(Bef, SOL),
% 		  int_verify_attr(AR , SOL),
% 		  int_verify_attr(BR , SOL)
% 	      ;
% 		  (
% 		      get_attribute(SOL, simple_attr([SolAttr]))
% 		  -> 
% 		      BefX = [SolAttr|Next],
% 		      int_combine_attr(AR, BR, Bef, Next, Ret)
% 		  ;
% 		      int_combine_attr(AR, BR, Bef, BefX, Ret)
% 		 )
% 	     )
% 	  ;
% 	      BefX = [B|Next],
% 	      int_combine_attr([A|AR], BR, Bef, Next, Ret)
% 	 )
% 	).

