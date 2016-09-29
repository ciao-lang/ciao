%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% RUN-TIME SUPPORT FOR VIRTUAL METHOD CALLING
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------
%% WARNING:
%%   Those predicates are used by O'Ciao class expansion.
%%   Dont call directly !!!!
%%------------------------------------------------------------------------

:- module(virtual,[mod_exp/5],[]).

%%------------------------------------------------------------------------

:- multifile 'class$virtual'/6.
:- multifile 'id$fromclass'/2.

:- data 'id$fromclass'/2.

%%------------------------------------------------------------------------

mod_exp(goal,Goal,FromModule,ID,Exp) :-
	'id$fromclass'(ID,Class),
	'class$virtual'(Class,FromModule,_,Goal,ID,Exp).

mod_exp(fact,Goal,FromModule,ID,Exp) :-
	'id$fromclass'(ID,Class),
	'class$virtual'(Class,FromModule,attribute,Goal,ID,Exp).

mod_exp(spec,F/A,FromModule,ID,ExpF/ExpA) :-
	'id$fromclass'(ID,Class),
	functor(Check,F,A),
	'class$virtual'(Class,FromModule,_,Check,ID,Exp),
	functor(Exp,ExpF,ExpA).

mod_exp(spec,Spec,FromModule,_,_) :-
	throw(error(existence_error(virtual_method,Spec),FromModule)),
	!,
	fail.

mod_exp(_,Goal,FromModule,_,_) :-
	functor(Goal,F,A),
	throw(error(existence_error(virtual_method,F/A),FromModule)),
	!,
	fail.
