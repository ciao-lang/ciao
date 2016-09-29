%%------------------------------------------------------------------------
%%
%% O'CIAO: Object Oriented Programming in CIAO/Prolog
%%
%% SOURCE-TO-SOURCE EXPANSION FOR INTERFACE DECLARATION
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : July 1999
%%
%%------------------------------------------------------------------------

:- module(interface_tr,
	[
	    interface_sentence_trans/3,
	    interface_clause_trans/3
	]).

:- use_module(library(compiler/c_itf_internal)).
:- use_module(library(lists), [append/3]).
:- use_module(library(class/class_error_reporting)).
:- use_module(library(class/class_itf)).

%%------------------------------------------------------------------------

:- data is_public/3.
:- data is_attribute/3.
:- data implements/2.

%%------------------------------------------------------------------------
%%
%% INITIALIZATION
%%
%%------------------------------------------------------------------------

interface_sentence_trans(0,_,Module) :-
	cleanup_class_info(Module),
	start_of_messages(Module,['Checking interface file of ',Module]),
	retractall_fact(implements(Module,_)),
	retractall_fact(is_public(Module,_,_)),
	retractall_fact(is_attribute(Module,_,_)),
	!,
	fail.

interface_sentence_trans(end_of_file,Exp,Module) :-
	end_of_messages(Module),
	!,
	findall((:- public(F/A)),is_public(Module,F,A),Pub),
	findall((:- method(F/A)),
	        ( is_public(Module,F,A),
		  \+ is_attribute(Module,F,A)
		),
		Method
	),
	findall((:- attribute(F/A)),
	        (
		    is_public(Module,F,A),
		    is_attribute(Module,F,A)
		),
		Attr
	),
	append(Pub,Method,Aux1),
	append(Aux1,Attr,Aux2),
	append(Aux2,[end_of_file],Exp),
	true.

%%------------------------------------------------------------------------
%%
%% NORMALIZE MULTIPLE-SPEC DECLARATIONS
%%
%%------------------------------------------------------------------------

interface_sentence_trans((:- IsDecl),Exp,Module) :-
	functor(IsDecl,Decl,1),
	( Decl = public ;
	  Decl = export
	),
	arg(1,IsDecl,Arg),
	functor(Arg,',',2),
	!,
	sequence_to_list(Arg,List),
	functor(NewDecl,Decl,1),
	arg(1,NewDecl,List),
	interface_sentence_trans((:- NewDecl),Exp,Module).

interface_sentence_trans((:- export(X)),Exp,Mod) :-
	!,
	interface_sentence_trans((:- public(X)),Exp,Mod).

interface_sentence_trans((:- public([])),[],_) :-
	!.

interface_sentence_trans((:- public([Spec|Nsp])),Exp,Mod) :-
	!,
	interface_sentence_trans((:- public(Spec)),OneDeclExp,Mod),
	interface_sentence_trans((:- public(Nsp)),NExp,Mod),
	append(OneDeclExp,NExp,Exp).

%%------------------------------------------------------------------------
%%
%% INTERFACE INHERITANCE
%%
%%------------------------------------------------------------------------

interface_sentence_trans((:- inherit_class(Source)),Exp,Mod) :-
	!,
	message(Mod,note,
	  ['inherit_class/1 declaration is used ',
	   'as an alias for implements/1']),
	interface_sentence_trans((:- implements(Source)),Exp,Mod).

interface_sentence_trans((:- implements(Source)),
	[
	    (:- reexport(Source,[])),
	    (:- implements(Itf))
	],Mod) :-
	class_from_base(Source,Itf),
	\+ implements(Mod,Itf),
	assertz_fact(implements(Mod,Itf)),
	!,
	true.

interface_sentence_trans((:- implements(_)),[],_Mod) :-
	!,
	true.

%%------------------------------------------------------------------------
%%
%% PUBLIC DECLARATIONS
%%
%%------------------------------------------------------------------------

interface_sentence_trans((:- public(F/A)),[],Mod) :-
	atom(F),
	integer(A),
	A >= 0,
	!,
	( is_public(Mod,F,A) ->
	  true
	;
	  assertz_fact(is_public(Mod,F,A))
	),
	true.

interface_sentence_trans((:- public(X)),[],Mod) :-
	message(Mod,error,
	  ['invalid public declaration: ',public(X)]),
	!.

%%------------------------------------------------------------------------
%%
%% ATTRIBUTE DECLARATIONS
%%
%%------------------------------------------------------------------------

interface_sentence_trans((:- data(F/A)),[],Mod) :-
	atom(F),
	integer(A),
	A >= 0,
	!,
	( is_attribute(Mod,F,A) ->
	  true
	;
	  assertz_fact(is_attribute(Mod,F,A))
	),
	( is_public(Mod,F,A) ->
	  true
	;
	  assertz_fact(is_public(Mod,F,A))
	),
	true.

interface_sentence_trans((:- data(X)),[],Mod) :-
	message(Mod,error,
	  ['invalid attribute declaration: ',data(X)]),
	!.

%%------------------------------------------------------------------------
%%
%% AVOID CLAUSES AND OTHER DECLARATIONS
%%
%%------------------------------------------------------------------------

interface_sentence_trans((:- Decl),[],Mod) :-
	message(Mod,error,['declaration not allowed: ',Decl]),
	!.

interface_sentence_trans((Head :- _),[],Mod) :-
	functor(Head,F,A),
	!,
	message(Mod,error,['clause of ',F,'/',A,' not allowed']).

interface_sentence_trans((Head),[],Mod) :-
	functor(Head,F,A),
	!,
	message(Mod,error,['clause of ',F,'/',A,' not allowed']).

%%------------------------------------------------------------------------
%%
%% SECOND PASS
%%
%%------------------------------------------------------------------------

interface_clause_trans(clause(0,0),_,Module) :-
	generate_oop_info(Module),
	defines_module(Base,Module),
	start_of_messages(Module,['Checking interface compatibility of ',
	  Base]),
	check_interface_compatibility(Module),
	end_of_messages(Module),
	!,
	fail.

%%------------------------------------------------------------------------
%%
%% CHECK COMPATIBILITY OF IMPLEMENTED INTERFACES
%%
%%------------------------------------------------------------------------

:- data itf_check/4.  % auxiliary

check_interface_compatibility(_Module) :-
	retractall_fact(itf_check(_,_,_,_)),
	fail.

check_interface_compatibility(Module) :-
	impl_interface(Module,Itf),
	public_pred(Itf,Kind,F,A),
	( itf_check(OtherItf,F,A,Kind2) ->
	  ( \+ (Kind = Kind2) -> 
	     ( message(Module,error,
	       ['predicate ',F,'/',A,' is required both as ',
		Kind,' (at ',Itf,' interface) and ',Kind2,' (at ',
	        OtherItf,' interface)']),
	       fail
	     )
	  )
	;
	  assertz_fact(itf_check(Itf,F,A,Kind))
	),
	fail.

check_interface_compatibility(_).

%%------------------------------------------------------------------------
%%
%% MACROS
%%
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% Convert sequence to list
%%------------------------------------------------------------------------

sequence_to_list(Sequence,List) :-
	functor(Sequence,',',2),
	!,
	arg(1,Sequence,Sq1),
	arg(2,Sequence,Sq2),
	sequence_to_list(Sq1,L1),
	sequence_to_list(Sq2,L2),
	append(L1,L2,List).

sequence_to_list(SomeThing,[SomeThing]).
