%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% SOURCE-TO-SOURCE EXPANSION FOR CLASS DECLARATION
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------

:- module(class_tr,
	[
	    class_sentence_translation/3,
	    class_clause_translation/3
	]).

%%------------------------------------------------------------------------

:- use_module(library(compiler/c_itf_internal)).
:- use_module(library(lists), [append/3]).
:- use_module(engine(internals), [module_concat/3]).
:- use_module(library(class/class_itf)).
:- use_module(library(terms), [atom_concat/2]).

%%------------------------------------------------------------------------
%% All those facts are indexed by module name(=class name)...

:- data super/2.               % CLASS inherits from SUPER.
:- data inherit_class/2.       % CLASS inherits from SOURCE FileName 
:- data implements/2.          % CLASS implements INTERFACE

:- data initial_state/3.       % CLASS declares that FACT(functor) 
                               % with ARGS must be present 
                               % at instance creation.

:- data is_inheritable/3.      % CLASS declared F/A as inheritable.
:- data is_public/3.           % CLASS declared F/A as public.
:- data is_multifile/3.        % CLASS declared F/A as multifile.
:- data is_state/3.            % CLASS declared F/A as data or dynamic.
:- data is_method/3.           % CLASS declared F/A as static (method).
:- data is_virtual/3.          % CLASS declared F/A as a virtual method.
:- data is_concurrent/3.       % CLASS declared F/A as a concurrent attribute.
%:- data is_persistent/3.       % CLASS declared F/A as a persistent attribute.
:- data is_metapredicate/2.    % CLASS declared SPEC as a 
                               % meta_predicate specification.

:- data runtime_info/1.        % MODULE needs runtime info to be generated.

%%------------------------------------------------------------------------

:- include(library(class/class_tr_aux)). % for "second pass" expansion.

%%------------------------------------------------------------------------

:- data debug_first_pass/0.
:- data debug_second_pass/0.
:- data debug_expansion_info/0.

% debug_first_pass.
% debug_second_pass.
% debug_expansion_info.

class_sentence_translation(X,Y,M) :-
	catch(
		 class_sentence_trans(X,Y,M),
		 Excpt,
		 (message(M,error,['(Exiting due to exception: ',Excpt,')']),
		 end_of_messages(M),
		 throw(Excpt))
	).

class_clause_translation(X,Y,M) :-
	catch(
		 class_clause_trans(X,Y,M),
		 Excpt,
		 (message(M,error,['(Exiting due to exception: ',Excpt,')']),
		 end_of_messages(M),
		 throw(Excpt))
	).

%%------------------------------------------------------------------------
%%
%% INITIALIZATION
%%
%%------------------------------------------------------------------------

class_sentence_trans(0,_,Module) :-
	cleanup_class_info(Module),
	retractall_fact(super(Module,_)),
	retractall_fact(inherit_class(Module,_)),
	retractall_fact(implements(Module,_)),
	retractall_fact(initial_state(Module,_,_)),
	retractall_fact(is_inheritable(Module,_,_)),
	retractall_fact(is_public(Module,_,_)),
	retractall_fact(is_method(Module,_,_)),
	retractall_fact(is_virtual(Module,_,_)),
	retractall_fact(is_metapredicate(Module,_)),
	retractall_fact(is_state(Module,_,_)),
%	retractall_fact(is_persistent(Module,_,_)),
	retractall_fact(is_multifile(Module,_,_)),
	retractall_fact(is_concurrent(Module,_,_)),
	retractall_fact(runtime_info(Module)),
	start_of_messages(Module,['Declaring class ',Module]),
	!,
	fail.

%%------------------------------------------------------------------------
%%
%% END OF FILE ADD-ON
%%
%% Notice: all called goals are assumed not to fail.
%%         '$end$$of$$expansion$' clause is used to detect ending of
%%         second-pass expansion.
%%------------------------------------------------------------------------

class_sentence_trans(end_of_file,FinalClauses,Module) :-
	!,
	generate_fixed_clauses(Module,FixedClauses),
	generate_initial_state_clauses(Module,InitialStateClauses),
	generate_metapredicate_decls(Module,MetaPredDecls),
	generate_interface_info(Module,ItfDecls),
	generate_redefining_clauses(Module,RedefClauses),
	generate_reexport_clauses(Module,ReexportClauses),
	generate_implements_decls(Module,ImplementsDecls),
%	generate_persistency_decls(Module,PersDecls),
	end_of_messages(Module),
	%
	append(FixedClauses,InitialStateClauses,Aux1),
	append(Aux1,MetaPredDecls,Aux2),
	append(Aux2,ItfDecls,Aux3),
	append(Aux3,ReexportClauses,Aux4),
	append(Aux4,RedefClauses,Aux5),
	append(Aux5,ImplementsDecls,Aux6),
%	append(Aux6,PersDecls,Aux7),
	append(Aux6,['$end$$of$$expansion$',end_of_file],FinalClauses),
	( debug_first_pass ->
	  (
	      nl,display('End of expansion clauses: '),nl,
	      inform_user(FinalClauses),nl
	  ) ; true
	),
	true.

%%------------------------------------------------------------------------
%%
%% DECLARATIONS
%%
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% NORMALIZE MULTIPLE-SPEC DECLARATIONS
%%------------------------------------------------------------------------

class_sentence_trans((:- IsDecl),Exp,Module) :-
	functor(IsDecl,Decl,1),
	( Decl = public ;
	  Decl = export ;
	  Decl = inheritable ;
	  Decl = data ;
	  Decl = dynamic ;
	  Decl = meta_predicate ;
	  Decl = multifile ;
	  Decl = concurrent ;
	  Decl = discontiguous ;
	  Decl = virtual 
%	  Decl = persistent
	),
	arg(1,IsDecl,Arg),
	functor(Arg,',',2),
	!,
	sequence_to_list(Arg,List),
	functor(NewDecl,Decl,1),
	arg(1,NewDecl,List),
	class_sentence_trans((:- NewDecl),Exp,Module).

class_sentence_trans((:- export(X)),Exp,Mod) :-
	!,
	class_sentence_trans((:- public(X)),Exp,Mod).

class_sentence_trans((:- public([])),[],_) :-
	!.

class_sentence_trans((:- public([Spec|Nsp])),Exp,Mod) :-
	!,
	class_sentence_trans((:- public(Spec)),OneDeclExp,Mod),
	class_sentence_trans((:- public(Nsp)),NExp,Mod),
	append(OneDeclExp,NExp,Exp).

class_sentence_trans((:- inheritable([])),[],_) :-
	!.

class_sentence_trans((:- inheritable([Spec|Nsp])),Exp,Mod) :-
	!,
	class_sentence_trans((:- inheritable(Spec)),OneDeclExp,Mod),
	class_sentence_trans((:- inheritable(Nsp)),NExp,Mod),
	append(OneDeclExp,NExp,Exp).

class_sentence_trans((:- virtual([])),[],_) :-
	!.

class_sentence_trans((:- virtual([Spec|Nsp])),Exp,Mod) :-
	!,
	class_sentence_trans((:- virtual(Spec)),OneDeclExp,Mod),
	class_sentence_trans((:- virtual(Nsp)),NExp,Mod),
	append(OneDeclExp,NExp,Exp).

class_sentence_trans((:- data([])),[],_) :-
	!.

class_sentence_trans((:- data([Spec|Nsp])),Exp,Mod) :-
	!,
	class_sentence_trans((:- data(Spec)),OneDeclExp,Mod),
	class_sentence_trans((:- data(Nsp)),NExp,Mod),
	append(OneDeclExp,NExp,Exp).

class_sentence_trans((:- dynamic([])),[],_) :-
	!.

class_sentence_trans((:- dynamic([Spec|Nsp])),Exp,Mod) :-
	!,
	class_sentence_trans((:- public(Spec)),OneDeclExp,Mod),
	class_sentence_trans((:- public(Nsp)),NExp,Mod),
	append(OneDeclExp,NExp,Exp).

class_sentence_trans((:- concurrent([])),[],_) :-
	!.

class_sentence_trans((:- concurrent([Spec|Nsp])),Exp,Mod) :-
	!,
	class_sentence_trans((:- concurrent(Spec)),OneDeclExp,Mod),
	class_sentence_trans((:- concurrent(Nsp)),NExp,Mod),
	append(OneDeclExp,NExp,Exp).

class_sentence_trans((:- multifile([])),[],_Mod) :-
	!.

class_sentence_trans((:- multifile([Spec|Nsp])),Exp,Mod) :-
	!,
	class_sentence_trans((:- multifile(Spec)),OneDeclExp,Mod),
	class_sentence_trans((:- multifile(Nsp)),NExp,Mod),
	append(OneDeclExp,NExp,Exp).

class_sentence_trans((:- meta_predicate([])),[],_) :-
	!.

class_sentence_trans((:- meta_predicate([Spec|Nsp])),Exp,Mod) :-
	!,
	class_sentence_trans((:- meta_predicate(Spec)),OneDeclExp,Mod),
	class_sentence_trans((:- meta_predicate(Nsp)),NExp,Mod),
	append(OneDeclExp,NExp,Exp).

class_sentence_trans((:- discontiguous([])),[],_) :-
	!.

class_sentence_trans((:- discontiguous([Spec|Nsp])),Exp,Mod) :-
	!,
	class_sentence_trans((:- discontiguous(Spec)),OneDeclExp,Mod),
	class_sentence_trans((:- discontiguous(Nsp)),NExp,Mod),
	append(OneDeclExp,NExp,Exp).

%class_sentence_trans((:- persistent([])),[],_) :-
%	!.

%class_sentence_trans((:- persistent([Spec|Nsp])),Exp,Mod) :-
%	!,
%	class_sentence_trans((:- persistent(Spec)),OneDeclExp,Mod),
%	class_sentence_trans((:- persistent(Nsp)),NExp,Mod),
%	append(OneDeclExp,NExp,Exp).

%%------------------------------------------------------------------------
%% INTERFACE IMPLEMENTATION
%%------------------------------------------------------------------------

class_sentence_trans((:- implements(Source)),
	[
	    (:- reexport(Source,[]))
	],Mod) :-
	class_from_base(Source,Itf),
	\+ implements(Mod,Itf),
	assertz_fact(implements(Mod,Itf)),
	!,
	true.

class_sentence_trans((:- implements(_)),[],_Mod) :-
	!,
	true.

%%------------------------------------------------------------------------
%% AVOID RESERVED DECLARATIONS
%%------------------------------------------------------------------------

class_sentence_trans((:- super(_)),[],_) :-
	!,
	true.

class_sentence_trans((:- attribute(_)),[],_) :-
	!,
	true.

class_sentence_trans((:- method(_)),[],_) :-
	!,
	true.

%%------------------------------------------------------------------------
%% INHERITANCE DECLARATION
%%------------------------------------------------------------------------

class_sentence_trans((:- inherit_class(_)),[],Module) :-
	super(Module,_),
	!,
	message(Module,error,'multiple inheritance not allowed').

class_sentence_trans((:- inherit_class(SuperSource)),
	[
	    (:- inherit_class(SuperSource)),
	    (:- super(Super))
	],Module) :-
	class_from_base(SuperSource,Super),
	asserta_fact(super(Module,Super)),
	asserta_fact(inherit_class(Module,SuperSource)),
	!.

class_sentence_trans((:- inherit_class(_)),[],Module) :-
	message(Module,error,'invalid inheritance declaration'),
	!.

%%------------------------------------------------------------------------
%% KEEP TRACKING OF METAPREDICATE DECLARATIONS
%%------------------------------------------------------------------------
	
class_sentence_trans((:- meta_predicate(Spec)),[],Mod) :-
	Spec =.. [_|SpecArgs],
	functor(Spec,F,A),
	\+ is_multifile(Mod,F,A),
	has_addmodule(SpecArgs),
	!,
	message(Mod,error,[
		'sorry, addmodule meta-arg is not allowed at ',
		F,'/',A]).

class_sentence_trans((:- meta_predicate(Spec)),[],Mod) :-
	functor(Spec,F,A),
	\+ is_multifile(Mod,F,A),
	assertz_fact(is_metapredicate(Mod,Spec)),
	!,
	true.

%%------------------------------------------------------------------------
%% KEEP TRACKING OF DATA DECLARATIONS
%%------------------------------------------------------------------------

% 1st case: normalize attribute declarations 

class_sentence_trans((:- dynamic(F/A)),Exp,Mod) :-
	!,
	class_sentence_trans((:- data(F/A)),Exp,Mod).

% 2nd case: invalid spec.

class_sentence_trans((:- data(F/A)),[],Mod) :-
	( (\+ atom(F)) ; (\+ integer(A)) ),
	!,
	message(Mod,error,
	['invalid attribute declaration for ',F,'/',A]).

% 3rd case: already declared as method.

class_sentence_trans((:- data(F/A)),[],Mod) :-
	is_method(Mod,F,A),
	!,
	message(Mod,error,['pretended attribute ',F,'/',A,
	 ' was assumed to be a method']),
	true.

% 4nd case: already declared.

class_sentence_trans((:- data(F/A)),[],Mod) :-
	is_state(Mod,F,A),
	!,
	true.

% 5rd case: multifile declaration already found.

class_sentence_trans((:- data(F/A)),[],Mod) :-
	is_multifile(Mod,F,A),
	!,
%PBC	fail.
	true.

% 6th case: predicate is reserved.

class_sentence_trans((:- data(destructor/0)),[],Mod) :-
	!,
	message(Mod,error,
	 ['destructor/0 is not allowed to be an attribute']).

class_sentence_trans((:- data(Module/A)),[],Module) :-
	!,
	message(Module,error,
	 [Module,'/',A,' is not allowed to be an attribute']).

% 7th case: valid attribute declaration.

class_sentence_trans((:- data(F/A)),[],Mod) :-
	asserta_fact(is_state(Mod,F,A)),
	!,
%PBC	fail.
	true.

%%------------------------------------------------------------------------
%% KEEP TRACKING OF MULTIFILE DECLARATIONS
%%------------------------------------------------------------------------

% 0 case : F/A is a reserved predicate.

class_sentence_trans((:- multifile(destructor/0)),[],Mod) :-
	!,
	message(Mod,error,
	 ['invalid multifile: destructor/0 is a reserved predicate']).

class_sentence_trans((:- multifile(Module/A)),[],Module) :-
	!,
	message(Module,error,
	 ['invalid multifile: ',Module,'/',A,' is a reserved predicate']).

% 1st case : F/A was declared dynamic before multifile

class_sentence_trans((:- multifile(F/A)),[],Mod) :-
	is_state(Mod,F,A),
	!,
	retract_fact(is_state(Mod,F,A)),
	asserta_fact(is_multifile(Mod,F,A)),
%PBC	fail.
	true.

% 2nd case : F/A was declared method before multifile

class_sentence_trans((:- multifile(F/A)),[],Mod) :-
	is_method(Mod,F,A),
	!,
	message(Mod,error,
	 ['multifile declaration of ',F,'/',A,
          ' ignored: it was assumed to be a method']),
	true.

% 3rd case : valid multifile declaration.

class_sentence_trans((:- multifile(F/A)),[],Mod) :-
	atom(F),
	integer(A),
	!,
	asserta_fact(is_multifile(Mod,F,A)),
%PBC	fail.
	true.

% 4th case : invalid multifile declaration.

class_sentence_trans((:- multifile(S)),[],Mod) :-
	!,
	message(Mod,error,
	 ['invalid multifile declaration: ',multifile(S)]).

%%------------------------------------------------------------------------
%% KEEP TRACKING OF DISCONTIGUOUS DECLARATIONS
%%------------------------------------------------------------------------

class_sentence_trans((:- discontiguous(F/A)),[],Mod) :-
	is_state(Mod,F,A),
	!.

class_sentence_trans((:- discontiguous(F/A)),[],Mod) :-
	is_multifile(Mod,F,A),
	!,
%PBC	fail.
	true.

class_sentence_trans((:-discontiguous(F/A)),[(:-discontiguous(NF/NA))],Mod) :-
	is_method(Mod,F,A),
	!,
	atom_concat('obj$',F,NF),
	NA is A+1.

class_sentence_trans((:- discontiguous(F/A)),
	[(:- discontiguous(F/NA)),(:- discontiguous(NF/A))],_Mod ) :-
	!,
	atom_concat('obj$',F,NF),
	NA is A+1.

%%------------------------------------------------------------------------
%% KEEP TRACKING OF CONCURRENT DECLARATIONS
%%------------------------------------------------------------------------

class_sentence_trans((:- concurrent(F/A)),[],Mod) :-
	is_multifile(Mod,F,A),
	!,
%PBC	fail.
	true.

class_sentence_trans((:- concurrent(F/A)),Exp,Mod) :-
	!,
%PBC	class_sentence_trans((:- data(F/A)),Exp,Mod),
	( is_concurrent(Mod,F,A) -> true ; 
          asserta_fact(is_concurrent(Mod,F,A)) ),
	class_sentence_trans((:- data(F/A)),Exp,Mod).

%%------------------------------------------------------------------------
%% PUBLIC PREDICATE DECLARATION
%%------------------------------------------------------------------------

class_sentence_trans((:- public(F/A)),[],Mod) :-
	atom(F),
	integer(A),
	!,
	( is_public(Mod,F,A) -> true ; asserta_fact(is_public(Mod,F,A)) ),
	true.

class_sentence_trans((:- public(S)),[],Mod) :-
	!,
	message(Mod,error,
	 ['invalid public declaration: ',S]).

%%------------------------------------------------------------------------
%% INHERITABLE PREDICATE DECLARATION
%%------------------------------------------------------------------------

class_sentence_trans((:- inheritable(F/A)),[],Mod) :-
	atom(F),
	integer(A),
	!,
	( is_inheritable(Mod,F,A) -> true ; 
          asserta_fact(is_inheritable(Mod,F,A)) ),
	true.

class_sentence_trans((:- inheritable(X)),[],Mod) :-
	!,
	message(Mod,error,['invalid inheritable declaration: ',
	 inheritable(X)]).

%%------------------------------------------------------------------------
%% VIRTUAL PREDICATE DECLARATION
%%------------------------------------------------------------------------

class_sentence_trans((:- virtual(destructor/0)),[],Mod) :-
	!,
	message(Mod,error,
	 ['destructor/0 is not allowed to be virtual']).

class_sentence_trans((:- virtual(Module/A)),[],Module) :-
	!,
	message(Module,error,
	 [Module,'/',A,' is not allowed to be virtual']).

class_sentence_trans((:- virtual(F/A)),[],Mod) :-
	atom(F),
	integer(A),
	!,
	( is_virtual(Mod,F,A) -> true ; asserta_fact(is_virtual(Mod,F,A)) ),
	true.

class_sentence_trans((:- virtual(X)),[],Mod) :-
	!,
	message(Mod,error,['invalid virtual declaration: ',
	 virtual(X)]).

%%------------------------------------------------------------------------
%% PERSISTENT ATTRIBUTE DECLARATION
%%------------------------------------------------------------------------

%class_sentence_trans((:- persistent(F/A)),[],Mod) :-
%	atom(F),
%	integer(A),
%	is_persistent(Mod,F,A),
%	!.

%class_sentence_trans((:- persistent(F/A)),[],Mod) :-
%	atom(F),
%	integer(A),
%	is_state(Mod,F,A),
%	assertz_fact(is_persistent(Mod,F,A)),
%	!.

%class_sentence_trans((:- persistent(F/A)),Exp,Mod) :-
%	atom(F),
%	integer(A),
%	!,
%	assertz_fact(is_persistent(Mod,F,A)),
%	class_sentence_trans((:- data(F/A)),Exp,Mod).

%class_sentence_trans((:- persistent(X)),[],Mod) :-
%	!,
%	message(Mod,error,['invalid persistent declaration: ',
%	 persistent(X)]).

%%------------------------------------------------------------------------

class_sentence_trans((:-_),_,_) :-
	!,
	fail.

%%------------------------------------------------------------------------
%%
%% DECLARE INITIAL STATE
%%
%%------------------------------------------------------------------------

class_sentence_trans((Head :- _),[],Mod) :-
	functor(Head,F,A),
	is_state(Mod,F,A),
	!,
	message(Mod,error,['clause of ',F,'/',A,' ignored : ',
	  'only facts are allowed as initial state']).

class_sentence_trans((Head),[],Mod) :-
	functor(Head,F,A),
	is_state(Mod,F,A),
	\+ is_multifile(Mod,F,A),
	!,
	Head =.. [_|Args],
	atom_concat(':',Mod,Aux1),
	atom_concat(Aux1,'::',Aux2),
	atom_concat(Aux2,F,QfdF),
	assertz_fact(initial_state(Mod,QfdF,Args)),
	(debug_first_pass ->
	 inform_user([Mod,' INITIAL STATE : ',Head,' ',QfdF,' ',Args])
	;
	 true),
	fail.

%%------------------------------------------------------------------------
%%
%% CONSTRUCTOR DECLS
%%
%%------------------------------------------------------------------------

class_sentence_trans((Head :- _),_,Module) :-
	functor(Head,Module,Arity),
	( is_inheritable(Module,Module,Arity) -> true ;
	  asserta_fact(is_inheritable(Module,Module,Arity))
	),
	fail.

class_sentence_trans((Head),_,Module) :-
	functor(Head,Module,Arity),
	( is_inheritable(Module,Module,Arity) -> true ;
	  asserta_fact(is_inheritable(Module,Module,Arity))
	),
	fail.

%%------------------------------------------------------------------------
%%
%% METHOD CLAUSE RECOGNITION
%%
%% METHOD BODY WILL BE EXPANDED AT SECOND PASS
%%------------------------------------------------------------------------

class_sentence_trans((Head :- Body),[(NewHead :- Body)],Mod) :-
	functor(Head,F,A),
	\+ is_state(Mod,F,A),
	\+ is_multifile(Mod,F,A),
	!,
	method_head(Head,NewHead,_),
	( is_method(Mod,F,A) -> true ; asserta_fact(is_method(Mod,F,A)) ),
	true.

class_sentence_trans((Head),[(NewHead)],Mod) :-
	functor(Head,F,A),
	\+ functor(Head,':-',_),
	\+ is_state(Mod,F,A),
	\+ is_multifile(Mod,F,A),
	!,
	method_head(Head,NewHead,_),
	( is_method(Mod,F,A) -> true ; asserta_fact(is_method(Mod,F,A)) ),
	true.

%%------------------------------------------------------------------------
%%
%% METHOD BODY EXPANSION
%% (ALSO CALLED "SECOND PASS EXPANSION").
%%
%% method_expansion/4 is defined in class_tr_aux.pl
%%------------------------------------------------------------------------

class_clause_trans(clause(0,0),clause(0,0),Module) :-
	generate_oop_info(Module),
	defines_module(Base,Module),
	start_of_messages(Module,['Compiling class ',Base,'.pl']),
	additional_itf_checking(Module),
	generate_class_template(Module),
	fail.

class_clause_trans(clause('$end$$of$$expansion$',_),_,Module) :-
	end_of_messages(Module),
	!,
	fail.

class_clause_trans(clause('goal$exp'(_,_,_),_),_,_) :-
	!,
	fail.

class_clause_trans(clause('fact$exp'(_,_,_),_),_,_) :-
	!,
	fail.

class_clause_trans(clause(mod_exp(_,_,_,_,_),_),_,_) :-
	!,
	fail.

class_clause_trans(clause(Head,Body),clause(Head,NewBody),Module) :-
	functor(Head,F,LastArgNumber),
	\+ is_multifile(Module,F,LastArgNumber),
	LastArgNumber > 0,
	arg(LastArgNumber,Head,LastArg),
	var(LastArg),
	!,
	method_expansion(Body,Module,LastArg,NewBody),
	( debug_second_pass ->
	  (
	      inform_user(['-- expanded body of ',Head,':']),nl,
	      inform_user([NewBody]),nl,nl
	  ) ; true
	),
	true.

%%------------------------------------------------------------------------
%%
%% GENERATE FIXED END-OF-EXPANSION CLAUSES  (At first-pass expansion)
%%
%%------------------------------------------------------------------------

generate_fixed_clauses(Module,FixedClauses) :-
	FixedClauses =
	[
	    (:- multifile     '$class$'/1),
	    (:- multifile     'class$super'/2),
	    (:- multifile     'class$call'/3),
	    (:- multifile     'class$initial_state'/3),
	    (:- multifile     'class$virtual'/6),
	    (:- multifile     'class$attr_template'/4),
	    (:- multifile     'class$default_cons'/1),
	    (:- multifile     'class$constructor'/4),
	    (:- multifile     'class$destructor'/3),
	    (:- multifile     'class$implements'/2),
	    (:- use_module(engine(internals), [last_module_exp/5])),
	    (:- use_module(engine(hiord_rt), ['$meta_call'/1])),
	    (:- redefining(mod_exp/5)),
	    ('$class$'(Module)),
	    ('$force$runtime$info$'(X) :- call(X))
                                             % Forces Ciao compiler to generate
                                             % run-time info for this 
	                                     % module.
	],
	true.

%%------------------------------------------------------------------------
%%
%% GENERATE STATE INITIALIZATION CLAUSES (At first-pass expansion)
%%
%%------------------------------------------------------------------------

generate_initial_state_clauses(Module,InitialStateClauses) :-
	findall(
	  'class$initial_state'(Module,F,Args),
	  initial_state(Module,F,Args),
	  InitialStateClauses
	),
	true.

%%------------------------------------------------------------------------
%%
%% GENERATE META PREDICATE SPECIFICATION DECLS. (at first-pass expansion)
%%
%%------------------------------------------------------------------------

generate_metapredicate_decls(Module,Decls) :-
	findall(
		   meta(F,A,Spec),
		   (is_metapredicate(Module,Spec),
		    functor(Spec,F,A)
		   ),
		   PreviousMetaDecls
	),
	metapredicate_decl(Module,PreviousMetaDecls,Decls).

metapredicate_decl(_,[],[]).

metapredicate_decl(Mod,[meta(F,A,Spec)|Nm],[(:-meta_predicate(NewSpec))|N] ) :-
	is_method(Mod,F,A),
	!,
	method_head(Spec,NewSpec,'?'),
	metapredicate_decl(Mod,Nm,N).

metapredicate_decl(Mod,[meta(F,A,_)|Nm],N) :-
	is_state(Mod,F,A),
	!,
	message(Mod,warning,
	 ['meta-predicate specification of ',F,'/',A,
	  ' ignored since this is an attribute']),
	metapredicate_decl(Mod,Nm,N).

metapredicate_decl(Mod,[meta(_,_,Spec)|Nm],[(:- meta_predicate(Spec))|N]) :-
	metapredicate_decl(Mod,Nm,N).

%%------------------------------------------------------------------------
%%
%% GENERATE CLASS INTERFACE INFO (at first-pass expansion)
%%
%%------------------------------------------------------------------------

generate_interface_info(Module,Interface) :-
	interface_checking(Module),
	findall((:- virtual(F/A)),is_virtual(Module,F,A),VirtualList),
	findall((:- public(F/A)),is_public(Module,F,A),PublicList),
	findall((:- inheritable(F/A)),is_inheritable(Module,F,A),InhList),
	findall((:- attribute(F/A)),is_state(Module,F,A),AttrList),
	findall((:- method(F/A)),is_method(Module,F,A),MethodList),
	append(PublicList,InhList,Aux1),
	append(Aux1,AttrList,Aux2),
	append(Aux2,MethodList,Aux3),
	append(Aux3,VirtualList,Interface),
	true.

%%------------------------------------------------------------------------

interface_checking(Module) :-
	is_public(Module,F,A),
	check_public_info(Module,F,A),% Note: this goal may retract is_public/3
	fail.

interface_checking(Module) :-     % public preds are inheritable by default
	is_public(Module,F,A),
	\+ is_inheritable(Module,F,A),
	asserta_fact(is_inheritable(Module,F,A)), 
	fail.

interface_checking(Module) :-
	is_inheritable(Module,F,A),
	check_inheritable_info(Module,F,A),
	fail.

interface_checking(Module) :-
	is_virtual(Module,F,A),
	check_virtual_info(Module,F,A),
	fail.

interface_checking(_).

%%------------------------------------------------------------------------
%% CHECK PUBLIC INTERFACE INFORMATION
%%------------------------------------------------------------------------

% 1st case: public predicate is a multifile: not allowed.

check_public_info(Module,F,A) :-
	is_multifile(Module,F,A),
	!,
	retract_fact(is_public(Module,F,A)),
	message(Module,error,['multifile ',F,'/',A,' is not allowed to be ',
	 'public.']).

% 2nd case: exported predicate is a destructor.

check_public_info(Module,destructor,0) :-
	message(Module,warning,['class destructor is public !!!']),
	fail.

check_public_info(_,_,_).

%%------------------------------------------------------------------------
%% CHECK INHERITABLE INTERFACE INFORMATION
%%------------------------------------------------------------------------

% 1st case: inheritable predicate is a multifile: not allowed.

check_inheritable_info(Module,F,A) :-
	is_multifile(Module,F,A),
	!,
	retract_fact(is_inheritable(Module,F,A)),
	message(Module,error,['multifile ',F,'/',A,' is not allowed to be ',
	 'inheritable.']).

% 2nd case: inheritable predicate is a destructor.

check_inheritable_info(Module,destructor,0) :-
	message(Module,warning,['class destructor is inheritable !!!']),
	fail.

% 3rd case: inheritable predicate was not defined in current source,

check_inheritable_info(Module,F,A) :-
	\+ is_method(Module,F,A),
	\+ is_state(Module,F,A),
	!,
	retract_fact(is_inheritable(Module,F,A)),
	message(Module,warning,['inheritable declaration of ',F,'/',A,
	 ' ignored: predicate was not defined at current class']).

check_inheritable_info(_,_,_).

%%------------------------------------------------------------------------
%% CHECK VIRTUAL INTERFACE INFORMATION
%%------------------------------------------------------------------------

% 1st case: virtual predicate is a multifile: not allowed.

check_virtual_info(Module,F,A) :-
	is_multifile(Module,F,A),
	!,
	retract_fact(is_virtual(Module,F,A)),
	message(Module,error,['multifile ',F,'/',A,' is not allowed to be ',
	 'virtual']).

% 2nd case: exported predicate was not defined in current source.

check_virtual_info(Module,F,A) :-
	\+ is_method(Module,F,A),
	\+ is_state(Module,F,A),
	!,
	retract_fact(is_virtual(Module,F,A)),
	message(Module,error,['virtual ',F,'/',A,
	 ' must be a method or attribute defined at this class']).

check_virtual_info(_,_,_).

%%------------------------------------------------------------------------
%%
%% GENERATE REDEFINING/1 CLAUSES TO AVOID SOME ANNOYING WARNINGS
%% (at first-pass expansion)
%%------------------------------------------------------------------------

generate_redefining_clauses(Module,Redef) :-
	findall(
		   (:- redefining(NewF/NewA)),
		   (
		         is_method(Module,F,A),
			 atom_concat('obj$',F,NewF),
			 NewA is A+1
		   ),
		   Redef
	).

%%------------------------------------------------------------------------
%%
%% GENERATE REEXPORT CLAUSES
%% (at first-pass expansion)
%%------------------------------------------------------------------------

generate_reexport_clauses(Module,Clauses) :-
	inherit_class(Module,SuperFile),
	!,
	Clauses = [(:- reexport(SuperFile))],
	true.

generate_reexport_clauses(_,[]).

%%------------------------------------------------------------------------
%%
%% GENERATE IMPLEMENTS/1 DECLARATIONS FOR .itf STORAGE
%% (at first-pass expansion)
%%------------------------------------------------------------------------

generate_implements_decls(Module,Decl) :-
	findall(
		   (:- implements(Itf)),
		   implements(Module,Itf),
		   Decl
	),
	true.

%%------------------------------------------------------------------------
%%
%% GENERATE PERSISTENCY DECLARATIONS
%% (at first-pass expansion)
%%------------------------------------------------------------------------

%generate_persistency_decls(_,
%	[(:- inheritable(persistent_attribute/1)),
%	 (:- virtual(persistent_attribute/1)),
%	 (:- method(persistent_attribute/1))]).

%%------------------------------------------------------------------------
%%
%% GENERATE CLASS TEMPLATE CLAUSES (at second-pass expansion)
%%
%%------------------------------------------------------------------------

% Declare fact$exp (it may have no clauses)
generate_class_template(Module) :-
	define_pred(Module, 'fact$exp', 3),
	fail.

% Import all methods which may be called from this class.

generate_class_template(Module) :-
	implementation(Module,method,AtClass,F,A),
	atom_concat(['obj$',F],NewF),
	NewA is A+1,
	\+ imports(Module,AtClass,NewF,NewA,AtClass),
	assertz_fact(imports(Module,AtClass,NewF,NewA,AtClass)),
	fail.

generate_class_template(Module) :-
	inherited_method_from(Module,AtClass,F/A),
	atom_concat(['obj$',F],NewF),
	NewA is A+1,
	\+ imports(Module,AtClass,NewF,NewA,AtClass),
	assertz_fact(imports(Module,AtClass,NewF,NewA,AtClass)),
	fail.

% Generate attribute template for new/2 to create objects.

generate_class_template(Module) :-
	attribute_set(Module,AtClass,F,A),
	atom_concat([':',AtClass,'::',F],QfdRealF),
	attribute_kind(Module,F,A,Kind),
	add_clause(Module,'class$attr_template'(Module,QfdRealF,A,Kind)),
	fail.

% Generate implemented interface info
% Used by runtime type tests.

generate_class_template(Module) :-
	impl_interface(Module,Itf),
	add_clause(Module,'class$implements'(Module,Itf)),
	fail.

% Generate inheritance relationship info.
% Used by runtime type tests.

generate_class_template(Module) :-
	super(Module,Super),
	add_clause(Module,'class$super'(Module,Super)),
	fail.

% Generate constructor info
% Used by new/2.

generate_class_template(Module) :-
	is_method(Module,Module,Arity),
	functor(Cons,Module,Arity),
	method_head(Cons,ConsImpl,Inst),
	method_clause_creation(Module,ConsImpl,MetaCall),
	functor(ConsImpl,CF,CA),
	functor(Check,CF,CA),
	( meta_args(Module,Check) ->
  	  add_clause(Module,
	   ( 'class$constructor'(Cons,Inst,From,Exp) :-
             last_module_exp(Module:ConsImpl,goal,From,Module,Exp)))
	;
  	  add_clause(Module,'class$constructor'(Cons,Inst,_,MetaCall))
	),
	fail.

generate_class_template(Module) :-
	\+ is_method(Module,Module,_),
	add_clause(Module,'class$default_cons'(Module)),
	fail.

% Generate destructor info
% Used by destroy/1.

generate_class_template(Module) :-
	is_method(Module,destructor,0),
	method_head(destructor,CodeDestr,Inst),
	module_concat(Module,CodeDestr,MetaCall),
	add_clause(Module,'class$destructor'(Module,Inst,MetaCall)),
	fail.

% Generate runtime expansor needed when meta-programming is 
% present at user code. Those clauses will be called from class_rt:mod_exp/5.
% (But those are not multifile ir order to preserve Prolog indexing...)
% Notice that this expansor is not related to Var:goal. It is used
% for goals such as X=inherited(goal(1)),call(X).

generate_class_template(Module) :-
	attribute_from(Module,AtClass,F/A),
	\+ virtual_pred(Module,attribute,F,A),
	functor(Goal,F,A),
	Goal =.. [_|Args],
	atom_concat([':',AtClass,'::',F],QfdRealF),
	InstGoal =.. [QfdRealF|Args],
	add_clause(Module,
	  ( 'fact$exp'(Goal,Inst,Exp) :-
	         class_rt:functor_concat(Inst,InstGoal,Exp)
	  )
	),
	fail.

generate_class_template(Module) :-
	virtual_pred(Module,attribute,F,A),
	functor(Goal,F,A),
	add_clause(Module,( 'fact$exp'(Goal,Inst,Exp) :-
             virtual:mod_exp(fact,Goal,Module,Inst,Exp))
	),
	fail.

generate_class_template(Module) :-
	inherited_attribute_from(Module,AtClass,F/A),
	functor(Goal,F,A),
	Goal =.. [_|Args],
	atom_concat([':',AtClass,'::',F],QfdRealF),
	InstGoal =.. [QfdRealF|Args],
	add_clause(Module,
	  ( 'fact$exp'(inherited(Goal),Inst,Exp) :-
		  class_rt:functor_concat(Inst,InstGoal,Exp)
	  )
	),
	fail.

generate_class_template(Module) :-
	method_from(Module,AtModule,F/A),
	\+ virtual_pred(Module,method,F,A),
	functor(MethodHead,F,A),
	method_head(MethodHead,CodeHead,Inst),
	method_clause_creation(AtModule,CodeHead,ChainClause),
	functor(CodeHead,CF,CA),
	functor(Check,CF,CA),
	( meta_args(AtModule,Check) ->
  	  add_clause(Module,
	   ( 'goal$exp'(MethodHead,Inst,Exp) :-
             last_module_exp(AtModule:CodeHead,goal,Module,Module,Exp)))
	;
	  add_clause(Module,'goal$exp'(MethodHead,Inst,ChainClause))
	),
	fail.

generate_class_template(Module) :-
	virtual_pred(Module,method,F,A),
	functor(Goal,F,A),
	add_clause(Module,( 'goal$exp'(Goal,Inst,Exp) :-
             virtual:mod_exp(goal,Goal,Module,Inst,Exp))
	),
	fail.

generate_class_template(Module) :-
	inherited_method_from(Module,AtModule,F/A),
	functor(MethodHead,F,A),
	method_head(MethodHead,CodeHead,Inst),
	method_clause_creation(AtModule,CodeHead,ChainClause),
	functor(CodeHead,CF,CA),
	functor(Check,CF,CA),
	( meta_args(AtModule,Check) ->
  	  add_clause(Module,
	   ( 'goal$exp'(inherited(MethodHead),Inst,Exp) :-
             last_module_exp(AtModule:CodeHead,goal,Module,Module,Exp))) 
	;
	 add_clause(Module,'goal$exp'(inherited(MethodHead),Inst,ChainClause))
	),
	fail.

generate_class_template(Module) :-
	add_clause(Module,('goal$exp'(Goal,Inst,Exp) :- 
		Module:'fact$exp'(Goal,Inst,Exp))
	),
	fail.

% Generate interface information (accesibility information).
% Used by runtime expansors and relatives...

generate_class_template(Module) :-
	implementation(Module,K,AtClass,F,A),
	accessible_check('$goal',F,A,Access),
	( public_pred(Module,K,F,A) ->   % Predicate is world-accessible
	  (
	      add_clause(Module,Access),
	      fail
	  )
	;
	  true
	),
	arg(1,Access,Module),        % Predicate is accessible by this class.
	add_clause(Module,Access),
	AtClass \== Module,
	accessible_check('$goal',F,A,AccessAux),
	arg(1,AccessAux,AtClass),    % Predicate is accessible by inh. class.
	add_clause(Module,AccessAux),
	fail.

generate_class_template(Module) :-
	implementation(Module,attribute,AtClass,F,A),
	accessible_check('$fact',F,A,Access),
	( public_pred(Module,attribute,F,A) -> % Predicate is world-accessible
	  (
	      add_clause(Module,Access),
	      fail
	  )
	;
	  true
	),
	arg(1,Access,Module),        % Predicate is accessible by this class.
	add_clause(Module,Access),
	AtClass \== Module,
	accessible_check('$fact',F,A,AccessAux),
	arg(1,AccessAux,AtClass),    % Predicate is accessible by inh. class.
	add_clause(Module,AccessAux),
	fail.

%generate_class_template(Module) :-
%	public_pred(Module,_,F,A),
%	add_clause(Module,('class$goal_accessible'(_,F,A) :-!)),
%	fail.

%generate_class_template(Module) :-
%	implementation(Module,Kind,AtClass,F,A),
%	AtClass \== Module,
%	\+ public_pred(Module,Kind,F,A),
%	add_clause(Module,('class$goal_accessible'(AtClass,F,A) :-!)),
%	fail.

% Generate run-time expansor for methods.
% Used in order to translate Var:goal.

generate_class_template(Module) :-
	implementation(Module,method,AtModule,F,A),
	functor(MethodHead,F,A),
	accessible_check('$goal',F,A,Accessible),
	arg(1,Accessible,From),
	method_head(MethodHead,CodeHead,Inst),
	method_clause_creation(AtModule,CodeHead,ChainClause),
	functor(CodeHead,CF,CA),
	functor(Check,CF,CA),
	( meta_args(AtModule,Check) ->
  	  add_clause(Module,
	   ( mod_exp(goal,
	                  MethodHead,
			  From,
			  Inst,
                          Exp) :-
	     Module:Accessible,
             last_module_exp(AtModule:CodeHead,goal,From,Module,Exp)))
	;
	  add_clause(Module,
	   (mod_exp(goal,MethodHead,From,Inst,ChainClause)
	     :- Module:Accessible
	    
	  ))
	),
	fail.

% Generate run-time expansor for attributes
% Used in order to translate Var:fact.

generate_class_template(Module) :-
	implementation(Module,attribute,AtClass,F,A),
	accessible_check('$fact',F,A,Accessible),
	arg(1,Accessible,From),
	functor(Goal,F,A),
	Goal =.. [_|Args],
	atom_concat([':',AtClass,'::',F],QfdRealF),
	InstGoal =.. [QfdRealF|Args],
	add_clause(Module,
	  ( mod_exp(goal,Goal,From,Inst,Exp) :-
	          Module:Accessible,
		  class_rt:functor_concat(Inst,InstGoal,Exp)
	  )
	),
	add_clause(Module,
	  ( mod_exp(fact,Goal,From,Inst,Exp) :-
	          Module:Accessible,
		  class_rt:functor_concat(Inst,InstGoal,Exp)
	  )
	),
	fail.

generate_class_template(Module) :-
	add_clause(Module,
	  ( mod_exp(_,Goal,From,_,_) :-
	     throw(error(existence_error(object_goal,Goal),From)),
	     !,
	     fail
	  )
	),
	fail.

% Generate run-time expansor for virtual calling
% stored at multifile sheet...

generate_class_template(Module) :-
	virtual_pred_template(Module,Module),
	fail.

% Generate calling optimization clauses
% (to be used by objects expansion)

generate_class_template(Module) :-
	implementation(Module,method,AtModule,F,A),
	accessible_check('$goal',F,A,Accessible),
	arg(1,Accessible,From),
	functor(Head,F,A),
	Head =.. [_|Args],
	atom_concat(['obj$',F],NewF),
	append(Args,[ID],NewArgs),
	ImplHead    =.. [NewF|NewArgs],
	functor(ImplHead,IF,IA),
	functor(Check,IF,IA),
	( meta_args(Module,Check) ->
  	  add_clause(Module,
	   ( 'class$goalcalling'(Head,ID,From) :-
	     Module:Accessible,
             last_module_exp(AtModule:ImplHead,goal,From,Module,Exp),
	     !,
             '$meta_call'(Exp)
	   ))
	;
	   add_clause(Module,
	   ('class$goalcalling'(Head,ID,From) :-
	     Module:Accessible,
	     !,
	     AtModule:ImplHead ) )
	),
	fail.

generate_class_template(Module) :-
	implementation(Module,attribute,AtClass,F,A),
	accessible_check('$fact',F,A,Accessible),
	arg(1,Accessible,From),
	functor(Goal,F,A),
	Goal =.. [_|Args],
	atom_concat([':',AtClass,'::',F],QfdRealF),
	InstGoal =.. [QfdRealF|Args],
	add_clause(Module,
	  ( 'class$goalcalling'(Goal,Inst,From) :-
	          Module:Accessible,
		  class_rt:functor_concat(Inst,InstGoal,Exp),
		  !,
		  '$meta_call'(Exp)
	  )
	),
	fail.

generate_class_template(Module) :-
	add_clause(Module,
	 ('class$goalcalling'(Goal,_,From) :- 
           throw(error(existence_error(object_goal,Goal),From)) 
	 )
	),
	fail.

generate_class_template(Module) :-
	functor(Index,Module,1),
	arg(1,Index,ID),
	add_clause(Module,
	 ('class$call'(Index,Method,From) :- 
	  Module:'class$goalcalling'(Method,ID,From)
	 )
	),
	fail.

% Generate persistent predicate info

%generate_class_template(Module) :-
%	is_persistent(Module,F,A),
%	is_state(Module,F,A),
%	atom_concat([':',Module,'::',F],NewF),
%	functor(Attr,NewF,A),
%	add_clause(Module,'obj$persistent_attribute'(Attr,_)),
%	fail.

%generate_class_template(Module) :-
%	inherited_pred(Module,method,_,persistent_attribute,1),
%	add_clause(Module,('obj$persistent_attribute'(Attr,_) :- 
%		  inherited(persistent_attribute(Attr)))),
%	fail.

% Do not fail.

generate_class_template(_).

%%------------------------------------------------------------------------

virtual_pred_template(Module,FromClass) :-
	virtual_pred(FromClass,attribute,F,A),
	implementation(Module,attribute,AtClass,F,A),
	functor(Goal,F,A),
	Goal =.. [_|Args],
	atom_concat([':',AtClass,'::',F],QfdRealF),
	InstGoal =.. [QfdRealF|Args],
	add_clause(Module,
	 ( 'class$virtual'(Module,FromClass,attribute,Goal,Inst,Exp) :-
	          class_rt:functor_concat(Inst,InstGoal,Exp)
	 )
	),
	fail.

virtual_pred_template(Module,FromClass) :-
	virtual_pred(FromClass,method,F,A),
	implementation(Module,method,AtClass,F,A),
	functor(MethodHead,F,A),
	method_head(MethodHead,CodeHead,Inst),
	method_clause_creation(AtClass,CodeHead,ChainClause),
	functor(CodeHead,CF,CA),
	functor(Check,CF,CA),
	( meta_args(Module,Check) ->
  	  add_clause(Module,
	   ( 'class$virtual'(Module,FromClass,method,
	                  MethodHead,
			  Inst,
                          Exp) :- 
             last_module_exp(AtClass:CodeHead,goal,AtClass,Module,Exp)))
	;
	  add_clause(Module,
	    'class$virtual'(Module,FromClass,method,
            MethodHead,Inst,ChainClause))
	),
	fail.

virtual_pred_template(Module,FromClass) :-
	defines_module(Base,FromClass),
	c_itf_internal:decl(Base,super(Super)),
	!,
	virtual_pred_template(Module,Super).

virtual_pred_template(_,_).

%%------------------------------------------------------------------------
%%
%% PERFORNM ADDITIONAL ITF CHECKING
%% (at second-pass expansion)
%%------------------------------------------------------------------------

:- data itf_check/4.   % Auxiliary.

% Interface was not interface-expanded nor class-expanded

additional_itf_checking(Module) :-
	impl_interface(Module,ITF),
	defines_module(Base,ITF),
        \+ c_itf_internal:includes(Base,library(interface)),
        \+ c_itf_internal:includes(Base,library(class)),
	message(Module,error,
	  ['implemented interface ',ITF,' is not a valid interface']),
	retract_fact(implements(Module,ITF)),
	fail.

% Interface can not be implemented

additional_itf_checking(_) :-
	retractall_fact(itf_check(_,_,_,_)),
	fail.

additional_itf_checking(Module) :-
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

% Super class was not class-expanded

additional_itf_checking(Module) :-
	super(Module,Super),
	defines_module(Base,Super),
        \+ c_itf_internal:includes(Base,library(class)),
	message(Module,error,['inherited ',Super,' must be a class']),
	retract_fact(super(Module,_)),
	fail.

% Circular inheritance is present

additional_itf_checking(Module) :-
	super(Module,Super),
	get_inheritance_line(Super,InhLine),
	member(Module,InhLine),
	message(Module,error,
          ['circular inheritance: ',Super,' is not a valid super-class']),
	retract_fact(super(Module,_)),
	fail.

% There is no call to inherited constructor

additional_itf_checking(Module) :-
	super(Module,Super),
	inherited_pred(Module,method,Super,Super,_),
	\+ is_method(Module,Module,_),
	message(Module,warning,
	  ['There is no call to inherited constructor/s']),
	fail.

% multifile F/A is also an inherited method or attribute definition

additional_itf_checking(Module) :-
	is_multifile(Module,F,A),
	inherited_pred(Module,_,_,F,A),
	message(Module,warning,
	  ['multifile ',F,'/',A,' hides inherited predicate']),
	fail.

% required predicate was not implemented.

additional_itf_checking(Module) :-
	public_pred(Module,Kind,F,A),
	\+ implementation(Module,Kind,_,F,A),
	message(Module,error,
	 [Kind,' ',F,'/',A,' must be implemented']),
	set_fact(module_error),
	fail.

% visibility of redefined F/A is more restrictive than the inherited one

additional_itf_checking(Module) :-
	defines_module(Base,Module),
	implementation(Module,_,Module,F,A),
	inherited_pred(Module,_,_,F,A),
	\+ c_itf_internal:decl(Base,inheritable(F/A)),
	\+ c_itf_internal:decl(Base,public(F/A)),
	message(Module,error,
	  ['local implementation of ',F,'/',A,
	   ' hides inheritable definition',
           ' from ascendant class']),
	fail.

additional_itf_checking(Module) :-
	defines_module(Base,Module),
	implementation(Module,_,Module,F,A),
	inherited_pred(Module,_,_,F,A),
	public_pred(Module,_,F,A),
	\+ c_itf_internal:decl(Base,public(F/A)),
	message(Module,error,
	  ['local implementation of ',F,'/',A,' hides public definition',
           ' from ascendant class']),
	fail.

% public F/A is unknown

additional_itf_checking(Module) :-
	defines_module(Base,Module),
	c_itf_internal:decl(Base,public(F/A)),
	\+ c_itf_internal:decl(Base,method(F/A)),
	\+ c_itf_internal:decl(Base,attribute(F/A)),
	\+ inherited_pred(Module,_,_,F,A),
	message(Module,error,
	  ['public predicate ',F,'/',A,' was not defined nor inherited']),
	fail.

% There are persistent declarations but there is no persistent support.

%additional_itf_checking(Module) :-
%	is_persistent(Module,F,A),
%	\+ impl_interface(Module,persistent),
%	message(Module,warning,
%	 ['attribute ',F,'/',A,' was declared as persistent but there is',
%          ' no persistency support']).

additional_itf_checking(_).

%%------------------------------------------------------------------------

get_inheritance_line(Class,[Class|N]) :-
	defines_module(Base,Class),
	c_itf_internal:decl(Base,super(Super)),
	!,
	get_inheritance_line(Super,N).

get_inheritance_line(Class,[Class]).

%%------------------------------------------------------------------------
%%
%% OTHER AUXILIARY PREDICATES
%%
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% GENERATE CHAINING CLAUSE FOR METHOD CREATION TO WORK 
%%------------------------------------------------------------------------

method_clause_creation(Class,Code,NeededClause) :-
	module_concat(Class,Code,NeededClause).

%%------------------------------------------------------------------------
%% FIND ADDMODULE DECLARATION AT METAPREDICATE
%%------------------------------------------------------------------------

has_addmodule([]) :- 
	!,
	fail.

has_addmodule([addmodule|_]) :- 
	!,
	true.

has_addmodule([_|N]) :-
	has_addmodule(N).

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

%%------------------------------------------------------------------------
%% Convert a method head to its implementation head
%%------------------------------------------------------------------------

method_head(Method,Code,Obj) :-
	var(Code),
	Method =.. [F|Args],
	append(Args,[Obj],NewArgs),
	atom_concat('obj$',F,NewF),
	Code   =.. [NewF|NewArgs].

%%------------------------------------------------------------------------
%% Compute whether attrubute is dynamic or concurrent
%%------------------------------------------------------------------------

attribute_kind(Module,F,A,concurrent) :-
	is_concurrent(Module,F,A),
	!.

attribute_kind(_,_,_,dynamic).

%%------------------------------------------------------------------------
%% Convert an attribute head to its implementation head
%%------------------------------------------------------------------------
/*
attribute_head(Class,Attr,Code) :-
	Attr =.. [F|Args],
	atom_concat(Class,'::',Aux),
	atom_concat(Aux,F,NewF),
	Code   =.. [NewF|Args].
*/
%%------------------------------------------------------------------------
%% Convert an attribute head to its implementation head
%%------------------------------------------------------------------------

accessible_check(Kind,F,A,Goal) :-
	number_codes(A,Codes),
	atom_codes(Arity,Codes),
	atom_concat([Kind,F,Arity],GoalF),
	functor(Goal,GoalF,1).

%%------------------------------------------------------------------------
%% Add new clauses on second-pass expansion
%%------------------------------------------------------------------------

add_clause(Module,(Head :- Body)) :-
	!,
	defines_module(Base,Module),
	functor(Head, F, A),
	define_pred(Module, F, A),
	add_clause_of(
		Base,
		Head,
		Body,
		_,
		'*class_expansion*',
		0,0),
	( debug_expansion_info -> 
          (inform_user(['Added clause: ',Head,' :- ',Body])) ; true 
	),
	true.
add_clause(Module,Head) :-
	add_clause(Module,(Head :- true)).

define_pred(Module, F, A) :-
	current_fact(defines(Module, F, A)), !.
define_pred(Module, F, A) :-
	assertz_fact(defines(Module, F, A)).

add_clause_of(Base, Head, Body, VarNames, Source, Line0, Line1) :-
	assertz_fact(c_itf_internal:clause_of(Base, Head, Body, VarNames, 
                     Source, Line0, Line1)).

%%------------------------------------------------------------------------
%%
%% Error message reporting
%%
%%------------------------------------------------------------------------

:- use_module(library(class/class_error_reporting)).

%%------------------------------------------------------------------------
%%
%% MACROS TO HANDLE OBJECT ORIENTED DEFINITIONS FROM ITF FILES
%%
%%------------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

% Check whether F/A is inherited and it is a method

inherited_method_from(Module,From,F/A) :-
	inherited_pred(Module,method,From,F,A).

% Check whether F/A is inherited and it is an attribute

inherited_attribute_from(Module,From,F/A) :-
	inherited_pred(Module,attribute,From,F,A).

% Check whether F/A is a defined attribute

attribute_from(Module,Module,Spec) :-
	defines_module(Base,Module),
	c_itf_internal:decl(Base,attribute(Spec)),
	!.

attribute_from(Module,From,Spec) :-
	inherited_attribute_from(Module,From,Spec).

% Check whether F/A is a defined method.

method_from(Module,Module,Spec) :-
	defines_module(Base,Module),
	c_itf_internal:decl(Base,method(Spec)),
	!.

method_from(Module,From,Spec) :-
	  inherited_method_from(Module,From,Spec).
