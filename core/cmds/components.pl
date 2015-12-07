
:- module(components,relevant_files/3,[assertions,hiord]).

:- use_module(library(compiler/c_itf)).
:- use_module(library(errhandle), [error_protect/1]).  
:- use_module(library(ctrlcclean), [ctrlc_clean/1]).
:- use_module(library(aggregates), [findall/3]).  
:- use_module(library(sort), [sort/2]).  
:- use_module(library(llists), [flatten/2]).  
:- use_module(library(lists), [append/3]).  

:- doc(module,"This utility allows you to gather the files that form
	the frontier of the module interface of a component (a group of
        modules). The frontier of the component are the modules directly
	imported by the modules that form the component.").

:- doc(author,"Francisco Bueno").

:- doc(relevant_files(Part,Files,G),
	"@var{Files} are the files in the frontier of the component
	  identified by @var{Part}, such that every component of your
	  software (including @var{Part}) is defined by 
	  @var{G}(@tt{Com},@tt{Main},@tt{Mods}),
	  where @tt{Main}, is the lists of main modules of component
	  @tt{Com}, and @tt{Mods} the list of the rest of the modules in
	  component @tt{Com}.").

:- meta_predicate relevant_files(?,?,pred(3)).

relevant_files(Part,Files,G):-
	retractall_fact(reached(_,_)),
	G(Part,Main,Comp),
	file_names(Main,Files0),
	group(All,G),
	file_names(All,AllFiles),
	process(Files0,AllFiles),
	file_names(Comp,Files1),
	append(Files0,Files1,Files2),
	findall(F,reachable(Files2,F),Files3),
	append(Files2,Files3,Files4),
	sort(Files4,Files).

file_names([],[]).
file_names([F|Files],[B|Bases]):-
	absolute_file_name(F,B),
	file_names(Files,Bases).

process([],_AllFiles).
process([F|Files],AllFiles):-
	error_protect(ctrlc_clean(
	     process_files_from(F, asr, any, 
	                ok(AllFiles), false, false, true)
		      )),
	process(Files,AllFiles).

ok(Base,AllFiles):-
	defines_module(Base, Module),
	imports(Module, Mod2, _F, _A, _EndMod),
	defines_module(Imp, Mod2),
	absolute_file_name(Base, FSo),
	absolute_file_name(Imp, FSi),
	member(FSi,AllFiles),
	assertz_fact(reached(FSo, FSi)),
	fail.
ok(_Base,_AllFiles).

true(_).

:- data reached/2.

reachable(Files,F):-
	member(S, Files),
	reached(S, F).

group(Files,G):-
	findall(L, ( G(_,M,C), append(M,C,L) ), LL),
	flatten(LL, Files0),
	sort(Files0, Files).
