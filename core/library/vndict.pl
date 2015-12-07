:- module(vndict,
	[ null_dict/1, create_dict/2, 
	  create_pretty_dict/2,
	  complete_dict/3, complete_vars_dict/3,
	  complete_dict_alpha/3, complete_vars_dict_alpha/3,
	  prune_dict/3, sort_dict/2,
	  dict2varnamesl/2, varnamesl2dict/2,
	  find_name/4, prettyvars/2,
	  rename/2,
	  varnamedict/1,
	  vars_names_dict/3
	],
	[ assertions, basicmodes, regtypes
	]).

:- use_module(library(varnames/dict_types)).
:- use_module(library(lists)).
:- use_module(library(idlists), [memberchk/2]).
:- use_module(library(terms_vars)).
:- use_module(library(sets)).
:- use_module(library(sort)).

:- doc(title, "Variable name dictionaries").

:- doc(author, "Francisco Bueno").
:- doc(author, "Edison Mera").
:- doc(author, "Alejandro Serrano").

%-------------------------------------------------------------------------
:- regtype varnamedict(D) # "@var{D} is a dictionary of variable names.".

varnamedict(dic([], [])).
varnamedict(dic([V|Vs], [N|Ns])) :-
	var(V),
	varname(N),
	varnamedict(dic(Vs, Ns)).

:- pred vars_names_dict(Dict, Vars, Names)
	:: (varnamedict(Dict),list(Vars),list(Names))
     # "@var{Varss} is a sorted list of variables, and @var{Names} is a list
	of their names, which correspond in the same order.".

vars_names_dict(dic(Vs,Ns),Vs,Ns).

:- regtype null_dict(D) # "@var{D} is an empty dictionary.".

null_dict(dic([],[])).

:- pred create_dict(Term,Dict) : term(Term) => varnamedict(Dict)
	# "@var{Dict} has names for all variables in @var{Term}.".

create_dict(Term,Dict):- complete_dict(dic([],[]),Term,Dict).

:- pred sort_dict(D,Dict) : varnamedict(D) => varnamedict(Dict)
	# "@var{D} is sorted into @var{Dict}.".

sort_dict(dic(Vs,Ns),dic(Vars,Names)):-
	separate_lists(VNs,Vs,Ns),
	sort(VNs,SortedVNs),
	separate_lists(SortedVNs,Vars,Names).

%----------------------------------------------------------------------------
:- pred varnamesl2dict(VNs,Dict) : varnamesl(VNs) => varnamedict(Dict)
     # "Translates @var{VNs} to @var{Dict}.".

varnamesl2dict(VNs,dic(Vars,Names)):-
	inverse_dic(VNs,InvertedVNs),
	sort(InvertedVNs,SortedVNs),
	separate_lists(SortedVNs,Vars,Names).

inverse_dic([Name=Var|Di],[Var=Name|Do]):-
	inverse_dic(Di,Do).
inverse_dic([],[]).

separate_lists([Var=Name|D],[Var|Vs],[Name|Ns]):-
	separate_lists(D,Vs,Ns).
separate_lists([],[],[]).

:- pred dict2varnamesl(Dict,VNs) : varnamedict(Dict) => varnamesl(VNs)
     # "Translates @var{Dict} to @var{VNs}.".

dict2varnamesl(dic(Vars,Names),VNs):-
	separate_lists(InvertedVNs,Vars,Names),
	inverse_dic(VNs,InvertedVNs).

%-----------------------------------------------------------------------------

:- pred complete_dict(+Dict,+Term,-NewDict)
	# "@var{NewDict} is @var{Dict} augmented with the variables of
           @var{Term} not yet in @var{Dict}.".

complete_dict(dic(TmpVars,TmpNames),Cl,dic(Vars,Names)):-
	varset(Cl,AllVars),
	ord_subtract(AllVars,TmpVars,NewVars),
	complete_dict_(NewVars,TmpVars,TmpNames,1,Vars,Names).

:- pred complete_vars_dict(+Dict,+Vars,-NewDict)
	# "@var{NewDict} is @var{Dict} augmented with the variables of
           the list @var{Vars} not yet in @var{Dict}.".

complete_vars_dict(dic(TmpVars,TmpNames),NewVars,dic(Vars,Names)):-
	complete_dict_(NewVars,TmpVars,TmpNames,1,Vars,Names).

complete_dict_([],Vars,Names,_,Vars,Names).
complete_dict_([V|Vs],Vars,Names,N,NewVars,NewNames):-
	new_name(N,Names,NewName,NewN),
	insert_in_dict(Vars,Names,V,NewName,TmpVars,TmpNames),
	complete_dict_(Vs,TmpVars,TmpNames,NewN,NewVars,NewNames).

new_name(N,Names,NewName,NewN):-
	name(N,Name),
	name('_',[U]),
	name(Atom,[U|Name]),
	( memberchk(Atom,Names) ->
	  TmpN is N+1,
	  new_name(TmpN,Names,NewName,NewN)
	;
	  NewName = Atom,
	  NewN is N+1
	).

insert_in_dict([], [],Element0,Element1, [Element0],[Element1]).
insert_in_dict([Head1|Tail1],[Head2|Tail2],Element1,Element2, Set1,Set2) :-
	compare(O, Head1, Element1),
	insert_in_dict_(O,Head1,Head2,Tail1,Tail2,Element1,Element2,Set1,Set2).

insert_in_dict_(<, H1,H2,Tail1,Tail2, E1,E2, [H1|Set1],[H2|Set2]) :-
	insert_in_dict(Tail1,Tail2, E1,E2, Set1,Set2).
% insert_in_dict_(=, H1,H2, Tail1,Tail2, _,_, [H1|Tail1],[H2|Tail2]). not used
insert_in_dict_(>, H1,H2, Tail1,Tail2, E1,E2, [E1,H1|Tail1],[E2,H2|Tail2]).

%-----------------------------------------------------------------------------

:- pred complete_dict_alpha(+Dict,+Term,-NewDict)
	# "@var{NewDict} is @var{Dict} augmented with the variables of
           @var{Term} not yet in @var{Dict} with alphabetical names.".

complete_dict_alpha(dic(TmpVars,TmpNames),Cl,dic(Vars,Names)):-
	varset(Cl,AllVars),
	ord_subtract(AllVars,TmpVars,NewVars),
	complete_dict_alpha_(NewVars,TmpVars,TmpNames,[0'A],Vars,Names).

:- pred complete_vars_dict_alpha(+Dict,+Vars,-NewDict)
	# "@var{NewDict} is @var{Dict} augmented with the variables of
           the list @var{Vars} not yet in @var{Dict} with alphabetical names.".

complete_vars_dict_alpha(dic(TmpVars,TmpNames),NewVars,dic(Vars,Names)):-
	complete_dict_alpha_(NewVars,TmpVars,TmpNames,[0'A],Vars,Names).

complete_dict_alpha_([],Vars,Names,_,Vars,Names).
complete_dict_alpha_([V|Vs],Vars,Names,N,NewVars,NewNames):-
	new_name_alpha(N,Names,NewName,NewN),
	insert_in_dict(Vars,Names,V,NewName,TmpVars,TmpNames),
	complete_dict_alpha_(Vs,TmpVars,TmpNames,NewN,NewVars,NewNames).

new_name_alpha(N,Names,NewName,NewN):-
	atom_codes(Atom,[0'_|N]),
	( memberchk(Atom,Names) ->
	  next_alpha(N,TmpN),
	  new_name_alpha(TmpN,Names,NewName,NewN)
	;
	  NewName = Atom,
	  next_alpha(N,NewN)
	).

next_alpha(Name,Next):-
	all_are_zs(Name), !,
	length(Name,L),
	L1 is L + 1,
	create_all_as(L1,Next).
next_alpha(Name,Next):-
	reverse(Name,RName), !,
	next_alpha_(RName,RNext),
	reverse(RNext,Next).

next_alpha_([0'Z|R],[0'A|S]) :-
	!, next_alpha_(R,S).
next_alpha_([A|R],[B|R]) :-
	A < 0'Z, !, B is A + 1.

all_are_zs([0'Z]).
all_are_zs([0'Z|R]) :- all_are_zs(R).

create_all_as(0,[]) :- !.
create_all_as(N,[0'A|R]) :-
	N > 0, !,
	N1 is N - 1,
	create_all_as(N1,R).

%-----------------------------------------------------------------------------

:- pred prune_dict(+Term,+Dict,-NewDict)
	# "@var{NewDict} is @var{Dict} reduced to just the variables
           of @var{Term}.".

prune_dict(Term,dic(Vars,Names),dic(NewVars,NewNames)):-
	varset(Term,Vs),
	prune_the_dict(Vars,Names,Vs,NewVars,NewNames).

prune_the_dict([V|Vars],[Name|Names],Vs,[V|NewVars],[Name|NewNames]):-
	ord_member(V,Vs), !,
	prune_the_dict(Vars,Names,Vs,NewVars,NewNames).
prune_the_dict([_|Vars],[_ame|Names],Vs,NewVars,NewNames):-
	prune_the_dict(Vars,Names,Vs,NewVars,NewNames).
prune_the_dict([],[],_Vs,[],[]).

%-------------------------------------------------------------------------

/* not used
merge_dicts(dic(Vars1,Names1),dic(Vars2,Names2),dic(Vars,Names)):-
	merge_dicts0(Vars1,Names1,Vars2,Names2,Vars,Names).

merge_dicts0([V|Vars1],[N|Names1],Vars2,Names2,Vars,Names):-
	insert_in_dict(Vars2,Names2,V,N,Vars3,Names3),
	merge_dicts0(Vars1,Names1,Vars3,Names3,Vars,Names).
merge_dicts0([],[],Vars,Names,Vars,Names).
*/

%-------------------------------------------------------------------------

:- pred rename(Term,Dict) : varnamedict(Dict)
	# "Unifies each variable in @var{Term} with its name in @var{Dict}.
           If no name is found, a new name is created.".

% too expensive
% rename(X,dic(Vars,Names)):- rename(X,Vars,Names).

rename(_X,dic(Vars,Names)):- unify_names(Vars,Names).

unify_names([V|Vars],[N|Names]):-
	(V='$VAR'(N) -> true ; true),
	unify_names(Vars,Names).
unify_names([],[]).

/* not used
rename(X,Vars,Names):-
	var(X),
	find_name(Vars,Names,X,Xn),!,
	X='$VAR'(Xn).
rename(X,_,_):-
	var(X),!.
rename([],_,_):- !.
rename([X|Xs],Vars,Names):- !,
	rename(X,Vars,Names),          
	rename(Xs,Vars,Names).
rename(X,Vars,Names):-
	X=..[_Fun|Args],
	rename(Args,Vars,Names).
*/

:- doc(find_name(Vars,Names,V,Name),"Given that
   @tt{vars_names_dict(Dict,Vars,Names)} holds, it acts as
   @tt{rename(X,Dict)}, but the name of @var{X} is given as 
   @var{Name} instead of unified with it.").

find_name([V|_],[Y|_],X,Xn):-
	X==V, !, 
	Xn=Y.
find_name([_|MoreV],[_|MoreN],X,Xn):-
	find_name(MoreV,MoreN,X,Xn).

%----------------------------------------------------------
:- pred create_pretty_dict(Term,Dict) : term(Term) => varnamedict(Dict)
	# "@var{Dict} has names for all variables in @var{Term}. The 
        difference with @pred{create_dict/2} is that prettier names 
	are generated".

create_pretty_dict(Term,Dict):-
	varsbag(Term,VarsBag,[]),
	varset(Term,Vars),
	remove_one_ocurrence(Vars,VarsBag,NonSingleton_u),
	sort(NonSingleton_u,NonSingleton),
 	copy_term(NonSingleton,NVars),
 	Dict0 = dic(NonSingleton,NVars),
	give_names_(NVars,'A',''),
	ord_subtract(Vars,NonSingleton,NewVars),
	complete_vars_dict(Dict0,NewVars,Dict).
	

remove_one_ocurrence([],VarsBag,VarsBag).
remove_one_ocurrence([V|Vars],VarsBag,NonSingleton_u):-
	remove_one(VarsBag,V,Tmp),
	remove_one_ocurrence(Vars,Tmp,NonSingleton_u).

remove_one([V1|V1s],V,Tmp):-
	V1 == V,!,
	Tmp = V1s.
remove_one([V1|VarsBag],V,[V1|Tmp]):-
	remove_one(VarsBag,V,Tmp).

give_names_([],_,_).
give_names_([V|Vs],'Z',Tail):-!,
	atom_concat('Z',Tail,V),
	(Tail = '' ->
	    NTail = '1'
	;
	    atom_codes(Tail,Codes),
	    number_codes(Num,Codes),
	    Num1 is Num + 1,
	    number_codes(Num1,Codes1),
	    atom_codes(NTail,Codes1)
	),
	give_names_(Vs,'A',NTail).
give_names_([V|Vs],Char,Tail):-
	atom_codes(Char,[Code]),
	atom_concat(Char,Tail,V),
	Code1 is Code + 1,
	atom_codes(Char1,[Code1]),
	give_names_(Vs,Char1,Tail).

:- pred prettyvars(?term, +varnamesl) # "Give names to the variables
	in the term @var{Term} using the dictionary @var{Dict}.
	Intended to replace @pred{prettyvars/1} in those places where
	is possible to get the dictionary of variables.".

prettyvars(Term, Dict) :-
	varnamesl2dict(Dict, VnDict),
	complete_dict(VnDict, Term, VnDict2),
	rename(Term, VnDict2).

%-------------------------------------------------------------------------
% varset_dict(+,-).                                                |
% varset_dict(Dict,Vars)                                           |
%  Collects all the variables appearing in a dictionary (clause)         |
%-------------------------------------------------------------------------

/* not used
varset_dict(Dict,Vars):-
	collect_thevars_dict(Dict,V),
	sort(V,Vars).

collect_thevars_dict([],[]).
collect_thevars_dict([_=V|Dicts],[V|Vars]):-
	collect_thevars_dict(Dicts,Vars).
*/
%-------------------------------------------------------------------------

/* not used
un_number_vars(Term,_NewTerm,_Dict,_Tail) :- 
	var(Term), !,
	fail.
un_number_vars(Term,NewTerm,Dict,Tail) :- 
	Term = '$VAR'(Name), !,
	un_number_vars0(Name,NewTerm,Dict,Tail).
un_number_vars(Term,NewTerm,Dict,Tail) :-
	functor(Term,F,A),
	functor(NewTerm,F,A),
	un_number_vars_inside(A,Term,NewTerm,Dict,Tail).

un_number_vars0(Name,NewTerm,Dict,Tail):-
	var(Name), !,
	NewTerm = Name,
	Dict = Tail.
un_number_vars0(Name,NewTerm,Dict,Tail):-
	Dict = dic([NewTerm|VTail],[Name|NTail]),
	Tail = dic(VTail,NTail).

un_number_vars_inside(0,_,_,Tail,Tail) :- !.
un_number_vars_inside(N,Term,NewTerm,Dict,Tail) :-
	Nth is N-1,
	arg(N,Term,Arg),
	arg(N,NewTerm,NewArg),
	un_number_vars(Arg,NewArg,Dict,Tail0),
	un_number_vars_inside(Nth,Term,NewTerm,Tail0,Tail).

reconstruct_dict(dic(Vars,Names),dic(NewVars,NewNames)):-
	un_number_var_list(Vars,Names,[],[],NewVars,NewNames).

un_number_var_list([T|Vars],[N|Names],Vars0,Names0,NewVars,NewNames):-
	( T='$VAR'(V) ; arg(1,T,'$VAR'(V)) ),
	insert_in_dict(Vars0,Names0,V,N,Vars1,Names1),
	un_number_var_list(Vars,Names,Vars1,Names1,NewVars,NewNames).
un_number_var_list([],[],Vars,Names,Vars,Names).
*/
