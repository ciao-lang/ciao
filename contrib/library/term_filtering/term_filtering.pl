:- module(term_filtering,[filter_term/3,filter_term/4,filter_term/5]).

:- use_module(library(lists), [reverse/2, append/3]).

:- data filter_mode/1.

filter_term(Term,Mask,FiltTerm,ModeOrRes) :-
	(ground(ModeOrRes) 
            -> filter_term(Term,Mask,FiltTerm,_Res,ModeOrRes)
	    ; filter_term(Term,Mask,FiltTerm,ModeOrRes,rem)).
filter_term(Term,Mask,FilteredTerm) :- filter_term(Term,Mask,FilteredTerm,_Res).
filter_term(Term,Mask,FilteredTerm,Residue,Mode) :-
	retractall_fact(filter_mode(_)),
	set_fact(filter_mode(Mode)),
	filter_term_(Term,Mask,FilteredTerm,Residue).
	%retractall_fact(filter_mode(_)).

filter_term_(Term,Mask,FilteredTerm,RResidue) :-
	list(Term),
	list(Mask),!,
	filter_args(Term,Mask,[],FilteredArgs,[],RResidue),
	FilteredTerm = FilteredArgs.
	%reverse(RResidue,Residue).
filter_term_(Term,Mask,FilteredTerm,RResidue) :-
	Term =..[F|TermArgs],
	Mask =..[_|MaskArgs],
	filter_args(TermArgs,MaskArgs,[],FilteredArgs,[],RResidue),
	FilteredTerm =..[F|FilteredArgs].
	%reverse(RResidue,Residue).

filter_args([],[],Acu,FilteredArgs,ResAcuR,ResAcu) :- !,
	reverse(Acu,FilteredArgs),reverse(ResAcuR,ResAcu).
filter_args([TArg|TermArgs],[0|MaskArgs],Acu,FilteredArgs,ResAcu,Res) :- 
	current_fact(filter_mode(rem)),!,
	rest_adjustment([0|MaskArgs],TermArgs,NewMaskArgs),
	filter_args(TermArgs,NewMaskArgs,Acu,FilteredArgs,[TArg|ResAcu],Res).
filter_args([TArg|TermArgs],[0|MaskArgs],Acu,FilteredArgs,ResAcu,Res) :- 
	current_fact(filter_mode(gen)),!,
	rest_adjustment([0|MaskArgs],TermArgs,NewMaskArgs),
	filter_args(TermArgs,NewMaskArgs,[_|Acu],FilteredArgs,[TArg|ResAcu],Res).
filter_args([TArg|TermArgs],[1|MaskArgs],Acu,FilteredArgs,ResAcu,Res) :- !,
	rest_adjustment([1|MaskArgs],TermArgs,NewMaskArgs),
	filter_args(TermArgs,NewMaskArgs,[TArg|Acu],FilteredArgs,ResAcu,Res).
filter_args([TArg|TermArgs],[MArg|MaskArgs],Acu,FilteredArgs,ResAcu,Res) :- 
	list(TArg),
	list(MArg),!,
	filter_args(TArg,MArg,[],FilteredSubArgs,[],SubRes),
	FilteredSubTerm = FilteredSubArgs,
	append([SubRes],ResAcu,NewResAcu), % Delete [..] if 1)
	filter_args(TermArgs,MaskArgs,[FilteredSubTerm|Acu],FilteredArgs,
	            NewResAcu,Res).
filter_args([TArg|TermArgs],[MArg|MaskArgs],Acu,FilteredArgs,ResAcu,Res) :- 
	TArg =..[F|SubTermArgs],
	MArg =..[F|SubMaskArgs],!,
	filter_args(SubTermArgs,SubMaskArgs,[],FilteredSubArgs,[],SubRes),
	FilteredSubTerm =..[F|FilteredSubArgs],
	rest_adjustment([MArg|MaskArgs],TermArgs,NewMaskArgs),
	append([SubRes],ResAcu,NewResAcu), % Delete [..] if 1)
	filter_args(TermArgs,NewMaskArgs,[FilteredSubTerm|Acu],FilteredArgs,
	            NewResAcu,Res).
% Old version -> This way we don't support dynamic polymorphic lists. 
%   E.g. filter_term([f(a),r(b)],[;(r(0),f(1))],FH). 
%filter_args([TArg|TermArgs],[MArg|MaskArgs],Acu,FilteredArgs,ResAcu,Res) :- 
%	MArg =..[;|Cases],member(Case,Cases),
%	filter_args([TArg|TermArgs],[Case|MaskArgs],Acu,FilteredArgs,ResAcu,Res).
% New version -> It handles dynamic polymorphic lists. It needs testing. 
%   So, for the moment I won´t activate it, until needed.
filter_args([TArg|TermArgs],[MArg|MaskArgs],Acu,FilteredArgs,ResAcu,Res) :- 
	MArg =..[;|Cases],
	rest_adjustment([MArg|MaskArgs],TermArgs,NewMaskArgs),
	member(Case,Cases),
	filter_args([TArg|TermArgs],[Case|NewMaskArgs],Acu,FilteredArgs,ResAcu,Res).

rest_adjustment([MArg],[_|_RTermArgs],[MArg]):- !.
rest_adjustment([_MArg|MaskArgs],_TermArgs,MaskArgs).
