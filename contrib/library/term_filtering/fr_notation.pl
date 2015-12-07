
:- module(fr_notation,[term_to_fr/3,fr_to_term/3,testFR/4]).

:- use_module(library(term_filtering)).

:- use_module(library(lists), [reverse/2]).

testFR(T,M,FR,Term) :- term_to_fr(T,M,FR), fr_to_term(FR,M,Term).

term_to_fr(Term,Mask,(F,R)) :-
	filter_term(Term,Mask,F,R,rem).

fr_to_term((F,R),Mask,Term) :-
	list(F),list(Mask),!,
	fr_to_term_args(F,R,Mask,[],Term,[]).
fr_to_term((F,R),Mask,Term) :-
	F =..[FFunc|ArgsF],
	Mask =..[_|MaskArgs],
	fr_to_term_args(ArgsF,R,MaskArgs,[],OutArgs,[]),
	Term =..[FFunc|OutArgs].

fr_to_term_args([],[],[],TermAcu,Term,_Res) :- 
	!,reverse(TermAcu,Term).
fr_to_term_args(Fs,[R1|Res],[0|MaskArgs],TermAcu,Term,RestRes) :- 
	!,rest_adjustment([0|MaskArgs],Fs,Res,NewMaskArgs),
	fr_to_term_args(Fs,Res,NewMaskArgs,[R1|TermAcu],Term,RestRes).
fr_to_term_args([F1|Fs],Res,[1|MaskArgs],TermAcu,Term,RestRes) :- 
	!,rest_adjustment([1|MaskArgs],Fs,Res,NewMaskArgs),
	fr_to_term_args(Fs,Res,NewMaskArgs,[F1|TermAcu],Term,RestRes).
fr_to_term_args([F1|Fs],[R1|Res],[M1|MaskArgs],TermAcu,Term,OutRes) :- 
	list(F1),
	list(M1),!,
	fr_to_term_args(F1,R1,M1,[],OriginalArgs,_RestRes),
	OrigF1 = OriginalArgs,
	rest_adjustment([M1|MaskArgs],Fs,Res,NewMaskArgs),
	fr_to_term_args(Fs,Res,NewMaskArgs,[OrigF1|TermAcu],Term,OutRes).
fr_to_term_args([F1|Fs],[R1|Res],[M1|MaskArgs],TermAcu,Term,OutRes) :- 
	F1 =..[F1Func|F1Args],
	M1 =..[F1Func|M1Args],!,
	fr_to_term_args(F1Args,R1,M1Args,[],OriginalArgs,_RestRes),
	OrigF1 =..[F1Func|OriginalArgs],
	rest_adjustment([M1|MaskArgs],Fs,Res,NewMaskArgs),
	fr_to_term_args(Fs,Res,NewMaskArgs,[OrigF1|TermAcu],Term,OutRes).
fr_to_term_args([F1|Fs],[R1|Res],[M1|MaskArgs],TermAcu,Term,OutRes) :- 
	M1 =..[;|Cases],member(Case,Cases),
	fr_to_term_args([F1|Fs],[R1|Res],[Case|MaskArgs],TermAcu,Term,OutRes).

rest_adjustment([MArg],[_|_RTermArgs],_,[MArg]):- !.
rest_adjustment([MArg],_,[_|_Res],[MArg]):- !. 
rest_adjustment([_MArg|MaskArgs],_TermArgs,_Res,MaskArgs).	

% There will be cases(*) in which fr_to_term will fail 
% (*)Rest adjustments with 0's.
% Solution1: Adjust also w.r.t. residue.
% Solution2: Return residue in an structured way E.g. [[b,c],5]
