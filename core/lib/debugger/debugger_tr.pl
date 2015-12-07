:- module(debugger_tr, [srcdbg_expand/5, search_args/6, add_pred_to_list/3,
		add_spy_pred_to_list/3, get_pred_number/3],
	    [assertions, hiord]).

:- use_module(library(write),                  [printable_char/1]).
:- use_module(library(lists),                  [delete/3, length/2]).
:- use_module(library(sets),                   [insert/3]).
:- use_module(library(debugger/debugger_lib), [functor_spec/5]).

:- meta_predicate srcdbg_expand(?, ?, ?, ?, pred(4)).

srcdbg_expand(Old_H, Old_B, New_B, Dict, ExpGoal) :-
	search_head(Old_H, Xs, Dict),
	srcdbg_expand_(Old_B, New_B, Xs, _, ExpGoal).

:- meta_predicate srcdbg_expand_(?, ?, ?, ?, pred(4)).

srcdbg_expand_(','(A, B), ','(NewA, NewB), Xs, Zs, ExpGoal) :- !,
	srcdbg_expand_(A, NewA, Xs, Ys, ExpGoal),
	srcdbg_expand_(B, NewB, Ys, Zs, ExpGoal).
srcdbg_expand_('->'(A, B), '->'(NewA, NewB), Xs, Zs, ExpGoal) :- !,
	srcdbg_expand_(A, NewA, Xs, Ys, ExpGoal),
	add_pred_to_list('->', Ys, Ks),
	srcdbg_expand_(B, NewB, Ks, Zs, ExpGoal).
srcdbg_expand_(';'(A, B), ';'(NewA, NewB), Xs, Zs, ExpGoal) :- !,
	srcdbg_expand_(A, NewA, Xs, Ys, ExpGoal),
	srcdbg_expand_(B, NewB, Ys, Zs, ExpGoal).
srcdbg_expand_(if(A, B, C), if(NewA, NewB, NewC), Xs, Ks, ExpGoal) :-
	!,
	add_pred_to_list('if', Xs, Ts),
	srcdbg_expand_(A, NewA, Ts, Ys, ExpGoal),
	srcdbg_expand_(B, NewB, Ys, Zs, ExpGoal),
	srcdbg_expand_(C, NewC, Zs, Ks, ExpGoal).
srcdbg_expand_(\+(A), \+(NewA), Xs, Ys, ExpGoal) :- !,
	srcdbg_expand_(A, NewA, Xs, Ys, ExpGoal).
srcdbg_expand_(!,     !,    Xs, Xs, _) :- !.
srcdbg_expand_(Goal0, Goal, Xs, Ys, ExpGoal) :-
	ExpGoal(Goal0, Xs, Ys, Goal),
	!.

%% search_head: Add pred to the list and all arguments
search_head(Goal, Xs, Dict) :-
	functor(Goal, Pred, Arity),
	search_args(1, [pred(Pred, 1)], Goal, Arity, Xs, Dict).

%% search_args: Search the whole list of arguments. It calls 
%% search_arg which add individual arguments to the list of strings
search_args(_Number, Xs, _Goal, 0,      Xs, _Dict) :- !.
search_args(Number,  Xs, Goal,  Number, Ys, Dict) :-
	!,
	arg(Number, Goal, Arg),
	search_arg(Arg, Xs, Ys, Dict).
search_args(Number, Xs, Goal, Arity, Ys, Dict) :-
	arg(Number, Goal, Arg),
	search_arg(Arg, Xs, Zs, Dict),
	Next_arg is Number+1,
	search_args(Next_arg, Zs, Goal, Arity, Ys, Dict).

%% search_arg: Add argument to the list. 
% Variables
search_arg(Arg, Xs, Ys, Dict) :-
	var(Arg), !,
	nonvar(Dict),
	name_var_realname(Dict, Arg, RealName),
	add_pred_to_list(RealName, Xs, Ys).
% Strings
search_arg([X|Xs], Ys, Zs, _Dict) :-
	ground([X|Xs]),
	is_string([X|Xs]), !,
	atom_codes(Word, [X|Xs]),
	add_pred_to_list(Word, Ys, Zs).
% Lists
search_arg(.(X, Y), Ys, Zs, Dict) :-
	!,
	search_arg(X, Ys, Ks, Dict),
	search_arg(Y, Ks, Zs, Dict).
% Empty list
search_arg([], Xs, Xs, _Dict) :- !.
% Numbers
search_arg(Arg, Xs, Xs, _Dict) :- number(Arg).
% Atoms
search_arg(Arg, Xs, Ys, _) :-
	nonvar(Arg),
	atom(Arg),
	add_pred_to_list(Arg, Xs, Ys).
% Preds
search_arg(Arg, Xs, Ys, Dict) :-
	functor(Arg, Pred, Arity),
	Arity > 0,
	add_pred_to_list(Pred, Xs, Zs),
	search_args(1, Zs, Arg, Arity, Ys, Dict).

%% name_var_realname
name_var_realname([],               _Var,   '_') :- !.
name_var_realname([Name=Var|_Rest], TheVar, Name) :-
	TheVar==Var, !.
name_var_realname([_|Xs], TheVar, Name) :-
	name_var_realname(Xs, TheVar, Name).

%% Add_pred_to_list
add_pred_to_list(Pred, Xs, Ys) :-
	( member(pred(Pred, OldNumber), Xs) ->
	    delete(Xs, pred(Pred, OldNumber), Zs),
	    NewNumber is OldNumber+1,
	    insert(Zs, pred(Pred, NewNumber), Ys)
	;
	    insert(Xs, pred(Pred, 1), Ys)
	).

%% add_spy_pred_to_list
add_spy_pred_to_list(Arg, Xs, Ys) :-
	functor_spec(Arg, Name, _, _, _),
	add_pred_to_list(Name, Xs, Ys).

% Used when searching in emacs for subexpressions. Not the whole word
get_pred_number(_,    [],                      0).
get_pred_number(Word, [pred(Pred, Number)|Xs], Occur_Number) :-
	count_suffix(Word, Pred, Suffix_Number),
	Count_Number is Number * Suffix_Number,
	get_pred_number(Word, Xs, Rest_Number),
	Occur_Number is Count_Number + Rest_Number.

%count_suffix
count_suffix(Word, Pred, Number) :-
	atom_codes(Word, WordList),
	atom_codes(Pred, PredList),
	find_sublist(WordList, PredList, Number, WordList).

% find_sublist/4
% Arguments: 
%  1.- Word to search
%  2.- List where search for the word.
%  3.- Number of times the word was found.
%  4.- The whole word for recursive search.
find_sublist(_,        [],       0, _) :- !.
find_sublist(WordList, PredList, 0, _) :-
	length(WordList, WordNumber),
	length(PredList, PredNumber),
	WordNumber > PredNumber, !.
find_sublist([], Ys, Number, Word) :-
	find_sublist(Word, Ys, NewNumber, Word), !,
	Number is NewNumber + 1.
find_sublist([X|Xs], [X|Ys], NewNumber, Word) :-
	find_sublist_(Xs, Ys, Number, Rest), !,
	find_sublist(Word, Rest, Rest_Number, Word),
	NewNumber is Number+Rest_Number.
find_sublist([X|Xs], [_|Ys], Number, Word) :-
	find_sublist([X|Xs], Ys, Number, Word), !.

find_sublist_([],     Ys,     1,      Ys) :- !.
find_sublist_([X|Xs], [X|Ys], Number, Zs) :-
	find_sublist_(Xs, Ys, Number, Zs), !.

%is string
is_string([X]) :-printable_char(X).
is_string([X|Xs]) :-
	printable_char(X),
	is_string(Xs).
