:- module(agenda, 
        [
            main/0,
            main_java/0,
            search/4,
            delete/3,
            insert/2,
            exit/0
        ], 
        [
            persdb,
            assertions,
            isomodes,
	    regexp
        ]).

:- use_module(library(dynamic)).
:- use_module(library(aggregates)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(javall/javart)).


:- doc(title, "A simple agenda").
:- doc(author, "Jes@'{u}s Correas").
:- doc(author, "Jos@'{e} Manuel G@'{o}mez P@'{e}rez").
:- doc(author, "Manuel Carro").

:- doc(module, "This is a simple program that shows the features
of the @concept{java interface} and of the @concept{persistent
database}.  The program implements a simple agenda where the user can
search, add, and remove data items.  The core of the functionality is
implemented by the predicates in this file (see the documentation
below).  From the Prolog programmer viewpoint, the state of the agenda
is maintained as facts in the dynamic database, where search,
deletion, and addition is performed.  This state @em{persists}
transparently to the programmer throrugh different invocations of the
program, thanks to the unique persistent database capabilities of Ciao
Prolog.

Java is used to provide a visual interface to the agenda.  The
interface was constructed using an off-the-shelf tool to create
Java-based GUIs, and then adapted so that the buttons in the Java
window activate callbacks in the Prolog side, which perform the
associated actions and return the results.  In order to do this, the
basic operations are made available by exporting them as in a regular
module.  In fact, this module, besides the initial calls to set up the
java interface, can be used as a regular module to manage a persistent
database.  The goal construction and invocation, type conversions,
etc. are performed automatically by the powerful Java interface of Ciao
Prolog.").

:- persistent(agenda_data/5, db).
persistent_dir(db,'./persistent_agenda').

:- data agenda_data/5.

:- concurrent termination/0.


main:-
	main_java,
	retract_fact(termination).

main_java :-
%%   Start Java process.
     java_start("."),
%%   Make this module available to Java
     java_use_module(agenda),
%%   Create Java main form
     java_create_object('PhoneList',Agenda),
%%   Show main form
     java_invoke_method(Agenda,show(_)).




:- doc(doinclude, search/4).

:- pred search(Table, FieldValues, SearchMode, ResultList) # "Searches
the @var{Table} according to the @var{FieldValues} and the
@var{SearchMode}. If @var{SearchMode} is 'normal', then exact match is
performed on any non variable field value; if it is 'reg_exp',
searching is based on regular expressions; and if it is 'case_ins',
case insensitive searching is made.  All the results are given in the
fourth argument as a list of @pred{agenda/5} compound terms.".

search(_, [], _, []) .
search(Table, FieldValues, SearchMode, ResultList) :-
	prepare_search(SearchMode, Table, FieldValues, ResultList).

prepare_search(normal, Table, LValues, LPreds):- %!,
	get_single_query(Table, LValues, Pred),
	findall(Pred, Pred, LPreds).

prepare_search(Search_Mode,Table, LValues, LPreds):-
	get_single_query(Table, LValues, Pred),
	VarList = [_A,_B,_C,_D,_E],
%	get_free_vars(1, 5, VarList),
	PredAux=..[Table|VarList],
	findall(PredAux, PredAux, LPredsAux),
	make_filter(Search_Mode, Pred, LPredsAux, LPreds).

% get_free_vars(N, A, []):- N > A , !.
% get_free_vars(N, A, [V|Vs]):-
% 	var(V),
% 	N1 is N + 1,
% 	get_free_vars(N1, A, Vs).


:- doc(doinclude, make_filter/4).

:- pred make_filter(+SearchMode, +Pred, +LPredsIn, -LPredsOut) #
"Given a search criterium @var{SearchMode}, a pattern @var{Pred}, and
a list of candidates for match @var{LPredsIn}, it obtains a list
@var{LPredsOut} wich match according to that criterium.".


make_filter(case_ins, Pred, LPredsAux, LPreds):- %!,
	filter_cimatch(Pred, LPredsAux, LPreds).
make_filter(reg_exp, Pred, LPredsAux, LPreds):-
	filter_regmatch(Pred, LPredsAux, LPreds).


:- doc(doinclude, filter_cimatch/3).
:- pred filter_cimatch(+Pred, +LPredsIn, -LPredsOut) # "Performs a
case insensitive search for a pattern @var{Pred} in a list of
candidates for match @var{LPredsIn}, and obtains a list
@var{LPredsOut} which match according to that criterium.".


filter_cimatch(_, [], []).
filter_cimatch(Pred, [PredAux1|PAs], [PredAux1|Ps]):-
	push_prolog_flag(case_insensitive,on),
	match_term(Pred, PredAux1), %!,
	pop_prolog_flag(case_insensitive),
	filter_cimatch(Pred, PAs, Ps).
filter_cimatch(Pred, [_PredAux1|PAs], Ps):-
	filter_cimatch(Pred, PAs, Ps).


:- doc(doinclude, filter_regmatch/3).
:- pred filter_regmatch(+Pred, +LPredsIn, -LPredsOut) # "Performs a regular
expresion search for a pattern @var{Pred} in the list of candidates
@var{LPredsIn}, and obtains a list @var{LPredsOut} which match
according to that criterium.".


filter_regmatch(_, [], []).
filter_regmatch(Pred, [PredAux1|PAs], [PredAux1|Ps]):-
	match_term(Pred, PredAux1), %!, 
	filter_regmatch(Pred, PAs, Ps).
filter_regmatch(Pred, [_PredAux1|PAs], Ps):-
	filter_regmatch(Pred, PAs, Ps).

:- doc(doinclude, get_single_query/2).
:- doc(get_single_query/2, " Gets a Prolog query @var{Pred} given
a relation name @var{Table} and its values @var{FieldValues}.").

get_single_query(Table, LFieldValues, Pred):-
	turn2terms(LFieldValues, LFieldTerms),
	Pred=..[Table|LFieldTerms].


:- doc(doinclude, turn2terms/2).
:- pred turn2terms(+Values, -Terms) #
"Converts a list of HTML @var{Values} into a list of Prolog @var{Terms}".

turn2terms([], []).
turn2terms([Value|Values], [Term|Terms]):-
	( empty_value(Value)->
	    var(Term)               %just declarative
        ; 
	    Term=Value
	),
	turn2terms(Values, Terms).

empty_value('').
empty_value('$empty').

empty_list_values([]).
empty_list_values([FieldValue|LFieldValues]):-
        empty_value(FieldValue),
        empty_list_values(LFieldValues).




:- doc(doinclude, delete/3).
:- doc(delete/3, "Receives in @var{FieldValues} the patterns of
the rows to delete from the table.").

delete(_, [], _) .
delete(Table, FieldValues, SearchMode) :-
	prepare_search(SearchMode, Table, FieldValues, LPreds),
	my_retractall(LPreds).

my_retractall([]).
my_retractall([P|Ps]):-
	retract_fact(P),
	my_retractall(Ps).


:- doc(doinclude, insert/3).
:- doc(insert/3, "Adds a new element into the table.").

insert(Table, FieldValues) :-
	get_single_query(Table, FieldValues, Pred),
        ( empty_list_values(FieldValues) ->
            fail
        ; call(Pred) ->
            fail
        ; assertz_fact(Pred)).

exit :-
	java_stop,
	set_fact(termination).
