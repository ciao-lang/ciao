 %% trie.pl -- implementacion de un indexador de palabras en 
 %%            ficheros usando tries
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro
 %% Created On      : Fri Jun 13 10:30:13 1997
 %% Last Modified By: MCL
 %% Last Modified On: Tue May  9 11:41:39 2000
 %% Update Count    : 141
 %% Status          : Tested and approved!




 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%
 %% Notes:
 %%
 %% I tried not to use library predicates as long as I can define them.  
 %% There is a lot of room improvemente readabilitywise.
 %%
 %% Green cuts in many places.
 %%
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(trie, [consult/3]).

:- use_module(library(write)).
:- use_module(library(streams)).

:- set_prolog_flag(multi_arity_warnings, off).

 %% Trie:
 %% [final(const, [fichero], [trie])]
 %% [trie(const, [trie])]
 %% []
 %%

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% empty_trie(T): T is an empty trie
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_trie([]).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% trie_inserta(Trie, Word, Info, NewTrie): NewTrie
 %% contains all the information in Trie, plus the association of
 %% Info to Word
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 %% Insertion in empty trie
trie_insert([], [C1,C2|List], Fich, [trie(C1, Trie)]):- !,  %% Verde
        trie_insert([], [C2|List], Fich, Trie).
trie_insert([], [C], Fich, [final(C, [Fich], [])]).

 %% Insertion in nonempty trie
trie_insert([Trie|Rest], [C|Cs], F, [NTrie|Rest]):-
        first_arg(Trie, C), !,                               %% Verde
        trie_insert_branch([C|Cs], F, Trie, NTrie).
trie_insert([Trie|Rest], [C1|Cs], F, [Trie|NRest]):-
        first_arg(Trie, C),
        C \== C1,
        trie_insert(Rest, [C1|Cs], F, NRest).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% trie_insert_branch(Word, Info, TrieBranch, NewTrieBranch):
 %% Word starts in TrieBranch and NewTrieBranch associates that
 %% word to Info.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trie_insert_branch([C, C1|CL], Fich, trie(C, TrList), trie(C, NTrList)):-!,
        trie_insert(TrList, [C1|CL], Fich, NTrList).
trie_insert_branch([C,C1|CL], Fich, final(C,Fs,TrList), final(C,Fs,NTrList)):-!,
        trie_insert(TrList, [C1|CL], Fich, NTrList).
trie_insert_branch([C], Fich, trie(C, TrList), final(C, [Fich], TrList)):-!.
trie_insert_branch([C], Fich, final(C, Fs, TrList), final(C, NFs, TrList)):-
        insert_sorted(Fs, Fich, NFs).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% insert_sorted(SortedList, Item, NewSortedList): NewSortedList is a 
 %% sorted list without repetitions which contains all the elements
 %% in SortedList plus Item.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


insert_sorted([], Fich, [Fich]).
insert_sorted([F1|Fs], F, [F1|F1s]):- 
        F1 @< F, !,                              
        insert_sorted(Fs, F, F1s).
insert_sorted([F|Fs], F, [F|Fs]):- !.            
insert_sorted([F1|Fs], F, [F,F1|Fs]):- F1 @> F, !.



 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% trie_multi_insert(WordList, Info, Trie, NewTrie): associates
 %% each word in WordList to Info in Trie, returning NewTrie.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trie_multi_insert([], _F, T, T).
trie_multi_insert([Word|WFs], File, T1, T3):-
        trie_insert(T1, Word, File, T2),
        trie_multi_insert(WFs, File, T2, T3).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% trie_lookup(Word, Trie, InfoList): the list of information items
 %% InfoList is associated to Word in Trie.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trie_lookup([C], Tries, Fichs):-          %% Need a final node
        get_trie(C, Tries, final(C, Fichs, _OtherTries)).
trie_lookup([C,C1|Cs], Tries, Fichs):-    %% Need a non-final node
        get_trie(C, Tries, TrieAndSons),
        (TrieAndSons = trie(_, Trie); TrieAndSons = final(_, _, Trie)),
        trie_lookup([C1|Cs], Trie, Fichs).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% get_trie(Char, Trie, TrieBranch): TrieBranch in Trie starts by Char
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_trie(C, [Trie|_Tries], Trie):-
        first_arg(Trie, C).
get_trie(C1, [NoTrie|Tries], Trie):-
        first_arg(NoTrie, C),
         C1 \== C,
         get_trie(C1, Tries, Trie).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% first_arg(TrieNode, Char): TrieNode starts a word which itself 
 %% starts with Char.  We could just use arg/3.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

first_arg(final(C, _, _), C).
first_arg(trie(C, _), C).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% index(Files, Trie): Trie indexes the words in a list of Files
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

index(Fichs, T):-
        empty_trie(T0),
        index(Fichs, T0, T).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% index(Files, Trie, NewTrie): Words in File are added to 
 %% Trie, returning NewTrie.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

index([], T, T).
index([F|Fs], T0, T):-
        read_file(F, Words),
        trie_multi_insert(Words, F, T0, T1),
        index(Fs, T1, T).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% read_file(File, Words): Words are in File.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_file(F, Words):-
        open(F, read, Stream),
        set_input(Stream),
        word_list(Words),
        close(Stream).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% word_list(Words): Words is the list of words reading from 
 %% the standard stream_input
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

word_list(Words):-
        get_alpha_or_eof_char(Char),
        word_list(Char, Words).

word_list(-1, []):- !.              %% Verde
word_list(Char, [P|Ps]):-
        Char > -1,
        read_word(Char, NewChar, P),
        word_list(NewChar, Ps).
        

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% read_word(StartChar, FinalChar, word)
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_word(Char, Char, []):- Char = -1, !.  %% Verde
read_word(Char, NewChar, [AtomChar|Chars]):-
        is_alpha(Char), !,                   %% Verde
        char_code(AtomChar, Char),
        get_code(NextChar),
        read_word(NextChar, NewChar, Chars).
read_word(Char, NewChar, []):- 
        \+ is_alpha(Char),
        get_alpha_or_eof_char(NewChar).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% get_alpha_or_eof_char(Char)
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_alpha_or_eof_char(Char):-
        repeat,
          get_code(Char),
          (is_alpha(Char); Char = -1), 
        !.


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% is_alpha(X): 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_alpha(X):- X >= 0'a, X =< 0'z, !.     %% Verde
is_alpha(X):- X >= 0'A, X =< 0'Z, !.     %% Verde
is_alpha(0'_).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% consult(Words, Files, Where)
 %%
 %% [Clp(H)] ?- consulta([is,mas,or,mucha,trie,consulta,queens,consulta_],
 %%                      [f,g,'queens_standard.pl','trie.pl'], T). 
 %%
 %% T = [is-[queens_standard.pl], mas-[f,trie.pl], or-no_encontrada,
 %%       mucha-[g], trie-[trie.pl], consulta-[trie.pl],
 %%       queens-[queens_standard.pl], consulta_-[trie.pl]] ? 
 %%
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


consult(Words, Files, Where):-
        index(Files, Trie),
        consult_(Words, Trie, Where).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% consulta_(words, Trie, Where)
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consult_([], _T, []).
consult_([P|Ps], Trie, [P-F|Files]):-
        atom_to_atoms_list(P, PAtoms),
        (
            trie_lookup(PAtoms, Trie, F) ->
            true
        ;
            F = no_encontrada
        ),
        consult_(Ps, Trie, Files).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% to_atoms(Caracteres, Atomos)
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

atom_to_atoms_list(Atom, AtomList):- atom_chars(Atom, AtomList).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% print_index(Files).
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prindt_index(Fs):-
        index(Fs, Trie),
        trie_lookup(AtomsList, Trie, Fichs),
        atom_to_atoms_list(Atom, AtomsList),
        write(Atom : Fichs), nl,
        fail.
print_index(_Fs).
