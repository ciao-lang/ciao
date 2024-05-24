:- module(menu_json, [], [assertions, fsyntax, dcg]).

% ---------------------------------------------------------------------------
:- doc(section, "Encode menu as JSON (for web environments)").

:- use_module(library(pillow/json), [atomic_to_json_str/2, atomiclst_to_json_strlist/2]).
:- use_module(library(menu/menu_generator), [generate_offline_menu/2]).
:- use_module(library(menu/menu_generator), [decomp_menu_node/3]).

:- export(menu_to_json/2).
:- pred menu_to_json(MenuName, Items)
    # "Encode menu as JSON for web environments".
menu_to_json(MenuName, Items) :-
    generate_offline_menu(MenuName, MenuItems),
    items_to_json(MenuItems, Items).

% From menu_item/7 to JSON representation
items_to_json([], []).
items_to_json([M|Ms], [X|Xs]) :-
    M = menu_item(Menu, Flag, Title, Help, Options, Def_Opt, Guard),
    decomp_menu_node(Menu, Menu0, Level),
    X = json([kind = string("menu_item"),
              menu = ~atomic_to_json_str(Menu0),
              level = ~atomic_to_json_str(Level),
              flag = ~atomic_to_json_str(Flag),
              title = ~atomic_to_json_str(Title),
              help = ~atomic_to_json_str(Help),
              options = ~opts_to_json(Options),
              def_opt = ~atomic_to_json_str(Def_Opt),
              guard = ~or_guard(Guard)]),
    items_to_json(Ms, Xs).    

opts_to_json(ask(A, _)) := R :- !, % TODO: write a different type
    R = ~atomic_to_json_str(A).
opts_to_json(alist(As)) := R :- !, % TODO: write a different type
    atomiclst_to_json_strlist(As, R).
opts_to_json(Options) := _ :- % TODO: incorrect
    throw(error(bad_options(Options), menu_to_json/2)).

or_guard([]) := [] :- !.
or_guard([G|Gs]) := [~and_guard(G)|Xs] :-
    Xs = ~or_guard(Gs).

and_guard([]) := [] :- !.
and_guard([G|Gs]) := [~guard_lit(G)|Xs] :-
    Xs = ~and_guard(Gs).

% TODO: write in a nicer way
guard_lit(F=V) := R :-
    ( V = neq(A) ->
        R = [string("!="), ~atomic_to_json_str(F), ~atomic_to_json_str(A)]
    ; R = [string("=="), ~atomic_to_json_str(F), ~atomic_to_json_str(V)]
    ).

% ---------------------------------------------------------------------------
:- doc(section, "Restore menu flags from JSON string").

:- use_module(library(pillow/json), [string_to_json/2]).
:- use_module(library(menu/menu_generator), [restore_menu_flags_list/1]).

:- export(restore_menu_flags_from_json_str/1).
:- pred restore_menu_flags_from_json_str(S)
    : string(S)
    # "Given a json string @var{S}, extract the flags' values and apply them to the menu".
restore_menu_flags_from_json_str(S) :-
    string_to_json(S, _J),
    menu_values_from_json(_J, _V),
    restore_menu_flags_list(_V).

% ---------------------------------------------------------------------------
:- doc(section, "Extract menu flags' values from JSON").

:- use_module(library(menu/menu_generator), [comp_menu_node/3]).

:- export(menu_values_from_json/2).
:- pred menu_values_from_json(J, F)
    # "From json answer to flag values in the same format required by
       @tt{menu_generator:restore_menu_flags_list/1}".
menu_values_from_json(json(Xs), FlagValuesList) :-
    menu_values_from_json_(Xs, FlagValuesList).

menu_values_from_json_([], []).
menu_values_from_json_([Flag=Args|Fs], [(Menu,Flag,Value)|FVs]) :-
    % TODO: not very nice (decodes menu, flag, and value)
    Args = [string(Menu0),string(Level0),string(Value0)],
    number_codes(Level, Level0),
    atom_codes(Menu1, Menu0),
    comp_menu_node(Menu1, Level, Menu),
    atom_codes(Value, Value0), % TODO: value may be numeric too!
    !,
    menu_values_from_json_(Fs, FVs).
menu_values_from_json_([_|Fs], FVs) :-
    menu_values_from_json_(Fs, FVs).