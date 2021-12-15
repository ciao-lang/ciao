:- module(menu_generator, [
    menu/1,
    menu/2,
    menu/3,
    menu/4,
    exists_menu_flag/2,
    get_menu_flag/3,
    set_menu_flag/3,
    space/1,
    %
    get_menu_configs/1,
    save_menu_config/1,
    remove_menu_config/1,
    restore_menu_config/1,
    show_menu_configs/0,
    show_menu_config/1,
    %
    get_menu_flags/1,
    restore_menu_flags_list/1,
    get_menu_flags/2,
    restore_menu_flags/2,
    %
    generate_offline_menu/2,
    eq/3,
    neq/3,
    uni_type/2,
    vmember/2
  ], [hiord,
      assertions,
      regtypes,
      argnames,
      persdb,
      nortchecks,
      datafacts]).

% TODO: Split this module into interactive menu, offline menu generation, etc.


% ---------------------------------------------------------------------------

:- include(library('menu/menu_common')).

% ---------------------------------------------------------------------------
% TODO: Remove all this hardwired information (must be in auto_interface.pl, preprocess_flags.pl, etc.)
% TODO: as multifile?

% NOTE: (For offline menu generation)
% TODO: as multifile?
opt_to_menu(optimize,         opt). % all_menu_branch
%opt_to_menu(analyze,          ana). % all_menu_branch % TODO: old menu
opt_to_menu(analyze_check,    ana). % all_menu_branch
%opt_to_menu(check_assertions, check). % all_menu_branch % TODO: old menu
opt_to_menu(parallelize,      para). % opt_menu_branch
opt_to_menu(poly_spec,        sp_poly). % opt_menu_branch
opt_to_menu(analyze,          java_ana). % java_all_menu_branch
opt_to_menu(A,                A).

% NOTE: (For offline menu generation)
excluded_flag(menu_config_name).
excluded_flag(menu_last_config).

do_not_ask_flag(main_module).

is_menu_level0(menu_level=naive).
is_menu_level0(menu_java_level=naive).

% ---------------------------------------------------------------------------

:- use_module(library(aggregates)).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(messages)).

:- use_module(library(prompt)).
:- use_module(library(lists), [member/2, select/3]).
:- use_module(library(lists), [reverse/2, append/3, length/2]).

:- data menu_flag/3.

persistent_dir(menudbdir, '~/.ciao.d/menu_flags').

:- persistent(menu_config/2, menudbdir).

%:- multifile pp_flag/2 , 
%            valid_flag_values/2,
%            pp_flag/1.
%
% TODO: Old comment: "There was a bug: One time we tried to create a
%   menu for mecce. We changed some preprocess_flags predicates to
%   multifiles. Then we discover a bug in Ciao, that didnt allow us to
%   execute code form multifiles. So, we have to retract the
%   changes. Unfortunately, I forgot to comment the multifiles lines
%   in menu_generator, also, in preprocess_flags (exporting this
%   "new-multifiles" predicates). When menu_generator was trying to
%   find the possible options of a flag (it uses
%   valid_flags_values/2), the predicate fails (it was not multifile),
%   so it tried with valid_flag_value/2 (note there is no 's' at the
%   end) and a findall... you can imagine the rest."

:- trust pred menu_flag(Menu, Flag, Value) :
    term * atom * term
# "This predicate is internal and stored the menu configuration.
 @var{Menu} is the menu, as defined in @pred{menu_opt/6} and
 @var{Flag} is the flag value stored. @var{Value} is the value
 stored.".

:- trust pred menu_config(Menu, List) =>
    atm * list.
:- trust pred menu_config(Menu, List) :
    atm * list
# "@pred{menu_config/2} is used to store user menu configuration
   permanently.  @var{Menu} is a term that has to correspond with the
   1st argument of @pred{Menu}. @var{List} are the flags saved for
   that menu in the way [flag1=value1, flag2=value2, ...].".

:- trust pred menu_config(Menu, List) :
    atm * var
# "This call mode can be used to ask the @var{List} flag
  configuation of a menu @var{Menu}.".

% ---------------------------------------------------------------------------

:- export(menu_flag_values/1).
:- regtype menu_flag_values(X) # "Flag values".

menu_flag_values(alist(X)) :- list(atm, X).
menu_flag_values(X) :- list(atm, X).
menu_flag_values(ask(T)) :-
    (T == int ; T == nnegint ; T == atom ; T == atm).

% ---------------------------------------------------------------------------
% DEFINITIONS         

:- argnames cc(flag, message, guard, pre, post).

% ---------------------------------------------------------------------------
% THE OUTPUT FUNCTIONS

:- use_module(engine(io_basic)).

:- pred space(N) : num(N)
# "prints @var{N} spaces.".

space(0) :- !.
space(N) :- N > 0, !,
    N1 is N-1,
    display(' '),
    space(N1).
space(_).

display_with_spaces(Label, DesiredLen) :-
    display(Label),
    display(':'),
    atom_length(Label, Len),
    Rest is DesiredLen - Len - 1,
    space(Rest).

mul_display(L) :-
    nmul_display(L, 0, 79).

nmul_display([A|R], Init, Max) :-
    number(A),
    !,
    line_position(user_output, Current),
    %atom_length(A, AL),
    AL = 2,
    (
        AL + Current > Max
    ->
        nl,
        space(Init)
    ;
        true
    ),
    display(user, A),
    nmul_display(R, Init, Max).
nmul_display([A|R], Init, Max) :-
    atom(A),
    !,
    line_position(user_output, Current),
    atom_length(A, AL),
    (
        AL + Current > Max
    ->
        nl,
        space(Init)
    ;
        true
    ),
    display(user, A),
    nmul_display(R, Init, Max).
nmul_display([A|R], _Init, Max) :-
    list(A),
    !,
    line_position(user_output, NInit),
    display('['),
    line_position(user_output, LInit),
    list_display(A, LInit, Max),
    nmul_display(R, NInit, Max).
nmul_display([_|R], Init, Max) :- !,
    nmul_display(R, Init, Max).
nmul_display([], _, _).

list_display([],  _,    _) :- !.
list_display([A], Init, Max) :- !,
    ( atom(A) ->
        atom_length(A, AL)
    ; AL = 1
    ),
    line_position(user_output, Current),
    ( AL + Current + 1 > Max ->
        nl,
        space(Init)
    ; true
    ),
    display(user, A),
    display(user, ']').
list_display([A|R], Init, Max) :- !,
    ( atom(A) ->
        atom_length(A, AL)
    ; AL = 1
    ),
    line_position(user_output, Current),
    ( AL + Current + 2 > Max ->
        nl,
        space(Init)
    ; true
    ),
    display(user, A),
    display(user, ', '),
    list_display(R, Init, Max).

:- pred display_long_atom(A) : atm
# "@var(A) atom is printed in 80 characters-wide.".

display_long_atom(X) :-
    atom(X),
    !,
    atom_codes(X, XC),
    display_long_str(XC).
display_long_atom(X) :-
    display_long_str(X).

display_long_str(XC) :-
    line_position(user_output, NInit),
    display_long_atom_n(XC, NInit, 79).

display_long_atom_n(X, I, Max) :-
    append(Y, [32|XE], X),
    !,
    atom_codes(YC, Y),
    atom_length(YC, AL),
    line_position(user_output, Current),
    (
        AL + Current > Max
    ->
        nl,
        space(I)
    ;
        true
    ),
    display(YC), display(' '),
    display_long_atom_n(XE, I, Max).
display_long_atom_n(X, I, Max) :-
    atom_codes(YC, X),
    atom_length(YC, AL),
    line_position(user_output, Current),
    (
        AL + Current > Max
    ->
        nl,
        space(I)
    ;
        true
    ),
    display(YC).

% ---------------------------------------------------------------------------
% THE MENU ITSELF 

:- set_prolog_flag(multi_arity_warnings, off).

:- pred menu(M) # "Like menu(M,true).".

menu(M) :- menu(M, true).

:- pred menu(M, Bool) # "Like @pred{menu/4} with no selected options,
  taking the menu level from the term @var{M} (example: ana(1) is
  expert, ana is naive), and using @var{Bool} to decide whether print
  help message or not.".

menu(M, B) :-
    norm_menu_node(M, M2),
    M2 =.. [F, L],
    menu(F, L, B).

% Add level to menu, if needed
norm_menu_node(M, M2) :- atom(M), !,
    M2 =.. [M, 0].
norm_menu_node(M, M).

% Decomp menu and level
:- export(decomp_menu_node/3).
decomp_menu_node(M, M0, Level) :- atom(M), !,
    M0 = M, Level = 0.
decomp_menu_node(M, M0, Level) :-
    functor(M, M0, 1),
    arg(1, M, Level).

% Comp menu from level % TODO: needed for menu defs enum, etc.
:- export(comp_menu_node/3).
comp_menu_node(M0, 0, M) :- !, M = M0.
comp_menu_node(M0, Level, M) :-
    functor(M, M0, 1),
    arg(1, M, Level).

:- pred menu(M, Level, Bool) # "Like @pred{menu/4} with no selected
   options.".

menu(A, B, C) :-
    menu(A, B, C, []).

:- pred menu(M, Level, Bool, AlreadySelectedOpts) # "Execute the
menu @tt{X}. @var{Level} specifies the menu level. @var{Bool} decides
whether print the help message. @var{AlreadySelectedOpts} is a list
with the selected options.".

menu(X, Level, Bool, AlreadySelectedOpts) :-
    atom(X),
    select_and_ask_menu(X, Level, Bool, AlreadySelectedOpts, Out),
    reverse(Out, Out2),
    restore_menu_flags(X, Out2).

select_and_ask_menu(X, Level, Bool, AlreadySelectedOpts, Out) :-
    findall(cc${flag => O,
            message => T,
            guard => G,
            pre => PreG,
            post => PG}, %(O,T,G,PreG,PG) , 
        ( menu_opt${menu => PossM,
                flag => O,
                message => T,
                guard => G,
                pre => PreG,
                post => PG},
            menu_match_level(PossM, X, Level) ),
        Menu),
    Menu \== [],
    exec_guard(Menu, AlreadySelectedOpts, NoPosibles, Posibles, []),
    (Bool == true -> display('\n\n\t\t\t(Press h for help)\n\n') ; true),
    ask_menu(Posibles, NoPosibles, X, AlreadySelectedOpts, Out).


:- set_prolog_flag(multi_arity_warnings, on).

% TODO: use norm_menu_node
menu_match_level(M, Functor, Level) :-
    decomp_menu_node(M, Functor, Level0),
    Level0 =< Level.

% If the flag already exists in Entry then we have to remove the
% question
exec_guard([CC|AR], Entry, NoPos, Pos, A) :-
    CC= cc${flag => F},
    member(F=V, Entry),
    nonvar(V),
    !,
    exec_guard(AR, Entry, NoPos, Pos, A).

exec_guard([CC|AR], Entry, NoPos, Pos, A) :-
    CC= cc${guard => G},
    !,
    (
        G(Entry)
    ->
        Pos = [CC|PosR],
        NoPos = NoPosR
    ;
        NoPos = [CC|NoPosR],
        Pos = PosR
    ),
    exec_guard(AR, Entry, NoPosR, PosR, A).

exec_guard([], _, [], A, A).

select_ask_menu(M, L, Result0, Result) :-
    ( select(ask_menu(M), Result0, Result) -> L = 0
    ; select(ask_menu(M, L0), Result0, Result) -> L = L0
    ).

ask_menu(Pos, NoPos, CurrentMenu, Result, Out) :-
    select_ask_menu(M, L, Result, Res),
    !,
    select_and_ask_menu(M, L, false, [changed_to_menu(M)|Res], Result2),
    functor(CurrentMenu, CMF, _),
    ask_menu(Pos, NoPos, CurrentMenu, [changed_to_menu(CMF)|Result2], Out).
%
ask_menu([CC|PR], NoPos, CurrentMenu, Result, Out) :-
    CC = cc${flag => F, message => Label},
    member(F = V, Result),
    nonvar(V),
    !,
    note_message("~w: [~w] Selected.", [Label, V]),
    ask_menu(PR, NoPos, CurrentMenu, Result, Out).
ask_menu([CC|PR], NoPos, CurrentMenu, Result, Out) :-
    CC = cc${flag => F},
    do_not_ask_flag(F),
    !,
    ask_menu(PR, NoPos, CurrentMenu, Result, Out).
%
ask_menu([CC|PR], NoPos, CurrentMenu, Result, Out) :-
%    CC = cc${flag => O, message => Label, pre => PreG, post => PG},
    CC = cc${flag => O, message => Label0, pre => PreG, post => PG},
    % Add flag name to message
    atom_concat(' (', O, O2),
    atom_concat(O2, ')', O3),
    atom_concat(Label0, O3, Label),
    %
    get_menu_options(CurrentMenu, O, OptsListArg),
    get_menu_flag(CurrentMenu, O, Default),
    %
    repeat,
    (PreG -> true ; true),
    ( ground(Default)
    ->
        (
            atom(Default)
        ->
            DEF = Default
        ;
            name(Default, _DEF1),
            atom_codes(DEF, _DEF1)
        ),
        atom_concat(' (', DEF, DD1),
        atom_concat(DD1, ') ? ', DefPrn)
    ;
        DefPrn = '?'
    ),
    (
        ( ( list(OptsListArg), OptsList = OptsListArg
            ; OptsListArg = alist(OptsList) ),
            OptsList \== [] )
    ->
        display_with_spaces(Label, 32),
        mul_display([' ', OptsList, DefPrn])
    ;
        display_with_spaces(Label, 32),
        display(DefPrn)
    ),
    ask_option(OptsListArg, Default, CurrentMenu, Opt),
    (
        Opt == h
    ->
        display('\nHelp for flag '''),
        display(O),
        display(''':'),
        nl,
        ( hook_menu_flag_help(CurrentMenu, O, Help)
        -> true
        ; Help = "No Help Available" ),
        nl,
        display_long_atom(Help),
        nl, nl,
        fail
    ;
        true
    ),
    NewResult_ = [O=Opt|Result],
    PG(NewResult_, NewResult),
    % to caught errors. Should be check( ground( NewResult ) ).
    ground(NewResult),
    exec_guard(NoPos, NewResult, NewNoPos, NewPos, PR),
    ask_menu(NewPos, NewNoPos, CurrentMenu, NewResult, Out),
    !.
%
ask_menu([], _, _, Out, Out).

ask_option([Opt], _, _, Opt) :-
    !,
    display('[Automatically Selected]\n').
%       
ask_option(OptsList, Default, M, Opt) :-
    ( OptsList = ask(int, F) ;
        OptsList = ask(nnegint, F) ;
        OptsList = ask(atom,    F) ;
        OptsList = ask(atm,     F) ),
    !,
    prompt_for_default(Opt, Default),
    hook_menu_check_flag_value(M, F, Opt).
%
ask_option(OptsListArg, Default, _, OptOut) :-
    ( OptsListArg = alist(OptsList)
    ; OptsListArg = [_|_],
        OptsListArg = OptsList ),
    !,
    %% Possible default option,
    prompt_for_default(Opt, Default),
    (
        OptsList == []
    ->
        OptOut = Opt
    ;
        closest_option([h|OptsList], Opt, OptOut)
    ).

% TODO: use fuzzy match (from IG)
closest_option(List, Opt, OptOut) :-
    atom_sub_member(List, Opt, PosibleOut),
    ( check_for_exact_match(PosibleOut, Opt, OptOut) ->
        true
    ; ( PosibleOut == [] ->
          note_message("Incorrect Option", [])
      ; note_message("Please be more specific. Posible Options: ~w", [PosibleOut])
      ),
      fail
    ).

%
% This are the posible cases:
%
% * INPUT:
%   [... aab, aa ... ]  Opt = aa
%   OUTPUT: [aab,aa], as the output is not a list of
%           1 element => we can think it is bad.
%
% SOLUTION: exact match. If there is any option what 
%           exactly match with the Opt, then is it the one selected.
%
atom_sub_member([],     _,   []).
atom_sub_member([A|As], Opt, [A|Os]) :-
    Opt = A,
    !,
    atom_sub_member(As, Opt, Os).
atom_sub_member([A|As], Opt, [A|Os]) :-
    atom(Opt),
    atom_concat(Opt, _, A),
    !,
    atom_sub_member(As, Opt, Os).
atom_sub_member([_|As], Opt, Out) :-
    atom_sub_member(As, Opt, Out).

check_for_exact_match([A], _,   A).
check_for_exact_match(A,   Opt, Opt) :-
    member(Opt, A).

%-----------------------------------------------------------------------------

%% HOOKS-GLUE
%% ----------

:- push_prolog_flag(multi_arity_warnings, off).

:- pred get_menu_options(Menu, Flag, V)
    : (atom(Flag))
# "All possible value sets for Menu and Flag.".

get_menu_options(Menu, Flag, V) :-
    hook_menu_flag_values(Menu, Flag, Values),
    ( Values = ask(A) ->
        V = ask(A, Flag)
    ; V = Values
    ).

:- pop_prolog_flag(multi_arity_warnings).

% ------------------------------------------------------------------------
:- doc(section, "Persistence of Menu Flags").

:- use_module(library(pathnames), [path_split_list/2]).

% Valid menu names must be simple (contain a single path component)
% TODO: other definition?
valid_saved_menu_name(Name) :-
    alphanum_atom(Name),
    !.
valid_saved_menu_name(Name) :-
    note_message("Invalid menu name: '~w' (use only alphanumeric)", [Name]),
    fail.

alphanum_atom(X) :- 
    atom_codes(X, Cs),
    \+ (member(C, Cs), \+ alphanum_code(C)).

alphanum_code(C) :- code_class(C, Class), alphanum_class(Class).

alphanum_class(1). % small letter
alphanum_class(2). % capital letter (including '_')
alphanum_class(3). % digit

%:- entry save_menu_config(Name) : atm.

:- pred save_menu_config(Name) : atm
   # "Save the current flags configuration under the @var{Name} key.".

% Here we have an especification problem. We decided to have 2 (or more)
% menu levels. Basic levels hide some options from the user, while
% advance level show them up. The question is: if we modify a value in
% advance level, do we want that value appear as modified when using
% basic level again?
%
% Example:
% a => 1 , 2 , 3
% a(1) => AA , BB , CC
% b => 10 , 20 , 30
%
% a(1) is the advance option of a.
% by default:
%  * a    is set to 1.
%  * a(1) is set to CC. 
%  * b    is set to 10.
%
%
% if we execute basic level on a 1st instance (everything on default
% values), we will have a = 1 , a(1) = CC, b = 10.
%
% Now, we execute advance menu, and we change a(1) to BB. Again, we
% execute basic level, so a(1) option is not showed. What value does it
% have, CC or BB?

save_menu_config(Name) :-
    get_menu_flags(L),
    % DTM: Not really sure if we want to do this. Read explication.
    save_menu_flags_list(Name, L).

save_menu_flags_list(Name, List) :-
    ( persdb_rt:retract_fact(menu_config(Name, _)),
      fail
    ; true
    ),
    persdb_rt:assertz_fact(menu_config(Name, List)).

:- pred remove_menu_config(Name) : atm
   # "Remove the configuration stored with the @var{Name} key (the
      same provided in @pred{save_menu_config/1}).".

remove_menu_config(Name) :-
    persdb_rt:retract_fact(menu_config(Name, _)),
    fail.
remove_menu_config(_Name).

:- pred restore_menu_config(Name) : atm
   # "Restore the configuration saved with the @var{Name} key (the
      same provided in @pred{save_menu_config/1}).".

restore_menu_config(Name) :-
    menu_config(Name, L),
    restore_menu_flags_list(L).

:- pred get_menu_configs(X) : var(X) => list(atm, X)
   # "Returns a list of atoms in @var{X} with the name of stored
      configurations.".

get_menu_configs(L) :-
    findall(Name, menu_config(Name, _), L).

:- pred show_menu_configs
# "Show all stored configurations.".

show_menu_configs :-
    get_menu_configs(L),
    show_menu_list(L).

show_menu_list([]) :- !.
show_menu_list([A|B]) :-
    write(A), nl,
    show_menu_list(B).

:- pred show_menu_config(C) : atm
   # "Show specific configuration values pointed by @var{C} key (the
      same provided in @pred{save_menu_config/1}).".

show_menu_config(Name) :-
    menu_config(Name, F),
    show_config_list(F).

show_config_list([]).
show_config_list([(A1, A2, A3)|As]) :-
    display('Menu: '),
    display(A1),
    display('   '),
    display(A2),
    display(' = '),
    display(A3), nl,
    show_config_list(As).

:- set_prolog_flag(multi_arity_warnings, off).

% ---------------------------------------------------------------------------
:- doc(section, "Save/Restore Menu Flags (to/from terms)").

:- multifile menu_default/3.
% TODO: make sure that menu_default/3 exists for all flags, add another
% multifile otherwise just to declare the flag
:- pred exists_menu_flag(M, F) : (atm(M), atm(F))
   # "The flag @var{F} in the menu (-branch)
      @var{M} has a default value.".
exists_menu_flag(Menu, F) :-
    functor(Menu, NMenu, _),
    menu_default(NMenu, F, _), !.

:- pred set_menu_flag(M, F, V) : (atm(M), atm(F), var(V))
   # "Set the value @var{V} of the flag @var{F} in the menu (-branch)
      @var{M}.".

set_menu_flag(Menu, F, V) :-
    nonvar(V),
    functor(Menu, NMenu, _),
    (datafacts_rt:retract_fact(menu_flag(NMenu, F, _)) ->true;true),
    datafacts_rt:assertz_fact(menu_flag(NMenu, F, V)).

:- pred get_menu_flag(M, F, V) : (atm(M), atm(F), var(V))
   # "Returns the value in @var{V} of the flag @var{F} in the menu
      (-branch) @var{M}.".

% There is already and option saved
get_menu_flag(Menu, F, V) :-
    functor(Menu, NMenu, _),
    datafacts_rt:current_fact(menu_flag(NMenu, F, Value)),
    !,
    Value = V.
% Lets try with the "flat" value (menu of level 0)
get_menu_flag(Menu, F, V) :-
    functor(Menu, NMenu, _),
    menu_default(NMenu, F, Value),
    !,
    V = Value.
get_menu_flag(Menu, F, V) :-
    hook_menu_default_option(Menu, F, V).

:- pred get_menu_flags(L) : (var(L)) => list(L)
   # "Return a list @var{L} of all current menu flags, composed by
      terms with the form (M,F,V), where M is the menu, F the flag,
      and V the value.  This list can be used as argument of
      @pred{restore_flags_list/1}".

get_menu_flags(L) :-
    findall((AF, B, C),
        ( menu_opt${menu => A, flag => B},
            functor(A, AF, _),
            get_menu_flag(A, B, C) ),
        L).

:- pred get_menu_flags(M, L) : (term(M), var(L)) => list(L)
   # "Return a list @var{L} of the current menu @var{M} composed by
      terms with the form (F=V), F the flag, and V the value.  This
      list can be used as argument of @pred{restore_menu_flags/2}".

get_menu_flags(M, L) :-
    functor(M, MF, _),
    findall((B=C),
        ( menu_opt${menu => A, flag => B},
            functor(A, MF, _),
            get_menu_flag(A, B, C) ),
        L).

:- set_prolog_flag(multi_arity_warnings, on).

:- pred restore_menu_flags_list(L) : list
   # "Restores menu flags. @var{L} is a list of tuple (M,F,V) where M
      is the menu, F is the flag, and V is the value of the flag F in
      the menu M.".

% What happens with non existing flags?
restore_menu_flags_list([]) :- !.
restore_menu_flags_list([(A1, A2, A3)|As]) :-
    (set_menu_flag(A1, A2, A3) ->true;true),
    restore_menu_flags_list(As).

:- pred restore_menu_flags(M, F) : (atm(M), list(F))
   # "Restore the flag of the menu @var{M}. @var{F} is a list of terms
      F=V, which indicate the flag (F) and the value (V). @var{M} is
      the target menu to which those flags \"belong\". Additionally,
      @var{F} can contains terms like @tt{changed_to_menu(NM)} that
      will put NM as the new target menu.".

restore_menu_flags(_Menu, [changed_to_menu(M)|Fs]) :- !,
    restore_menu_flags(M, Fs).
restore_menu_flags(Menu, [F=V|Fs]) :- !,
    set_menu_flag(Menu, F, V),
    restore_menu_flags(Menu, Fs).
restore_menu_flags(_, []).

% ---------------------------------------------------------------------------
:- doc(section, "Generation of offline menu items").

% The offline menu is generated by precomputing a list of menu
% items. Each menu item is a term of the form:
%
%   menu_item(Menu, Flag, Title, Help, Options, Def_Opt, GUARD)
%
% where:
%
%   Menu: term   :: (sub)menu name
%   Flag: atm    :: name of the flag
%   Title: atm   :: description of the flag (in natural language)
%   Help: atm    :: verbose description of the flag (in natural language)
%   Options: menu_flag_values :: possible values
%   Def_Opt:     :: default value
%   GUARD:       :: a list of lists of Flag=Value,
%                   represents the formula (in disjunctive normal form)
%                   for the flag assignments that enables this menu item
%

% TODO: Rewrite the following paragraphs:

% 
% A menu may have several defined submenus (or branches) and
% connection with other menus. For example, the menu:
% 
% @begin{verbatim}
% all, 'Select Menu Level'   # menu_level - naive.
% all, 'Select Action Group' # inter_all  - analyze :: all_menu_branch.
% 
% check(1), 'Perform Compile-Time Checks' # ctcheck - on.
% ...
% 
% ana     , 'Select Aliasing-Mode Analysis' # modes - shfr <- true. 
% @end{verbatim}
% 
% defines three menus: @tt{all}, @tt{check(1)}, and @tt{ana}. When
% generating the guards we have to add additional constraints in order
% to make submenus appear at correct moment. For example, the
% @tt{check(1)} menu will be only active if @tt{menu_level=expert} and
% @tt{inter_all=check_assertions}.
%
% Menus are connected with each others via "post hook" (defined after
% '::' field). The post-processing hook only have to add the element
% @tt{ask_menu(Branch,Level)} or @tt{ask_menu(Branch)} to the selected
% flag list (the argument of the hook). E.g., let us say that when
% calling all_menu_branch(X, Y) we get:
% 
% @begin{verbatim}
% ?- all_menu_branch(A,B).
% 
% A = [inter_all=optimize,menu_level=naive|_A],
% B = [ask_menu(opt,0),inter_all=optimize,menu_level=naive|_A] ? ;
% 
% A = [inter_all=optimize,menu_level=expert|_A],
% B = [ask_menu(opt,1),inter_all=optimize,menu_level=expert|_A] ? ;
% 
% A = [inter_all=analyze,menu_level=naive|_A],
% B = [ask_menu(ana,0),inter_all=analyze,menu_level=naive|_A] ? ;
% 
% A = [inter_all=analyze,menu_level=expert|_A],
% B = [ask_menu(ana,1),inter_all=analyze,menu_level=expert|_A] ? ;
% 
% A = [inter_all=check_assertions,menu_level=naive|_A],
% B = [ask_menu(check,0),inter_all=check_assertions,menu_level=naive|_A] ? ;
% 
% A = [inter_all=check_assertions,menu_level=expert|_A],
% B = [ask_menu(check,1),inter_all=check_assertions,menu_level=expert|_A] ? ;
% 
% A = [inter_all=check_certificate,menu_level=_B|_A],
% B = [inter_all=check_certificate,menu_level=_B|_A] ? ;
% 
% A = [inter_all=optimize,_A,menu_level=naive|_B],
% B = [ask_menu(opt,0),inter_all=optimize,_A,menu_level=naive|_B] ? 
% @end{verbatim}
% 
% Note that the last option contains free variables in the list (not
% taking the tail into account). That is the indicator for us to stop
% searching for more solutions. Additionally, we can have more complex
% things like:
% 
% @begin{verbatim}
% ?- opt_menu_branch(A,B).
% 
% A = [menu_level=naive,inter_optimize=_A|_B],
% B = [ask_menu(_A,0),menu_level=naive,inter_optimize=_A|_B] ? ;
% 
% A = [menu_level=expert,inter_optimize=_A|_B],
% B = [ask_menu(_A,1),menu_level=expert,inter_optimize=_A|_B] ? ;
% 
% A = [menu_level=naive,_B,inter_optimize=_A|_C],
% B = [ask_menu(_A,0),menu_level=naive,_B,inter_optimize=_A|_C] ? 
% @end{verbatim}
% 
% In this situation, the menu that will be asked will depend on the
% value of the flag @tt{inter_optimize}, so we will have to generate as
% many ask_menu as possible values the flag has. The predicate
% @pred{generate_menu_path/2} solves all this problem: for a given flag,
% it looks up to find out the guard composed by flags that will activate
% the flag. 
%
% For example, for the given flag @tt{asr_not_stat_eval}, the path is:
% [menu_level=expert, inter_all=check_assertions], that means that
% @tt{menu_level} flag has to have the value ""expert"" and the
% @tt{inter_all} flag has to have the value ""check_assertions"" (note
% that menu_level and inter_all defines two menu branches). If we
% would execute only the precondition we would get:
% [ctcheck=on].

% TODO: avoid this!!!
is_true_pa('$:'('PAEnv'(true,'PA'(true, _, _)))) :- !.
is_true_pa('$:'('PA'(true, _, _))) :- !. % TODO: deprecate PA without PAEnv

% TODO: cleaner
guard_sols(P, SOL) :-
    findall(X, guard_sols_(P, X), SOL0),
    reverse(SOL0, SOL). % TODO: DO NOT REVERSE!!!

guard_sols_(P, X) :-
    P(X),
    (list(X) -> true ; true). % (close the list)

% TODO: Numeric menu_level instead? (menu_level=naive is
%   menu_level=<0, menu_level=expert is menu_level=<1)

tr_menu_level([],     []).
tr_menu_level([A|As], ARs) :- is_menu_level0(A),
    !,
    tr_menu_level(As, ARs).
tr_menu_level([A|As], [AR|ARs]) :-
    list(A),
    !,
    tr_menu_level(A,  AR),
    tr_menu_level(As, ARs).
tr_menu_level([A|As], [A|ARs]) :-
    tr_menu_level(As, ARs).

:- meta_predicate get_branch_sols(pred(2), ?, ?).
:- pred get_branch_sols(P, ASK, REST)
# "For a predicate @var{P} obtained from the argument @tt{post}
      of multifile @var{menu_opt/7}, it returns the concatenation with
      another menu branch, @var{ASK}, and the flag restrictions
      (values) needed for calling it in @var{REST}.".

% TODO: Is this just precomputing P?
get_branch_sols(P, ASK, REST) :-
    P(REST, B),
    (
        is_list_with_no_vars(REST),
        is_list_with_no_vars(B)
    ->
        select_ask_menu(M, L, B, BRest),
        ( nonvar(M) -> % Menu is given
            trace(no_opt_to_menu(L, M))
        ; % Menu is bound by a flag in BRest
            trace(look_branch_sol(P, ASK, REST, B)),
            member(Flag=MM, BRest), MM==M, % (same var)
            %
            trace(look_branch_sol_2(P, ASK, REST, B)),
            get_menu_options(_, Flag, Opts2), % NOTE: any Menu!
            trace(get_menu_options(Flag, Opts2)),
            Opts2 = alist(Opts),
            member(M1, Opts),
            opt_to_menu(M1, M), % (nondet)
            trace(look_opt_to_menu(L, M1, M))
        ),
        ASK =.. [M, L]
    ;
        !,
        fail
    ).

is_list_with_no_vars([]).
is_list_with_no_vars([A|B]) :-
    nonvar(A),
    is_list_with_no_vars(B).

enum_menu_opt(Menu, Flag, Title, Guard, BeforePrint, AfterSel) :-
    menu_opt(Menu, Flag, Title, Guard, BeforePrint, AfterSel),
    \+ excluded_flag(Flag).

:- pred generate_offline_menu(EntryMenu, MenuItems)
    : (term(EntryMenu), term(MenuItems))
# "Generate an offline menu @var{MenuItems} for the given @var{EntryMenu}".

% TODO: Use _EntryMenu (hardwired to 'all')
generate_offline_menu(EntryMenu, MenuItems) :-
    datafacts_rt:retractall_fact(menu_path(_, _, _)),
    add_entry_menu_path(EntryMenu),
    generate_menu_path_offline,
    generate_menu_path_offline_kleene,
    findall(menu_opt(Menu, Flag, Title, Guard, BeforePrint, AfterSel),
        enum_menu_opt(Menu, Flag, Title, Guard, BeforePrint, AfterSel),
        MenuOpts),
    menu_opt_to_menu_items(MenuOpts, MenuItems0),
    filter_menu_items(MenuItems0, MenuItems).

% menu_path(Menu, Guard, _): Guard must be active in order to reach Menu
:- data menu_path/3. % TODO: why third argument?

add_entry_menu_path(M) :-
    norm_menu_node(M, M2),
    datafacts_rt:assertz_fact(menu_path(M2, [], no)).

% (fill menu_path/3)
generate_menu_path_offline :-
    ( % (failure-driven loop)
        enum_menu_opt(_Menu, _Flag, _Title, _Guard, _BeforePrint, AfterSel),
        trace(off__menu_opt(_Menu, _Flag, _Title, _Guard, AfterSel)),
        get_branch_sols(AfterSel, A, B0),
        tr_menu_level(B0, B),
        % trace(get_branch_sols(AfterSel, A, B)),
        trace(menu_path__0(A, B)),
        datafacts_rt:assertz_fact(menu_path(A, B, no)),
        % trace(menu_path(A,B)),
        fail
    ; true
    ).

generate_menu_path_offline_kleene :-
    % Take A<-Path0
    datafacts_rt:current_fact(menu_path(A, Path0, no), Ref),
    trace(menu_path__iter(A, Path0)),
    %
    % Look Flag which node has which flags
    member(Flag=_, Path0),
    enum_menu_opt(Node, Flag, _Title, _Guard, _BeforePrint, Post),
    trace(menu_path__m(Node, Flag, Post)),
    trace(menu_path__post(Post)),
    \+ is_true_pa(Post), % check that it is a node
    %
    % Get the path to that node
    generate_menu_path(Node, Path),
    datafacts_rt:erase(Ref),
    \+ Path = [fail], % TODO: OK?
    append(Path0, Path, BPath),
    datafacts_rt:assertz_fact(menu_path(A, BPath, yes)),
    trace(menu_path__first(A, Path0)),
    trace(menu_path__node(Node)),
    trace(menu_path__then(A, BPath)),
    fail.
generate_menu_path_offline_kleene :-
    datafacts_rt:retract_fact(menu_path(A, Path, no)),
    datafacts_rt:assertz_fact(menu_path(A, Path, yes)),
    %       trace(keep_menu_path(A, B)),
    fail.
generate_menu_path_offline_kleene.

generate_menu_path(M, Path) :-
    norm_menu_node(M, M2),
    generate_menu_path_(M2, Path).

generate_menu_path_(M, Path) :-
    menu_path(M, Path, _),
    !.
generate_menu_path_(M, Path) :-
    Path = [fail],
    % display(user_error, 'MENU ENTRY IS UNREACHABLE'(M)), nl(user_error),
    trace(unreachable_menu(M)).

% TODO: generate_menu_path_offline should be a fixpo?

menu_opt_to_menu_items([],     []).
menu_opt_to_menu_items([M|Ms], [J|Js]) :-
    menu_opt_to_menu_item(M, J),
    menu_opt_to_menu_items(Ms, Js).

% From menu_opt to menu_item (obtain the guards that activate the flag)
menu_opt_to_menu_item(M, J) :-
    % TODO: Use _BeforePrint?
    % TODO: keep Menu
    M = menu_opt(Menu, Flag, Title, Guard, _BeforePrint, _AfterSel),
    J = menu_item(Menu, Flag, Title, Help, Options, Def_Opt, GUARD),
    (hook_menu_flag_help(Menu, Flag, Help) -> true ; Help = ''),
    % Generate the path to see how to get this menu
    generate_menu_path(Menu, Path),
    % Options
    get_menu_options(Menu, Flag, Options),
    % Default Options
    get_menu_flag(Menu, Flag, Def_Opt),
    % Generate the guard expression (conjunction of menu path and flag guard)
    guard_sols(Guard, IG),
    ( Path = [fail] ->
        GUARD = [] % (fail)
    ; dnf_and(IG, Path, GUARD)
    ).

% Remove menu items with false guard
filter_menu_items([],     []).
filter_menu_items([M|Ms], Ns) :-
    M = menu_item(_Menu, _Flag, _T, _Help, _O, _DefOpt1, G1),
    ( G1 = [] -> Ns = Ns2 % guard always fail!
    ; Ns = [M|Ns2]
    ),
    filter_menu_items(Ms, Ns2).

% :- export(dnf_and/3).
% (A1\/A2\/...\/An) /\ B ===> (A1/\B) /\ (A2/\B) /\ ... /\ (An/\B)
%
% (+A::dnf, +B::conj, -C::dnf)
dnf_and([],     _, []) :- !.
dnf_and([A|As], B, Cs) :-
    append(A, B, C0),
    simpl_conj(C0, C), % TODO: do later?
    ( C = [fail] -> Cs = Cs0
    ; Cs = [C|Cs0]
    ),
    dnf_and(As, B, Cs0).

% simplify a conjunction of F=V flag assignments
% (F=V1 and F=V2 imply V1=V2)
simpl_conj(As, Bs) :-
    (simpl_conj_(As, Bs0) -> Bs = Bs0 ; Bs = [fail]).

simpl_conj_([],     []).
simpl_conj_([A|As], Bs) :-
    A = (Flag=V),
    member((Flag=V0), As),
    !,
    V == V0, % (fail otherwise)
    simpl_conj_(As, Bs).
simpl_conj_([A|As], [A|Bs]) :-
    simpl_conj_(As, Bs).

% ---------------------------------------------------------------------------
% Auxiliary for writing the guards

:- pred eq(Type, A, B)
# "@var{Type} is the value returned by the 2nd arg of
  @pred{uni_type}. @var{A} and @var{B} are whatever terms. This
  predicate success if they are equal (like A=B).".

eq(true,  V, V).
eq(false, Y, V) :- Y == V.

:- pred neq(Type, A, B)

# "@var{Type} is the value returned by the 2nd arg of
  @pred{uni_type}. @var{A} and @var{B} are whatever terms. The
  semantic is similar to A \== B.".

neq(true,  [true|_], []) :- !.
neq(false, Y,        []) :- !, Y \== [].
neq(true,  neq(V),   V).
neq(false, Y,        V) :- Y \== V.

:- pred uni_type(Var, Type)

# "@var{Var} should be the argument passed to the menu
  guard. @var{Type} is an abstract type that decides how unifications
  should be done in @pred{eq/3} and @pred{neq/3}.".

uni_type(X, true) :-
    (var(X) ; member(Y, X), var(Y)),
    !.
uni_type(_, false).

% val(K,V,[K=V1|_]) :-
%       ( V == V1 -> ! ; V = V1 ).
% val(K,V,[I|Is]) :-
%       nonvar(I),
%       val(K,V,Is).

:- pred vmember(Var, List)
# "It is @pred{member} equivalent predicate to be used in guards.".

vmember(K=neq(V), [K=V1|_]) :-
    V == V1,
    !,
    fail.
vmember(K=V, [K=V1|_]) :-
    nonvar(V1),
    V1 = neq(A),
    A == V,
    !,
    fail.
vmember(K=V, [K=V1|_]) :-
    (V == V1 -> ! ; V = V1).
vmember(K, [I|Is]) :-
    nonvar(I),
    vmember(K, Is).

% ---------------------------------------------------------------------------

% TODO: Uncomment to debug
trace(_).
%trace(X) :-
%       display(X), nl.
 
