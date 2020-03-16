:- module(ciaodump, [main/1], [assertions, hiord]).

:- doc(title,"Display information about Ciao object files").

:- doc(author,"Manuel Hermenegildo").
:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Carro").
:- doc(author,"Jose F. Morales").

:- doc(module,"This program outputs in symbolic form the information
   contained in a @concept{Ciao object file} produced by the Ciao
   compiler. This information includes:

   @begin{itemize}
   @item declarations and code after compiler's pass one:
     @concept{assertion information} (@concept{predicate
     declarations}, @concept{property declarations}, @concept{type
     declarations}, etc.) and @concept{code-related
     information} (@concept{imports}, @concept{exports},
     @concept{libraries used}, etc.)
   @item contents of a module interface file (@tt{.itf})
   @item contents of a bytecode file (@tt{.po}), include WAM bytecode
   @item plain terms in @lib{fastrw} format
   @end{itemize}

   This program is specially useful for example for checking what
   assertions the @concept{assertion normalizer} is producing from the
   original assertions in the file, what the compiler is actually
   seeing after some of the syntactic expansions (but before goal
   translations), or what bytecode instructions are generated.

   @section{Usage}

   @begin{verbatim}
@includefact{usage_text/1}
   @end{verbatim}
").

:- use_module(engine(messages_basic), [message/2]).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(read)).  
:- use_module(library(fastrw)).  
:- use_module(library(format)).  
:- use_module(library(aggregates)).  

:- use_module(library(compiler/c_itf)).
:- use_module(library(assertions/assrt_lib), [
    cleanup_code_and_related_assertions/0,
    assertion_read/9,
    clause_read/7,
    get_code_and_related_assertions/5,
    assertion_body/7,
    print_assertions/1
   ]).
:- use_module(library(assertions/assrt_write)).
:- use_module(library(compiler), [make_wam/1]).

:- use_module(engine(runtime_control), [set_prolog_flag/2, prolog_flag/3]).
:- use_module(engine(internals)).
:- use_module(library(system)).
:- use_module(library(stream_utils), [open_input/2, close_input/1]).
:- use_module(library(write), [numbervars/3, writeq/1]).
:- use_module(library(read)).

:- use_module(library(pathnames), [path_split/3]).

% ---------------------------------------------------------------------------

usage_text("ciaodump <options> <filename>

where the possible options are:

  -h
     Print this information

  -itf <filename>[.itf]
     Print .itf contents in symbolic form

  -asr0 <filename>[.asr]
     Print .asr contents in symbolic form (raw form)

  -asr <filename>[.asr]
     Pretty print .asr contents in symbolic form

  -fastrw <filename>
     Decode a file written with the fastrw library

  -norm [-v] [-m] <-a|-f|-c|-e> <filename>[.pl]
     Print output of compiler's pass one

     -v : verbose output (e.g., lists all files read)
 
     -m : restrict info to current module (omit imported)
 
     -a : print assertions (only code-oriented assertions -- not
      comment-oriented assertions), after normalization
 
     -f : print declarations, code, and interface (imports/exports, etc.)
      after compiler's pass one
 
     -c : print code only
 
     -e : print only errors - useful to check syntax of assertions in file
 
     <filename> must be the name of a Ciao source file.
 
  -po <filename>[.po]
     Print .po contents in symbolic form

  -wam <filename>[.pl]
     Print WAM code (simplified) for a .pl file
").

% TODO: '-f' could be added to ciaoc (like ciaoc -w) -- optimcomp does
%   it in some form
% TODO: '-norm' is not a good name (it is called 'split' in optimcomp, which
%   is not a good name either)

% ---------------------------------------------------------------------------

main(Args) :-
    handle_args(Args).

% ---------------------------------------------------------------------------

handle_args(['-h']) :- !,
    usage.
handle_args(['-itf', File]) :- !, view_itf(File).
handle_args(['-asr0', File]) :- !, view_asr0(File).
handle_args(['-asr', File]) :- !, view_asr(File).
handle_args(['-fastrw', File]) :- !, view_fastrw(File).
handle_args(['-norm'|IArgs]) :- !,
    (  IArgs = ['-v'|Args]
    -> prolog_flag(verbose_compilation,_,on)
    ;  prolog_flag(verbose_compilation,_,off),
       Args=IArgs ),
    ( Args = [Opt1,Opt2,Main], 
      Opt1 = '-m' 
    ; Args = [Opt2,Main] ),
    ( Opt2 = '-a' ; Opt2 = '-c' ; Opt2 = '-f' ; Opt2 = '-e' ),
    !,
    (  prolog_flag(verbose_compilation,on,on)
    -> format("{Printing info for ~w}~n",[Main])
    ;  true ),
    cleanup_code_and_related_assertions,
    get_code_and_related_assertions(Main,M,Base,_Suffix,_Dir),
    (  Opt1 == '-m' 
    -> DM = M, DBase = Base
    ;  true),
    handle_options(Opt2,DM,DBase),
    true.
handle_args(['-po', File]) :- !, view_ql(File).
handle_args(['-wam', File]) :- !, view_wam(File).
handle_args(Args) :-
    format("error: invalid arguments ~w~n",[Args]),
    usage.

usage :-
    usage_text(Text),
    format(user_error,"Usage: ~s~n",[Text]).

% ---------------------------------------------------------------------------

handle_options('-a',M,_Base) :-
    !,
    prolog_flag(write_strings, Old, on),
    print_assertions(M),
    set_prolog_flag(write_strings, Old).
handle_options('-f',M,Base) :-
    !,
    prolog_flag(write_strings, Old, on),
    print_gathered_module_data(M,Base),
    set_prolog_flag(write_strings, Old).
handle_options('-c',M,Base) :-
    !,
    prolog_flag(write_strings, Old, on),
    print_code_only(M,Base),
    set_prolog_flag(write_strings, Old).
handle_options('-e',_M,_Base).

% ---------------------------------------------------------------------------

view_itf(File) :-
    find_filename('.itf', File, AbsName),
    open_input(AbsName, InState),
    read(v(V,Format)),
    message(user, ['Version: ',V]),
    message(user, ['Format: ',Format]),
    repeat,
      do_read(Format,ITF),
    ( ITF = end_of_file, !
    ; display(ITF), nl,
      fail
    ),
    close_input(InState).

do_read(f,Term) :- fast_read(Term), ! ; Term = end_of_file.
do_read(r,Term) :- read(Term).

% ---------------------------------------------------------------------------

view_fastrw(File):-
    open_input(File,OldInput),
    show_fast_read,
    close_input(OldInput).

% ---------------------------------------------------------------------------

view_asr0(File) :-
    find_filename('.asr', File, AbsName),
    open_input(AbsName,OldInput),
    read(Vstruct),
    arg(1,Vstruct,V),
    message(user, ['Version ',V]),
    prolog_flag(write_strings, Old, on),
    show_fast_read,
    set_prolog_flag(write_strings, Old),
    close_input(OldInput).

show_fast_read :-
    ( fast_read(R) ->
        \+ \+ ( numbervars(R, 0, _N), writeq(R), nl ), 
        show_fast_read
    ; true
    ).

% ---------------------------------------------------------------------------

view_asr(File) :-
    find_filename('.asr', File, AbsName),
    open_input(AbsName,OldInput),
    read(Version),
    format("Normalizer version: ~w~n",[Version]),
    prolog_flag(write_strings, Old, on),
    read_asr_data_loop,
    set_prolog_flag(write_strings, Old),
    close_input(OldInput).

read_asr_data_loop :-
    ( fast_read(X) ->
        process_assrt(X),
        read_asr_data_loop
    ; true
    ).

process_assrt(assertion_read(PD,_M,Status,Type,Body,Dict,_S,_LB,_LE)) :- 
    !,
    write_assertion(PD,Status,Type,Body,Dict,status).
process_assrt(clause_read(_Base,H,B,Dict,_S,_LB,_LE)) :- 
    !,
    unify_vars(Dict),
    format("Clause ~w :- ~w.\n",[H,B]).
process_assrt(X) :- 
    format("*** Warning: ~w is not an assertion~n",[X]).

unify_vars([]).
unify_vars([N=V|Dict]):-
    V='$VAR'(N),
    unify_vars(Dict).

% ---------------------------------------------------------------------------
% % (limited form of print_assertions)
% 
% show_asrts(Alias) :-
%       cleanup_code_and_related_assertions,
%         absolute_file_name(Alias, '_opt', '.pl', '.', FileName, Base, AbsDir),
%         path_split(Base, AbsDir, Module),
%       get_code_and_related_assertions(FileName, Module, Base, '.pl', AbsDir),
%       show_asrts.
% 
% show_asrts :-
%       %Type = pred,
%         current_fact(assertion_read(_Pred0, _Module, check, Type,
%                                     Body, _Dict,   _Src,   _LB, _LE)),
%       assertion_body(Pred, Compat, Precond, Success, Comp, _, Body),
%       numbervars(Body, 0, _N),
%       write(a(Type, Pred, Compat, Precond, Success)), nl,
%       fail.
% show_asrts.
%
% ---------------------------------------------------------------------------

print_gathered_module_data(_M,Base) :-
    format("{Printing all code info~n",[]),
    %
    forall(defines_module(Base,DefMod),
      format("~w defines module ~w~n",[Base,DefMod])),
    %
    forall(exports(Base,F,A,T,Met),
       format("~w exports ~w/~w (~w) meta=~w~n",[Base,F,A,T,Met])),
    %
    forall(def_multifile(Base,F,A,Mo),
       format("~w defines multifile ~w/~w as ~w~n",[Base,F,A,Mo])),
    %
    forall(defines(Base,F,A,T,Met),
       format("~w defines ~w/~w (~w) meta=~w~n",[Base,F,A,T,Met])),
    %
    forall(decl(Base,Decl),
       format("~w has itf-exported new declaration ~w~n",[Base,Decl])),
    %
    forall(uses_file(Base,File),
       format("~w uses ~w~n",[Base,File])),
    %
    forall(adds(Base,File),
       format("~w does ensure_loaded of user file ~w~n",[Base,File])),
    %
    forall(imports_pred(Base,M2,F,A,DefType,Met,EndFile),
            % M2\==builtin,M2\==internals,
       format("~w imports ~w/~w of type ~w from ~w (~w) meta=~w~n",
       [Base,F,A,DefType,M2,EndFile,Met])),
    %
    forall(imports_all(Base,M2), 
       format("~w imports all from ~w~n",[Base,M2])),
    %
    forall(includes(Base,File), 
       format("~w includes ~w~n",[Base,File])),
    %
    forall(loads(Base,Path),
       format("~w loads ~w as compilation module~n",[Base,Path])),
    %
    forall(clause_read(Base,Head,Body,VNs,Source,LB,LE),
       format("~w (~w-~w):~n ~w :- ~w.~nDictionary:~w~n",
              [Source,LB,LE,Head,Body,VNs])),
    %
    format("}~n",[]).

print_code_only(_M,Base) :-
    forall(clause_read(Base,Head,Body,VNs,Source,LB,LE),
       format("~w (~w-~w):~n ~w :- ~w.~nDictionary:~w~n",
              [Source,LB,LE,Head,Body,VNs])).

:- meta_predicate forall(goal, goal).
% For each G, do Do
forall(G, Do) :-
    \+ ( call(G), \+ call(Do) ).

% ---------------------------------------------------------------------------

view_ql(File) :-
    find_filename('.po', File, AbsName),
    '$push_qlinfo',
    '$open'(AbsName, r, Stream),            % Gives errors
    repeat,
        '$qread'(Stream, Goal),
        (   Goal= -1
        ;   display(Goal), nl, fail
        ), !,
    '$pop_qlinfo',
    close(Stream).

% ---------------------------------------------------------------------------

find_filename(Ext, File, AbsName) :-
    atom_concat(_, Ext, File),
    !,
    AbsName = File.
find_filename('.itf', File, AbsName) :-
    find_pl_filename(File, _PlName, Base, _Dir),
    itf_filename(Base, AbsName).
find_filename('.po', File, AbsName) :-
    find_pl_filename(File, _PlName, Base, _Dir),
    po_filename(Base, AbsName).
find_filename('.asr', File, AbsName) :-
    find_pl_filename(File, _PlName, Base, _Dir),
    asr_filename(Base, AbsName).
find_filename('.wam', File, AbsName) :-
    find_pl_filename(File, _PlName, Base, _Dir),
    wam_filename(Base, AbsName).

% ---------------------------------------------------------------------------

:- use_module(library(stream_utils), [file_to_string/2, write_string/1]).

view_wam(File):- 
    make_wam([File]),
    find_filename('.wam', File, AbsName),
    file_to_string(AbsName, String),
    write_string(String).

