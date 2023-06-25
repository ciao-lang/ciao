:- module(aux_filenames,[
    get_module_filename/3,
    get_loaded_module_name/3,
    just_module_name/2
   ], [assertions, isomodes]).

:- doc(title,"Auxiliary file name generation").
% TODO: Move together with compiler libs and factorize (jfmc)

:- doc(author,"Jes@'{u}s Correas").

:- doc(module,"This module provides names for auxiliary files used
   during the execution of ciaopp.").

:- use_module(library(pathnames), [path_basename/2, path_splitext/3]).
:- use_module(library(compiler/p_unit/itf_db), [current_itf/3]).
:- use_module(engine(internals), [ast_filename/2]).
:- use_module(engine(stream_basic), [absolute_file_name/7]).

:- doc(bug,"the get_module_filename/3 predicate name does not
   correspond to the real meaning. It needs a file name without
   extension in the second argument, whereas it seems that the module
   name is enough.").

get_module_filename(pl,Source,FileName):-
    !,
    absolute_file_name(Source,'_opt','.pl','.',FileName,_,_).
get_module_filename(Type,Source,FileName):-
    absolute_file_name(Source,'_opt','.pl','.',_,Base0,_),
    (
        atom_concat(Base,'_opt',Base0)
    ;
        Base = Base0
    ),
    get_extension(Type,Ext),
    % TODO: better integration with CIAOCCACHE (jfmc)
    ( Ext = '.ast' -> ast_filename(Base, FileName)
    ; atom_concat(Base,Ext,FileName)
    ).

%% get_loaded_module_name(+Module,-AbsName,-AbsBase)
%% Given a module spec (as the one returned by current_itf(imports,_,_)),
%% returns the absolute file name and file base. The module must have
%% been read by driver:module/n, or it must be directly related to a 
%% current module already loaded.
get_loaded_module_name(Module,AbsName,AbsBase):-
    just_module_name(Module,MName),
    current_itf(defines_module,MName,Base),
    absolute_file_name(Base,'_opt','.pl','.',AbsName,AbsBase,_).
    
get_extension(asr,'.ast').
get_extension(reg,'.reg').
get_extension(dump,'.dmp'). % TODO: sure? .dump?

%% --------------------------------------------------------------------

%% just_module_name(+IM0,?IM)
%% given a module spec, returns the module name, without directory or 
%% search path qualifiers, nor suffixes (_opt, .pl).
just_module_name(IM0,IM):-
    nonvar(IM0),
    IM0 = user(_),
    !,
    IM = IM0.
just_module_name(IM0,IM):-
    nonvar(IM),
    IM = user(F0),
    !,
    path_splitext(IM0,Base,_),
    path_splitext(F0,Base,_).
just_module_name(IM0,IM):-
    atom(IM0),
    path_splitext(IM0,IM1,_),
    current_itf(defines_module,IM,IM1),
    !.
%% Previous clause should work in most cases.  
%% Following clauses are only applicable for special 
%% cases (non-loaded modules, etc.)
just_module_name(IM0,IM):-
    functor(IM0,_,1),  %% excluding user(F) specifications.
    !,
    arg(1,IM0,IM1),
    path_basename(IM1,IM2),
    path_splitext(IM2,IM3,_),
    ( atom_concat(IM,'_opt',IM3) ->
        true
    ;
        IM = IM3
    ).
just_module_name(IM0,IM):-
    atom(IM0),
    !,
    path_basename(IM0,IM2),
    path_splitext(IM2,IM3,_),
    ( atom_concat(IM,'_opt',IM3) ->
        true
    ;
        IM = IM3
    ).

