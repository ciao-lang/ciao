:- module(_, [], [assertions]).

% Obtain paths for specific engines (for some EngCfg)
% (See eng_defs:eng_cfg/2)

% NOTE: This module defines functionality similar to that of
%   eng_defs.pl and config_common.pl, which are partially duplicated
%   to avoid the inclusion of builder modules inside the bootstrap
%   compiler.

% TODO: This module needs a serious rewrite (JFMC) -- there is no need to
%   do backtracking to locate the engine! (use getplatformdeb, etc.)

:- use_module(library(pathnames), [path_split/3, path_concat/3]).
:- use_module(library(system), [file_exists/1]).
:- use_module(engine(internals), [ciao_root/1]).

:- doc(bug, "Avoid code duplication from ciaobld(eng_defs) and
   ciaobld(config_common). DO NOT use those modules here! (See note
   above). A solution may be to delegate all this functionality to a
   load_compilation_module").

:- doc(bug, "Allow selection of dynamic/static engines.").

:- doc(bug, "Windows engines are dynamic (needs libciaoengine.dll)
   while Unix are not!").

:- doc(bug, "Connect with car_maker.pl (so that we can build fully
   static engines including foreign libraries)").

:- export(get_engine_file/2).
get_engine_file(EngCfg, EngPath) :-
    eng_path(exec, eng_def0('ciaoengine', EngCfg), EngPath). % TODO: customize 'ciaoengine' (for using different engines)

:- export(get_engine_dir/2).
get_engine_dir(EngCfg, EngDir) :-
    eng_path(exec, eng_def0('ciaoengine', EngCfg), EngPath), % TODO: customize 'ciaoengine' (for using different engines)
    path_split(EngPath, EngDir, _).

% (equivalent to eng_defs:eng_path/3)
eng_path(D, Eng, EngPath) :-
    base_eng_path(Eng, EngDir),
    rel_eng_path(D, Eng, Rel),
    path_concat(EngDir, Rel, EngPath),
    file_exists(EngPath).

% (equivalent to eng_defs:base_eng_path/3)
% E.g., .../build/eng/EngMainMod
base_eng_path(eng_def0(EngMainMod, _), Path) :-
    ciao_root(CiaoRoot),
    %
    rel_builddir(RelBuildDir),
    path_concat(CiaoRoot, RelBuildDir, BldDir),
    path_concat(BldDir, 'eng', Path0),
    %
    path_concat(Path0, EngMainMod, Path).

rel_builddir('build').

% (equivalent to eng_defs:rel_eng_path/5)
rel_eng_path(exec, Eng, Path) :-
    rel_eng_path2(objdir, Eng, Path0),
    %
    Eng = eng_def0(EngMainMod, EngCfg),
    Base0 = EngMainMod,
    ( atom_concat('Win32', _, EngCfg) -> % TODO: customize (allow cross(_,_))
        atom_concat(Base0, '.exe', Base)
    ; Base = Base0
    ),
    path_concat(Path0, Base, Path).

rel_eng_path2(objdir, eng_def0(_, EngCfg), Path) :-
    path_concat('objs', EngCfg, Path).
