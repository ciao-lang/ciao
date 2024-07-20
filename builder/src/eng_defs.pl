:- module(eng_defs, [], [assertions, fsyntax]).

:- doc(title, "Engine maker: defs and paths").
% TODO: merge with third-party install?
:- doc(author, "Jose F. Morales").
:- doc(author, "The Ciao Development Team").

:- doc(module, "

This module defines engine specifications for @em{engine}
builds. Engines are defined using the @lib{emugen} package. Engine
build specifications contains parameters that affects the compiled
engine:

Engine options:
@begin{itemize}
@item @tt{cross(OS,Arch)}: force cross compilation to selected platform
@item @tt{boot}: the boot engine
@item @tt{addpath(Spec)}: additional (symbolic) source path for locating
   C and header files (handy for custom engines)
@item @tt{h_alias(Alias)}: subdirectory for headers (i.e., name
   used to include header for this engine @tt{#include <ciao/...>})
@end{itemize}

@section{Engine build directory layout}

Sections at the engine build directory layout:
@begin{verbatim}
  hdir: C headers
  cdir: C source files 
  cfgdir: sysdep config files
  objdir: sysdep compiled object files
  engdir: root directory for engine build
  exec: engine as an executable
  lib_so: engine as a C shared library
  lib_a: engine as a C static library
(for engine activation)
  objdir_anyarch: compiled object files root
  exec_anyarch: engine as an executable -- link to active architecture
@end{verbatim}

NOTE: This hierarchy is assumed by other tools and code like
  @tt{build_car.sh}. Please be careful if introducing any change.

Example: directory hierarchy at @tt{build/eng/ciaoengine/}:
@begin{verbatim}
cfg
cfg/DARWINx86_64
cfg/DARWINx86_64/config_mk
cfg/DARWINx86_64/config_sh
cfg/DARWINx86_64/meta_sh
include
include/ciao
include/ciao/*.h
include/*.h
objs
objs/ciaoengine
objs/ciaoengine.DARWINx86_64
objs/DARWINx86_64
objs/DARWINx86_64/*.o
objs/DARWINx86_64/*.P
src
src/*.c
@end{verbatim}
").

% TODO: store 'exec','exec_anyarch' section in some engine bindir?
% TODO: section 'lib_so' is missing in installation
% TODO: section 'lib_a' is missing in installation

% NOTE: Synchronize with build_car.sh definitions for bootstrap

:- use_module(library(lists), [member/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(pathnames), [path_concat/3, path_split/3]).

:- use_module(library(bundle/bundle_paths),
    [bundle_path/3, bundle_path/4, ext_absolute_file_name/3]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(engine(system_info), [get_so_ext/1, get_a_ext/1]).
:- use_module(ciaobld(config_common), [concat_ext/3]).
:- use_module(ciaobld(install_aux), [inst_builddir_path/2]). % TODO: it should be here

% ===========================================================================

:- export(eng_cfg/2).
% Identifier for this build (platform + debug_level)
eng_cfg(Eng) := EngCfg :-
    EngOpts = ~eng_opts(Eng),
    ( member(boot, EngOpts) ->
        EngCfg = 'BOOT' % (special case, see 'boot_eng_cfg' in builder_boot.sh)
    ; ( member(cross(CrossOS, CrossArch), EngOpts) ->
          OS = CrossOS,
          Arch = CrossArch
      ; OS = ~get_bundle_flag(core:os),
        Arch = ~get_bundle_flag(core:arch)
      ),
      OSArch = ~atom_concat(OS, Arch),
      DebugLevel = ~debug_level,
      ( DebugLevel = 'nodebug' -> EngCfg = OSArch
      ; EngCfg = ~atom_concat([OSArch, '-', DebugLevel])
      )
    ).

:- export(eng_dbg/2).
% Suffix for debug_level
eng_dbg(Eng) := EngDbg :-
    EngOpts = ~eng_opts(Eng),
    ( member(boot, EngOpts) ->
        EngDbg = '' % (special case, see 'boot_eng_cfg' in builder_boot.sh)
    ; DebugLevel = ~debug_level,
      ( DebugLevel = 'nodebug' -> EngDbg = ''
      ; EngDbg = ~atom_concat('-', DebugLevel)
      )
    ).

debug_level := ~get_bundle_flag(core:debug_level).

% ===========================================================================
% Eng def accessors

:- export(eng_mainspec/2).
eng_mainspec(eng_def(_Bundle, EngMainSpec, _EngOpts), EngMainSpec).

:- export(eng_opts/2).
eng_opts(eng_def(_Bundle, _EngMainSpec, EngOpts), EngOpts).

:- export(eng_bundle/2).
eng_bundle(eng_def(Bundle, _EngMainSpec, _EngOpts), Bundle).

%:- export(eng_set_boot/2).
% (mark as boot engine -- affects build directory and EngCfg)
eng_set_boot(eng_def(Bundle, EngMainSpec, EngOpts),
         eng_def(Bundle, EngMainSpec, [boot|EngOpts])).

% ===========================================================================
% Paths for source/generated/build/instalation items

:- export(eng_mainmod/2).
eng_mainmod(Eng) := Mod :-
    path_split(~eng_mainbase(Eng), _, Mod).

:- export(eng_mainbase/2).
% Base (source without .pl) of the main engine module
eng_mainbase(Eng) := Base :-
    Bundle = ~eng_bundle(Eng),
    EngMainSpec = ~eng_mainspec(Eng),
    Base = ~bundle_path(Bundle, EngMainSpec).

:- export(eng_srcdirs/2).
% Path for location of C sources
% (It can be extended with usepath/1 in engine options)
eng_srcdirs(Eng) := [EngSrcDir|Others] :- % TODO: Allow more using EngOpts!
    path_split(~eng_mainbase(Eng), EngSrcDir, _),
    EngOpts = ~eng_opts(Eng),
    findall(Dir, (member(usepath(Spec), EngOpts),
                  ext_absolute_file_name(Spec, EngSrcDir, Dir)), Others).

:- export(emugen_code_dir/3).
% Directory where autogenerated File goes in the build area of Eng
emugen_code_dir(Eng, File, DestDir) :-
    ( atom_concat(_, '.h', File) ->
        eng_h_alias(Eng, HAlias),
        DestDir = ~path_concat(~eng_path(hdir, Eng), HAlias)
    ; DestDir = ~eng_path(cdir, Eng)
    ).

:- export(eng_h_alias/2).
% Alias for .h files (for #include directives from C)
% (E.g., "#include <ciao/...>")
eng_h_alias(Eng, Path) :-
    EngOpts = ~eng_opts(Eng),
    ( member(h_alias(Path0), EngOpts) ->
        Path = Path0
    ; Path = 'ciao' % default value
    ).

:- export(eng_path/3).
% Engine directory layout in build area
eng_path(D, Eng) := ~eng_path_(D, bld, Eng).

:- export(inst_eng_path/3).
% Engine directory layout in global installs
%
% TODO: Missing installation of lib_a, lib_so, and probably some of the configuration files?
%
inst_eng_path(D, Eng) := ~eng_path_(D, inst, Eng).

eng_path_(D, EngLoc, Eng) := Path :-
    EngDir = ~base_eng_path(EngLoc, Eng),
    Rel = ~rel_eng_path(D, Eng),
    ( Rel = '' -> Path = EngDir
    ; Path = ~path_concat(EngDir, Rel)
    ).

base_eng_path(EngLoc, Eng) := Path :-
    EngMainMod = ~eng_mainmod(Eng),
    Bundle = ~eng_bundle(Eng),
    ( EngLoc = bld ->
        EngOpts = ~eng_opts(Eng),
        ( member(boot, EngOpts) ->
            Path0 = ~bundle_path(Bundle, bootbuilddir, eng)
        ; Path0 = ~bundle_path(Bundle, builddir, eng)
        )
    ; EngLoc = inst ->
        Path0 = ~inst_builddir_path(eng)
    ; fail
    ),
    Path = ~path_concat(Path0, EngMainMod).

% ---------------------------------------------------------------------------

%:- export(rel_eng_path/5).
% TODO: Add exec_sh and exec_bat (for exec_headers)
rel_eng_path(D, Eng) := Path :-
    EngMainMod = ~eng_mainmod(Eng),
    eng_bin(D, EngMainMod, Eng, BaseD, EngExt, Base),
    !,
    Path = ~path_concat(~rel_eng_path2(BaseD, Eng), ~concat_ext(EngExt, Base)).
rel_eng_path(D, Eng) := ~rel_eng_path2(D, Eng).

rel_eng_path2(cfgdir, Eng) := ~path_concat('cfg', ~eng_cfg(Eng)) :- !.
rel_eng_path2(objdir, Eng) := ~path_concat('objs', ~eng_cfg(Eng)) :- !.
rel_eng_path2(D, _) := ~rel_eng_path1(D).

rel_eng_path1(engdir) := ''.
rel_eng_path1(cdir) := 'src'.
rel_eng_path1(hdir) := 'include'.
rel_eng_path1(objdir_anyarch) := 'objs'.

% TODO: .so and .a ext depend on cross/2
eng_bin(exec, M, Eng, objdir, Ext, M) :-
    eng_ext(Eng, Ext).
eng_bin(lib_so, M, _Eng, objdir, ext(~get_so_ext), ~atom_concat('lib', M)).
eng_bin(lib_a, M, _Eng, objdir, ext(~get_a_ext), ~atom_concat('lib', M)).

% Extension for the engine executable
eng_ext(Eng, EngExt) :-
    EngOpts = ~eng_opts(Eng),
    ( member(cross('EMSCRIPTEN', 'wasm32'), EngOpts) ->
        EngExt = ext('.js')
    ; EngExt = exec
    ).

% ---------------------------------------------------------------------------

:- export(active_bld_eng_path/3).
% Paths for the active engine and multi-platform engine selection
% (in build area).
%
% TODO: define properly the 'activation' operation
active_bld_eng_path(D, Eng) := Path :-
    Name = ~active_eng_name(D, Eng),
    Path = ~path_concat(~eng_path(objdir_anyarch, Eng), Name).

:- export(active_inst_eng_path/3).
% Paths for the active engine and multi-platform engine selection
% (in global installation).
%
% TODO: define properly the 'activation' operation
active_inst_eng_path(D, Eng) := Path :-
    Name = ~active_eng_name(D, Eng),
    Path = ~path_concat(~inst_eng_path(objdir_anyarch, Eng), Name).

:- export(active_eng_name/3).
% Active engine name (for multi-platform build and installation)
active_eng_name(D, Eng) := Name :-
    EngMainMod = ~eng_mainmod(Eng),
    ( D = exec ->         % E.g., ciaoengine.<OSARCH>
        Name0 = ~atom_concat([EngMainMod, '.', ~eng_cfg(Eng)])
    ; D = exec_anyarch -> % E.g., ciaoengine[-debug_level]
        % Name0 = EngMainMod
        % Note: add debug_level so that there can coexists several builds
        Name0 = ~atom_concat(EngMainMod, ~eng_dbg(Eng))
    ; fail
    ),
    EngExt = ~eng_ext(Eng),
    Name = ~concat_ext(EngExt, Name0).

% ---------------------------------------------------------------------------
% Special build paths for cmake components (see cmake_aux.pl)

% TODO: If 'eng' above is generalized to any C target, then we could
%   merge this definition. 

:- export(bld_cmake_path/3).
% Path under build/ (for Bundle workspace) where the CMake build area
% for Name is stored. 'Name' must be a unique name in the workspace
% (like, e.g., binary command names).
bld_cmake_path(Bundle, Name, Path) :-
    BuildDir0 = ~bundle_path(Bundle, builddir, cmake),
    path_concat(BuildDir0, Name, Path).

