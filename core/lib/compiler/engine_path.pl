:- module(engine_path, [], [assertions]).

% TODO: This module needs a serious rewrite (JFMC) -- there is no need
%   to do backtracking to locate the engine! (use getplatformdeb, etc.)

:- use_module(library(system)).

% Which engine name can be applied to each architecture? Windows
% executables are always named ciaoengine.exe and is only an small loader,
% but the engine is located in libciao.dll
% **x static executables are named *.sta, and can have the
% OS/Arch combination in the name.  It is probably not wise to look
% for a generic ciaoengine.sta executable in the shared, general
% library directory; hence the direct/generic atom in the third
% argument. 

:- doc(bug, "Find a better way to implement this (JFMC)").

:- doc(bug, "Generation of a static engine requires that also the
	foreign modules be compiled together with the engine.
	Currently the engine is always dynamic. -- EMM.").

:- export(get_engine_file/2).
get_engine_file(TargetEng, Engine) :- % TODO: rename TargetEng by ciao_os_arch
	get_engine_common(TargetEng, Engine, _EngDir).

% TODO: use definitions at config_common.pl (perhaps move to paths_extra or a bettr place)

:- export(get_engine_dir/2).
% TODO: why not the value stored in the configuration?
get_engine_dir(TargetEng, EngDir) :-
	get_engine_common(TargetEng, _Engine, EngDir).

get_engine_common(TargetEng, Engine, EngDir) :-
	ciao_lib_dir(LibDir),
	determine_engine_name(TargetEng, EngName, Where),
	determine_engine_dir(TargetEng, Where, LibDir, EngDir),
	atom_concat(EngDir, EngName, Engine),
	file_exists(Engine),
	!. % cut here (once we finally found the engine)

determine_engine_name(TargetEng, Engine, Where) :-
	( atom_concat('Win32', _, TargetEng) ->
	    % Windows engines always have the same name (at least for now)
	    Engine = 'ciaoengine.exe',
	    Where = direct
	; % Other engines have different names according to placement!
	  ( % (nondet)
            % If in installation
	    atom_concat('ciaoengine.', TargetEng, Eng1),
	    Where = direct
          ; % For sources
	    Eng1 = 'ciaoengine',
	    Where = generic
	  ),
	  stat_extension(Sta), % (nondet)
	  atom_concat(Eng1, Sta, Engine)
	).

stat_extension('.sta').
stat_extension('').

% What directory this engine can be in?
% (nondeterministic)
determine_engine_dir(TargetEng, Where, LibDir, EngDir) :-
	% TODO: use something like bld_eng_path/4 or inst_eng_path/4, without introducing a dependency to ciaobld(config_common)
	intermediate_dir(TargetEng, Where, IntermediateDir),
	atom_concat(LibDir, IntermediateDir, EngDir),
	file_exists(EngDir).

% Windows engines can be placed differently from other engines
% (nondeterministic)
% TODO: Do we want it to be non-det?
intermediate_dir(_Target, direct, '/engine/'). % For unix --- Windows also, later? % TODO: obsolete?
intermediate_dir(Target,  _,      Dir       ) :-
	rel_builddir(RelBuildDir),
	atom_concat('/../', RelBuildDir, Dir0),
	atom_concat(Dir0, '/eng/', Dir1),
	EngMainMod = 'ciaoengine', % TODO: fix
	atom_concat(Dir1, EngMainMod, Dir2),
	atom_concat(Dir2, '/objs/', Dir3),
	atom_concat(Dir3, Target, Dir4), % TODO: use get_eng_cfg
	atom_concat(Dir4, '/',       Dir ).

rel_builddir('build').
