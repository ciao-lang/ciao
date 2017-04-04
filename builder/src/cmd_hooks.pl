% :- doc(section, "Hooks for definition of a command").
% (included file)

% NOTE:
%   A command must define at least 'cmd.grade'/2 or 'cmd.do'/2.
%
%   If 'cmd.do'/2 is not defined, then we use the definitions at
%   .hook.pl files or execute the default actions on the primitive targets.

:- multifile 'cmd.comment'/2. % comment for command
:- discontiguous('cmd.comment'/2).
%
:- multifile 'cmd.grade'/2. % grade, needed when when 'cmd.do'/2 is not defined
:- discontiguous('cmd.grade'/2).
%
:- multifile 'cmd.only_global_instype'/1. % disable if \+instype(global)
:- discontiguous('cmd.only_global_instype'/1).
% TODO: can it be simpler to express the negation instead?
:- multifile 'cmd.needs_update_builder'/1. % needs an up-to-date builder
:- discontiguous('cmd.needs_update_builder'/1).
:- multifile 'cmd.needs_rescan'/1. % needs rescanned bundles
:- discontiguous('cmd.needs_rescan'/1).
:- multifile 'cmd.needs_config'/1. % needs a configuration
:- discontiguous('cmd.needs_config'/1).
:- multifile 'cmd.allow_unknown_targets'/1. % allow unknown targets (e.g., for 'fetch')
:- discontiguous('cmd.allow_unknown_targets'/1).
:- multifile 'cmd.no_manifest_load'/1. % manifest does not need to be loaded (only for some 'cmd.do'/2)
:- discontiguous('cmd.no_manifest_load'/1).
%
:- multifile 'cmd.recursive'/2. % recursive on dependencies
                                %  - forward: dependencies first
                                %  - backward: dependencies later
                                %  - none (or undeclared): do not process deps
:- discontiguous('cmd.recursive'/2).
%
:- multifile 'cmd.do_before.decl'/1. % has 'cmd.do_before'/2
:- discontiguous('cmd.do_before.decl'/1).
:- multifile 'cmd.do_before'/2. % do_before command
:- discontiguous('cmd.do_before'/2).
:- multifile 'cmd.do.decl'/1. % has 'cmd.do'/2
:- discontiguous('cmd.do.decl'/1).
:- multifile 'cmd.do'/2. % do command
:- discontiguous('cmd.do'/2).
:- multifile 'cmd.do_after.decl'/1. % has 'cmd.do_after'/2
:- discontiguous('cmd.do_after.decl'/1).
:- multifile 'cmd.do_after'/2. % do_after command
:- discontiguous('cmd.do_after'/2).

% Grades

:- multifile 'grade.cmd'/3. % special command for grade
:- discontiguous('grade.cmd'/3).

:- multifile 'grade.requires'/3. % target requirements for a grade
:- discontiguous('grade.requires'/3).

% 'grade.prim_do'(Grade, Prim, Bundle, Cmd): perform Cmd on primitive target
%   Prim of bundle Bundle
:- multifile 'grade.prim_do'/4.
:- discontiguous('grade.prim_do'/4).

% 'grade.prim_kind'(Grade, Kind): kind of primitive target that this
%   grade can treat ('bin' or 'docs')
:- multifile 'grade.prim_kind'/2.
:- discontiguous('grade.prim_kind'/2).

