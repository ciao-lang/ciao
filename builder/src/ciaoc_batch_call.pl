:- module(ciaoc_batch_call, [], []).

% Auxiliary file to call ciaoc in batch mode from invoke_ciaosh_batch/1
% (many modules compiled from the same process).
%
% IMPORTANT: Keep it as simple as possible!

% TODO: This (with some changes) may be part of ciaoc

:- use_module(library(format), [format/3]).
:- use_module(library(lists), [length/2]).
:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(llists), [flatten/2]).
:- use_module(engine(stream_basic)).
:- use_module(library(stream_utils), [string_to_file/2]).
:- use_module(library(pathnames), [path_get_relative/3]).

:- use_module(library(compiler),                [make_po/1]).
:- use_module(library(compiler/c_itf_internal), [cleanup_itf_cache/0]).
:- use_module(library(assertions/assrt_lib),
    [get_code_and_related_assertions/5,
     cleanup_code_and_related_assertions/0]).
:- use_module(engine(internals),
	[po_filename/2,
	 itf_filename/2,
	 asr_filename/2,
	 ast_filename/2]).
:- use_module(library(compiler/up_to_date)).

:- export(compile_mods/5).
% Process modules, sequentially
compile_mods(Modules, CompActions, BaseDir, RelDir, ReportMode) :-
	length(Modules, N),
	compile_mods_(Modules, CompActions, BaseDir, RelDir, ReportMode, 1, N),
	display_done(ReportMode).

compile_mods_([], _CompActions, _BaseDir, _RelDir, _ReportMode, _I, _N).
compile_mods_([M|Ms], CompActions, BaseDir, RelDir, ReportMode, I, N) :-
	compile_mod(M, CompActions, BaseDir, RelDir, ReportMode, I, N),
	I1 is I + 1,
	compile_mods_(Ms, CompActions, BaseDir, RelDir, ReportMode, I1, N).

compile_mod(m(_, _, FileName), CompActions, BaseDir, RelDir, ReportMode, I, N) :-
	( path_get_relative(BaseDir, FileName, File0) -> File = File0
	; File = FileName
	),
	display_progress(ReportMode, RelDir, File, I, N),
	do_comp_actions(CompActions, FileName),
	cleanup_itf_cache. % TODO: needed?

do_comp_actions([], _FileName).
do_comp_actions([Action|Actions], FileName) :-
	do_comp_action(Action, FileName),
	do_comp_actions(Actions, FileName).

do_comp_action(do_gaf, FileName) :- gaf(FileName).
do_comp_action(do_gpo, FileName) :- gpo(FileName).

gaf(FileName0) :-
	% (absolute path needed in *_filename/2)
	absolute_file_name(FileName0, FileName),
	get_base_name(FileName, FileBase),
	asr_filename(FileBase, FileNameAsr),
	( up_to_date(FileNameAsr, FileName) ->
	    true
	; gen_asr_file_main_rel(FileName)
	).

gen_asr_file_main_rel(FileName0) :-
	absolute_file_name(FileName0, FileName),
	gen_dummy_file(FileName, DummyBase, DummyPl),
	gen_asr_file(DummyPl),
	itf_filename(DummyBase, DummyItf),
	del_file_nofail(DummyPl),
	del_file_nofail(DummyItf).

gen_asr_file(FileName) :-
	get_code_and_related_assertions(FileName, _M, _Base, _Suffix, _Dir),
	cleanup_itf_cache,
	cleanup_code_and_related_assertions.

gen_dummy_file(FileName, DummyBase, DummyPl) :-
	% (DummyBase must be an absolute path)
	mktemp_in_tmp('tmpciaoXXXXXX', DummyBase),
	atom_concat(DummyBase, '.pl', DummyPl),
	atom_codes(FileName, SFileName),
	flatten([
		":- module(_, _, [assertions]).\n" ||
		":- use_module(\'" || SFileName,
		"\').\n" ||
		"main."], Content),
	string_to_file(Content, DummyPl).

gpo(FileName0) :-
	% (absolute path needed in *_filename/2)
	absolute_file_name(FileName0, FileName),
	get_base_name(FileName, FileBase),
	po_filename(FileBase, FileNamePo),
	itf_filename(FileBase, FileNameItf),
	( up_to_date(FileNamePo,  FileName),
	  up_to_date(FileNameItf, FileName) ->
	    true % TODO: missing dep checks? use make_po instead?
	; % TODO: The following was introduced by EMM (see comment
	  %   below), but it is not worth it (specially if we want to
	  %   use out-of-tree builds for rtchecks) --JF
	  %
	  % % "If runtime_checks are enabled, then the itf file
	  % %  generated in gaf/1, could be not valid w.r.t. the
	  % %  generated in gpo/1, that is why I delete the itf in the
	  % %  gpo/1 predicate, when creating the po file. -- EMM"
	  % del_file_nofail(FileNameItf),
	  % del_file_nofail(FileNamePo),
	  %
	  make_po(FileName)
	).

% TODO: (partially) duplicated
get_base_name(FileName, FileBase) :-
	atom_concat(FileBase, '.pl',  FileName).

% ---------------------------------------------------------------------------
% Cleaning

:- use_module(engine(internals), [ciao_root/1]).
:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(sh_process), [sh_process_call/3]).

% Clean (compilation files in) a directory tree (recursively)
:- export(clean_tree/1).
clean_tree(Dir) :-
	( cachedir_prefix(Dir, Prefix) ->
	    % out-of-tree build
	    path_split(Prefix, CacheDir, RelPrefix),
	    clean_aux(clean_cachedir, [CacheDir, RelPrefix]),
	    % TODO: make it optional; it should not needed with out-of-tree builds
	    clean_aux(clean_tree, [Dir])
	; % in-tree builds
	  clean_aux(clean_tree, [Dir])
	).

clean_aux(Command, Args) :-
	% TODO: reimplement in Prolog
	ciao_root(CiaoRoot),
	path_concat(CiaoRoot, 'builder/sh_src/clean_aux.sh', Sh),
	sh_process_call(Sh, [Command|Args], []).

% Clean compilation files for individual modules (Base is file without .pl suffix)
:- export(clean_mods/1).
clean_mods([]).
clean_mods([Base|Bases]) :- clean_mod(Base), clean_mods(Bases).

% TODO: complete, replace sh version
%          "$1.asr"
%          "$1.ast"
%          "$1.itf"
%          "$1.po"
%          "$1.testout"
% 	   "$1""_""$CIAOOS$CIAOARCH"".o"
% 	   "$1""_""$CIAOOS$CIAOARCH"".a"
% 	   "$1""_""$CIAOOS$CIAOARCH"".so"
%          "$1""_""$CIAOOS$CIAOARCH"".dll"
% 	   "$1""_""$CIAOOS$CIAOARCH"".dylib"
%          "$1""_""$CIAOOS$CIAOARCH""_glue.c"
%          "$1""_""$CIAOOS$CIAOARCH""_glue.o"
% 	   "$1""_""$CIAOOS$CIAOARCH""_inline.c"
clean_mod(Base) :-
	itf_filename(Base, Itf),
	po_filename(Base, Po),
	del_file_nofail(Itf),
	del_file_nofail(Po).

% ---------------------------------------------------------------------------
% Support for cachedir

:- use_module(engine(stream_basic), [fixed_absolute_file_name/3]).
:- import(internals, [translate_base/2]). % TODO: export

:- export(show_cachedir_prefix/1).
% Show the absolute path prefix for out-of-tree compilation of files at Dir
% (used from installation)
show_cachedir_prefix(Dir) :-
	( cachedir_prefix(Dir, Prefix) ->
	    MaybePrefix = yes(Prefix)
	; MaybePrefix = no
	),
	format(user_output, "~q.\n", [MaybePrefix]).

% Prefix is the cachedir path prefix for out-of-tree builds.
% Fails if out-of-tree builds is not enabled for Dir.
cachedir_prefix(Dir, Prefix) :-
	fixed_absolute_file_name(Dir, '.', AbsDir),
	translate_base(AbsDir, AbsDir2),
	\+ AbsDir2 = AbsDir, % out-of-tree builds
	Prefix = AbsDir2.

% ---------------------------------------------------------------------------
% Messages for compilation progress

% TODO: it must have same format as normal_message, share code?

display_progress(repmode(Count,EraseLine), RelDir, File, I, N) :-
	( I = 1 -> C = '' ; newline_code(EraseLine, C) ),
	( Count = yes ->
	    format(user_error, "~w   compiling [~w/~w] ~w/~w ", [C, I, N, RelDir, File])
	; format(user_error, "~w   compiling ~w/~w ", [C, RelDir, File])
	).

display_done(repmode(_,EraseLine)) :-
	newline_code(EraseLine, C),
	format(user_error, "~w", [C]).

% newline_code(EraseLine, Code)
%newline_code(yes, '\r').
newline_code(yes, '\33\[2K\r'). % erase line and carriage-return 
newline_code(no,  '\n').

