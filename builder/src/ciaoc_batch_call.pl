:- module(ciaoc_batch_call, [], []).

:- use_module(library(format), [format/3]).
:- use_module(library(lists), [length/2]).
:- use_module(library(system), [delete_file/1, file_exists/1]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(file_utils), [string_to_file/2]).
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
:- use_module(library(make/up_to_date)). % TODO: this should be in the compiler

% Auxiliary file to call ciaoc in batch mode from invoke_ciaosh_batch/1
% (many modules compiled from the same process)

% TODO: This (with some changes) may be part of ciaoc

:- export(compile_mods/4).
compile_mods(Modules, CompActions, BaseDir, UsingTTY) :-
	length(Modules, N),
	compile_mods_(Modules, CompActions, BaseDir, UsingTTY, 1, N),
	newline_code(UsingTTY, C),
	format(user_error, "~w   Compiled ~w modules\n", [C, N]).

newline_code(using_tty, '\r').
newline_code(no_tty,    '\n').

pl_filename(FileBase, FileName) :- % (reversible)
	atom_concat(FileBase, '.pl',  FileName).

display_compiling_msg(using_tty, I, N) :-
	format(user_error, "\r   Compiling ~w/~w ", [I, N]).
display_compiling_msg(no_tty, _, _) :-
	format(user_error, "\n   Compiling ", []).

compile_mods_([], _CompActions, _BaseDir, _UsingTTY, _I, _N).
compile_mods_([M|Ms], CompActions, BaseDir, UsingTTY, I, N) :-
	compile_mod(M, CompActions, BaseDir, UsingTTY, I, N),
	I1 is I + 1,
	compile_mods_(Ms, CompActions, BaseDir, UsingTTY, I1, N).

compile_mod(m(_, _, FileName), CompActions, BaseDir, UsingTTY, I, N) :-
	( path_get_relative(BaseDir, FileName, File0) -> File = File0
	; File = FileName
	),
	display_compiling_msg(UsingTTY, I, N),
	format(user_error, "~w ", [File]),
	%
	do_comp_actions(CompActions, FileName),
	cleanup_itf_cache.

do_comp_actions([], _FileName).
do_comp_actions([Action|Actions], FileName) :-
	do_comp_action(Action, FileName),
	do_comp_actions(Actions, FileName).

do_comp_action(do_gaf, FileName) :- gaf(FileName).
do_comp_action(do_gpo, FileName) :- gpo(FileName).

gaf(FileName0) :-
	absolute_file_name(FileName0, FileName), % (needed for reliable _filename with CIAOCACHEDIR)
	pl_filename(FileBase, FileName),
	asr_filename(FileBase, FileNameAsr),
	( up_to_date(FileNameAsr, FileName) ->
	    true
	; gen_asr_file_main_rel(FileName)
	).

gen_asr_file_main_rel(FileName0) :-
	absolute_file_name(FileName0, FileName),
	gen_dummy_file(FileName, DummyFileNamePl),
	gen_asr_file(DummyFileNamePl),
	pl_filename(DummyFileName, DummyFileNamePl),
	itf_filename(DummyFileName, DummyFileNameItf),
	delete_file(DummyFileNamePl),
	delete_file(DummyFileNameItf).

gen_asr_file(FileName) :-
	get_code_and_related_assertions(FileName, _M, _Base, _Suffix, _Dir),
	cleanup_itf_cache,
	cleanup_code_and_related_assertions.

gen_dummy_file(FileName, DummyFileName) :-
	get_dummy_file_name(FileName, DummyFileName),
	atom_codes(FileName, SFileName),
	flatten([
		":- module(_, _, [assertions]).\n" ||
		":- use_module(\'" || SFileName,
		"\').\n" ||
		"main."], Content),
	string_to_file(Content, DummyFileName).

get_dummy_file_name(FileName, DummyFileName) :-
	pl_filename(FileBase, FileName),
	atom_concat(FileBase, '_tmp_co.pl', DummyFileName).

gpo(FileName0) :-
	absolute_file_name(FileName0, FileName), % (needed for reliable _filename with CIAOCACHEDIR)
	pl_filename(FileBase, FileName),
	po_filename(FileBase, FileNamePo),
	itf_filename(FileBase, FileNameItf),
	( up_to_date(FileNamePo,  FileName),
	  up_to_date(FileNameItf, FileName) ->
	    true
	; % Remove .itf and .po to force complation
	  %
	  % TODO: If runtime_checks are enabled, then the itf file
	  % generated in gaf/1, could be not valid w.r.t. the
	  % generated in gpo/1, that is why I delete the itf in the
	  % gpo/1 predicate, when creating the po file. -- EMM
	  %
	  delete_if_exists(FileNameItf),
	  delete_if_exists(FileNamePo),
	  make_po(FileName)
	).

delete_if_exists(File) :-
	( file_exists(File) ->
	    delete_file(File)
	; true
	).
