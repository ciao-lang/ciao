:- module(autodoc_settings, [], [dcg, assertions, regtypes, fsyntax]). 

:- doc(title,"Access to Default Settings").
:- doc(author,"Jose F. Morales").

:- doc(module, "
	This module defines the setting values with some default values.

@begin{alert}   
@bf{Note: This part needs better documentation. -- JFMC}
@end{alert}
   ").

% ===========================================================================

% (settings db from make_rt)
:- use_module(library(make/make_rt)).

% ===========================================================================

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(messages), [error_message/1]).

:- doc(section, "Loading Setting").

:- export(settings_file/1).
:- data settings_file/1.

set_settings_file(ConfigFile) :-
	retractall_fact(settings_file(_)),
	assertz_fact(settings_file(ConfigFile)).

% TODO: no unload?
% Load settings, set make options, and perform some sanity checks.
:- export(load_settings/2).
load_settings(ConfigFile, Opts) :-
	clean_make_opts,
	load_settings_(ConfigFile),
	set_make_opts(Opts),
	ensure_lpdoclib_defined.

load_settings_(ConfigFile) :-
	(
	    fixed_absolute_file_name(ConfigFile, AbsFilePath),
	    ( file_exists(AbsFilePath)
	    ; atom_concat(AbsFilePath, '.pl', TryThisFile),
	      file_exists(TryThisFile)
	    )
	->
	    trace_message("Using configuration file ~w", [AbsFilePath]),
	    set_settings_file(AbsFilePath),
	    dyn_load_cfg_module_into_make(AbsFilePath)
	;
	    working_directory(CWD0, CWD0),
	    path_concat(CWD0, '', CWD),
	    add_name_value(filepath, CWD),
	    add_name_value('$schema', 'SETTINGS_schema'), % assume that we have a valid schema
	    trace_message("No configuration file. Setting filepath to ~w", [CWD])
	),
	!.
load_settings_(ConfigFile) :-
	throw(make_error("settings file ~w does not exist", [ConfigFile])).

clean_make_opts :-
	retractall_fact(make_option(_)),
	retractall_fact(name_value(_, _)).

set_make_opts([]).
set_make_opts([X|Xs]) :- set_make_opt(X), set_make_opts(Xs).

set_make_opt(make_option(Opt)) :- !,
	assertz_fact(make_option(Opt)).
set_make_opt(name_value(Name, Value)) :- !,
	assertz_fact(name_value(Name, Value)).
set_make_opt(X) :- throw(error(unknown_opt(X), set_make_opt/1)).

% verify that the loaded settings implement SETTINGS_schema
:- export(verify_settings/0).
verify_settings :-
	( setting_value('$schema', 'SETTINGS_schema') ->
	    true
	; throw(make_error("The settings file does not seem to be including SETTINGS_schema", []))
	).

% Define 'lpdoclib' setting, check that it is valid
ensure_lpdoclib_defined :-
	( LpDocLibDir = ~file_search_path(lpdoclib),
	  file_exists(~path_concat(LpDocLibDir, 'SETTINGS_schema.pl')) ->
	    add_name_value(lpdoclib, LpDocLibDir)
	; error_message(
"No valid file search path for 'lpdoclib' alias. It is not defined or it does \n"||
"not contain proper installation files. The LPdoc build/installation does not \n"||
"seem to be correct."),
	  fail
	).

%:- dynamic file_search_path/2.
%:- multifile file_search_path/2.

:- use_module(library(system), [file_exists/1]).

% ===========================================================================

:- doc(section, "Checking or Setting Options").

:- use_module(library(system)).
:- use_module(library(system_extra)).
:- use_module(library(bundle/doc_flags), [docformatdir/2]).

:- export(check_setting/1).
check_setting(Name) :- check_var_exists(Name).

:- use_module(library(bundle/doc_flags), [bibfile/1, docformatdir/2]).

% (With implicit default value)
:- export(setting_value_or_default/2).
:- pred setting_value_or_default(Var, Value)
# "Returns in @var{Value} the value of the variable @var{Var}. In case
  this variable does not exists, it returns a default value. If there
  is no default value for the variable @var{Var} it fails.".

setting_value_or_default(Name, Value) :-
	( make_rt:get_value(Name, Value0) ->
	    Value = Value0
	; Value = ~default_val(Name)
	).

default_val(startpage) := 1.
default_val(papertype) := afourpaper.
default_val(perms) := perms(rwX, rX, rX).
default_val(owner) := ~get_pwnam.
default_val(group) := G :- ( G = ~get_grnam -> true ; G = 'unknown' ).
default_val(bibfile) := ~bibfile.
default_val(htmldir) := ~docformatdir(html).
default_val(docdir) := ~docformatdir(any).
default_val(infodir) := ~docformatdir(info).
default_val(mandir) := ~docformatdir(manl).

% (With explicit default value)
:- export(setting_value_or_default/3).
setting_value_or_default(Name, DefValue, Value) :-
	( make_rt:get_value(Name, Value0) ->
	    Value = Value0
	; Value = DefValue
	).

:- export(setting_value/2).
setting_value(Name, Value) :-
	make_rt:get_value(Name, Value).

:- export(all_setting_values/2).
%all_setting_values(Name) := ~findall(T, ~setting_value(doc_mainopt)).
all_setting_values(X) := ~findall(T, setting_value(X, T)) :-
	( X = doc_mainopts ; X = doc_compopts ), !. % TODO: all_values fail if empty?!
all_setting_values(Name) := ~all_values(Name).

:- use_module(library(aggregates)).

:- export(requested_file_formats/1).
:- pred requested_file_formats(F) # "@var{F} is a requested file format".
requested_file_formats := F :-
	F = ~all_values(docformat).

% ===========================================================================

:- doc(section, "Paths to Code").

:- export(load_vpaths/0).
load_vpaths :-
	get_lib_opts(Libs, SysLibs),
	( % (failure-driven loop)
	  ( member(P, Libs)
	  ; member(P, SysLibs)
	  ),
	    add_vpath(P),
	    trace_message("Added file path: ~w", [P]),
	    fail
	; true
	).

:- export(get_lib_opts/2).
get_lib_opts(Libs, SysLibs) :-
	Libs = ~all_setting_values(filepath),
%	SysLibs = ~all_setting_values(systempath),
	SysLibs = ~findall(P, (file_search_path(_Alias, P), \+ P = '.')).

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

% TODO: prioritize alias paths for the current bundle?
% :- use_module(lpdoc(autodoc_filesystem), [get_parent_bundle/1]).
% :- use_module(engine(internals), ['$bundle_alias_path'/3]).

% ===========================================================================

:- doc(section, "External Commands").
% TODO: Ideally, each backend should specify this part.

:- doc(subsection, "Visualization of Documents").
% TODO: These commands were originally customizable by the
%       user. Nowadays, configuration files are not easy to find... It
%       is lpdoc task to determine what application to use
%       automatically based on the operating system.

:- use_module(engine(system_info), [get_os/1]).

:- export(generic_viewer/1).
% Generic document viewer
generic_viewer('open') :- get_os('DARWIN'), !.
generic_viewer('cygstart') :- get_os('Win32'), !.
%viewer('start') :- get_os('Win32'), !.
generic_viewer('xdg-open') :- get_os('LINUX'), !.

% TODO: This seems to be done by the emacs mode...
% lpsettings <- [] # "Generates default LPSETTINGS.pl in the current directory"
% 	:-
% 	working_directory(CWD0, CWD0),
%       path_concat(CWD0, '', CWD),
% 	generate_default_lpsettings_file(CWD, '').

%% The command that views dvi files in your system
:- export(xdvi/1).
xdvi := 'xdvi'.

%% The default size at which manuals are viewed This
%% is typically an integer (1-10 usually) and unfortunately changes
%% depending on the version of xdvi used.
:- export(xdvisize/1).
xdvisize := '8'.

:- doc(subsection, "Bibliography Generation").

%% The command that builds .bbl files from .bib bibliography
%% files in your system
:- export(bibtex/1).
bibtex := 'bibtex'.

:- doc(subsection, "Texinfo Related Commands").

%% Define this to be the command that runs tex in your system
:- export(tex/1).
tex := 'tex'.

%% Alternative (sometimes smarter about number of times it needs to run):
%% tex := 'texi2dvi '.
%% (but insists on checking the links, which is a pain...)

%% The command that runs texindex in your system
%% (Not needed if texi2dvi is installed)
:- export(texindex/1).
texindex := 'texindex'.

%% The command that converts dvi to postscript in your system.
:- export(dvips/1).
dvips := 'dvips'.

%% The command that converts postscript to pdf in your system. Make
%% sure it generates postscript fonts, not bitmaps (selecting -Ppdf in
%% dvips often does the trick)
:- export(ps2pdf/1).
ps2pdf := 'ps2pdf'.

%% The command that converts tex to pdf in your system
%% texpdf := 'pdftex'.

%% The command that converts texinfo files into info
%% files in your system. Set also the appropriate flags.
:- export(makeinfo/1).
makeinfo := 'makeinfo'.

:- doc(subsection, "Image Conversions").

%% The command that converts graphics files to other formats
:- export(convertc/1).
convertc := 'convert'.

