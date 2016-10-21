:- module(autodoc_settings, [], [dcg, assertions, regtypes, fsyntax]). 

:- doc(title, "Current documentation settings").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module defines the predicates to load and access
   documentation configurations.").

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(messages), [error_message/2]).
:- use_module(library(aggregates)).

% ---------------------------------------------------------------------------

% TODO: Merge autodoc_option/1 with name_value/2?
:- export(autodoc_option/1).
:- data autodoc_option/1.
%
:- export(settings_file/1).
% settings_file(F): F is the absolute file name with settings (doccfg)
% (none for standalone)
:- data settings_file/1.
%
:- data name_value/2.

set_settings_file(ConfigFile) :-
	assertz_fact(settings_file(ConfigFile)).

:- export(clean_autodoc_settings/0).
clean_autodoc_settings :-
	retractall_fact(settings_file(_)),
	retractall_fact(autodoc_option(_)),
	retractall_fact(name_value(_, _)).

set_opts([]).
set_opts([X|Xs]) :- set_opt(X), set_opts(Xs).

set_opt(autodoc_option(Opt)) :- !,
	assertz_fact(autodoc_option(Opt)).
set_opt(name_value(Name, Value)) :- !,
	add_name_value(Name, Value).
set_opt(X) :- throw(error(unknown_opt(X), set_opt/1)).

% ---------------------------------------------------------------------------
% NOTE: 
%   Any name_value/2 value overwrides **all** the doccfg values for
%   that setting.

:- use_module(lpdoc(doccfg_holder)).
:- use_module(library(lists), [append/3]).

add_name_value(Name, Value) :-
	data_facts:assertz_fact(name_value(Name, Value)).

% read all values
% TODO: findall of get_value should be equivalent
all_values(Name, Values) :-
	all_name_values(Name, Values0),
	( Values0 = [] ->
	    all_pred_values(Name, Values)
	; Values = Values0
	).

all_pred_values(Name, Values) :-
	findall(Value, get_pred_value(Name, Value), Values).

all_name_values(Name, Values) :-
	findall(Value, name_value(Name, Value), Values).

get_value(Name, Value) :-
	( name_value(Name, _) ->
	    name_value(Name, Value)
	; get_pred_value(Name, Value)
	).

dyn_load_doccfg(ConfigFile) :-
	doccfg_holder:do_use_module(ConfigFile).

% (Get value, given by a predicate definition Name/1)
get_pred_value(Name, Value) :-
	( atom(Name) ->
	    Pred =.. [Name, Value]
	; Name =.. Flat,
	  append(Flat, [Value], PredList),
	  Pred =.. PredList
	),
	doccfg_holder:call_unknown(_:Pred).

% ---------------------------------------------------------------------------
:- doc(section, "Loading Setting").

% TODO: no unload?
:- export(load_settings/3).
:- pred load_settings(InFile, InKind, Opts) # "Set configuration from
   @var{InFile} of kind @var{InKind} and @var{Opts}".

load_settings(InFile, InKind, Opts) :-
	clean_autodoc_settings,
	fixed_absolute_file_name(InFile, '.', InFile2),
	( InKind = doccfg ->
	    ( dyn_load_doccfg(InFile2) -> true
	    ; throw(autodoc_error("could not load doccfg file ~w", [InFile]))
	    ),
	    set_settings_file(InFile2)
	; InKind = standalone ->
	    path_dirname(InFile2, InDir),
	    % Fill cfg for standalone
	    add_name_value(filepath, InDir),
	    add_name_value('$implements', 'doccfg'),
	    add_name_value(doc_structure, [InFile]) % TODO: or InFile2?
	; fail
	),
	set_opts(Opts),
	ensure_lpdoclib_defined.

% Verify that the configuration module uses the lpdoclib(doccfg) package
:- export(verify_settings/0).
verify_settings :-
	( setting_value('$implements', 'doccfg') ->
	    true
	; throw(autodoc_error("Configuration files must use the lpdoclib(doccfg) package", []))
	).

% Define 'lpdoclib' setting, check that it is valid
ensure_lpdoclib_defined :-
	( LpDocLibDir = ~file_search_path(lpdoclib),
	  file_exists(~path_concat(LpDocLibDir, 'doccfg.pl')) ->
	    add_name_value(lpdoclib, LpDocLibDir)
	; error_message(
% ___________________________________________________________________________
 "No valid file search path for 'lpdoclib' alias.\n"||
 "Please, check this is LPdoc installation.\n", []),
	  fail
	).

%:- dynamic file_search_path/2.
%:- multifile file_search_path/2.

:- use_module(library(system), [file_exists/1]).

% ---------------------------------------------------------------------------
:- doc(section, "Checking or Setting Options").

:- use_module(library(system)).
:- use_module(library(bundle/doc_flags), [docformatdir/2]).

% (With implicit default value)
:- export(setting_value_or_default/2).
:- pred setting_value_or_default(Var, Value)
# "Returns in @var{Value} the value of the variable @var{Var}. In case
  this variable does not exists, it returns a default value. If there
  is no default value for the variable @var{Var} it fails.".

setting_value_or_default(Name, Value) :-
	( get_value(Name, _) -> % Has some value
	    get_value(Name, Value)
	; Value = ~default_val(Name)
	).

% TODO: Use defaults from doccfg package instead?
default_val(startpage) := 1.
default_val(papertype) := afourpaper.
default_val(perms) := perms(rwX, rX, rX).
default_val(owner) := ~get_pwnam.
default_val(group) := G :- ( G = ~get_grnam -> true ; G = 'unknown' ).
default_val(htmldir) := ~docformatdir(html).
default_val(docdir) := ~docformatdir(any).
default_val(infodir) := ~docformatdir(info).
default_val(mandir) := ~docformatdir(manl).

:- export(setting_value/2).
setting_value(Name, Value) :-
	get_value(Name, Value).

:- export(all_setting_values/2).
all_setting_values(Name) := ~all_values(Name).

% ---------------------------------------------------------------------------
:- doc(section, "Paths to files").

% TODO: Reuse the logic to locate modules by the compiler?

:- use_module(library(bundle/bundle_paths), [ext_absolute_file_name/3]).
:- use_module(library(pathnames), 
	[path_dirname/2, path_is_absolute/1, path_norm/2, path_concat/3]).
:- use_module(lpdoc(autodoc_filesystem), [cleanup_vpath/0, add_vpath/1]).

:- export(load_vpaths/1).
% Setup vpath values, relative to directory of InFile if needed
load_vpaths(InFile) :-
	cleanup_vpath,
	path_dirname(InFile, InDir),
	( % (failure-driven loop)
	  ( P = InDir
	  ; resolved_filepath(InDir, P)
	  ; file_search_path(_Alias, P), % TODO: prioritize alias paths for the current bundle?
	    \+ P = '.'
	  ),
	    add_vpath(P),
	    fail
	; true
	).

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

% Obtain a resolved filepath (use ext_absolute_file_name/2 and make it relative to InDir if needed)
resolved_filepath(InDir, P) :-
	% TODO: document at_bundle(_,_) syntax?
	member(P0, ~all_setting_values(filepath)),
	ext_absolute_file_name(P0, InDir, P1),
	( path_is_absolute(P1) -> P = P1
	; path_concat(InDir, P1, P2),
	  path_norm(P2, P)
	).

% TODO: prioritize alias paths for the current bundle?
% :- use_module(lpdoc(autodoc_filesystem), [get_parent_bundle/1]).
% :- use_module(engine(internals), ['$bundle_alias_path'/3]).

% ---------------------------------------------------------------------------

:- export(custom_html_layout/0).
custom_html_layout :-
	setting_value(html_layout, Layout),
	( Layout = website_layout -> true
	; Layout = tmpl_layout(_, _, _)
	).

% ---------------------------------------------------------------------------
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

