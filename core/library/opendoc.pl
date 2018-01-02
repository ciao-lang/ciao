:- module(opendoc, [], [assertions, regtypes, dcg, basicmodes, fsyntax]).

:- doc(title, "Open a document with an external application").
:- doc(author, "Ciao Development Team").

:- doc(module, "This module provides predicates to open documents with
   user's preferred external applications. It is based external
   commands typically provided by each operating system (e.g.,
   @tt{xdg-open} (Linux), @tt{cygstart} (Windows), @tt{open}
   (macOS)).").

:- use_module(library(pathnames), [path_splitext/3]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(engine(system_info), [get_os/1]).

% ---------------------------------------------------------------------------

:- export(opendoc/1).
:- pred opendoc(+Target) :: term
   # "Opens @var{Target} (a file or URL) with the user's preferred
      application".

opendoc(Path) :-
	opendoc(Path, []).

:- export(opendoc/2).
:- pred opendoc(+Target, +Opts) :: term * list
   # "Opens @var{Target} (a file or URL) with the user's preferred
      application. @var{Opts} is a subset of @tt{[generic, in_emacs]}
      (@tt{generic} is the default)".

opendoc(Path, Opts) :-
	get_viewer(Path, Opts, Viewer, Path2),
	( Viewer = emacs(EmacsFun) -> view_in_emacs(EmacsFun, Path2)
	; Viewer = osascript -> view_in_osascript(Path2)
	; % Viewer = generic
	  generic_viewer(ViewerCmd),
	  process_call(path(ViewerCmd), [Path], [])
	).

% ---------------------------------------------------------------------------

% Path is a URL with protocol
has_protocol(Path) :-
	( atom_concat('file://', _, Path) -> true
	; atom_concat('http://', _, Path) -> true
	; atom_concat('https://', _, Path) -> true
	; fail
	).

% ---------------------------------------------------------------------------
	
% get_viewer(+Path, +Opts, -Viewer, -Path2): 
%   Obtain Viewer to view document at Path:
%
%    - emacs(Fun): elisp function Fun
%    - osascript: custom osascript (for macOS)
%
%   Path2 is a modified Path (if needed) for the specified viewer

get_viewer(Path, Opts, Viewer, Path2) :-
	( has_protocol(Path) -> Ext = '.html' % assume .html
	; path_splitext(Path, _, Ext)
	),
	custom_viewer(Ext, Opts, Viewer),
	!,
	% Escape path (for elisp expression)
	( has_protocol(Path) -> Path2 = Path
	; absolute_file_name(Path, Path1),
	  ( Ext = '.html' -> % force protocol
	      Path2 = ~atom_concat('file://', Path1)
	  ; Path2 = Path1
	  )
	).
get_viewer(Path, _Opts, generic, Path).

custom_viewer('.info', _, emacs(info)).
custom_viewer('.manl', _, emacs(man)).
custom_viewer('.html', Opts, emacs(eww)) :- member(in_emacs, Opts), !.
custom_viewer('.html', _Opts, osascript) :- get_os('DARWIN').

% ---------------------------------------------------------------------------
% Open with emacsclient

view_in_emacs(EmacsFun, Path) :-
	atom_codes(Path, Path1),
	esc_codes(Path1, Path2, []),
	atom_codes(Path3, Path2),
	Code = ~atom_concat(['(', EmacsFun, ' \"', Path3, '\")']),
	%
	process_call(path(emacsclient), ['-n', '--eval', Code], []).

% ---------------------------------------------------------------------------
% Use `osascript` in macOS so that we can handle URL fragments
% (E.g., file:///foo/bar.html#anchor). The `open` command ignores them
% for file://

view_in_osascript(Path) :-
	atom_codes(Path, Path1),
	esc_codes(Path1, Path2, []),
	atom_codes(Path3, Path2),
	Code = ~atom_concat(['open location \"', Path3, '\"']),
	%
	process_call(path(osascript), ['-e', Code], []).

% ---------------------------------------------------------------------------

% TODO: merge esc_codes/3 with elisp_interface.pl

% string-escaped codes
esc_codes([]) --> [].
esc_codes([X|Xs]) --> esc_code(X), esc_codes(Xs).

esc_code(0'") --> !, "\\\"".
esc_code(0'\\) --> !, "\\\\".
esc_code(X) --> [X].

% ---------------------------------------------------------------------------

% Generic document viewer
generic_viewer('open') :- get_os('DARWIN'), !.
generic_viewer('cygstart') :- get_os('Win32'), !.
%viewer('start') :- get_os('Win32'), !.
generic_viewer('xdg-open') :- get_os('LINUX'), !.

