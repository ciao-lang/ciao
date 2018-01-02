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
	( use_emacs(Path, Opts, EmacsFun) ->
	    view_in_emacs(EmacsFun, Path)
	; generic_viewer(Viewer),
	  process_call(path(Viewer), [Path], [])
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
	
% use_emacs(+Path, +Opts, -Fun): 
%   Use emacs to view document at Path with elisp function Fun
use_emacs(Path, Opts, Fun) :-
	( has_protocol(Path) -> Ext = '.html' % assume it is .html (skips anchors, etc.)
	; path_splitext(Path, _, Ext)
	),
	use_emacs_(Ext, Opts, Fun).

use_emacs_('.info', _, info).
use_emacs_('.manl', _, man).
use_emacs_('.html', Opts, 'eww') :-
        member(in_emacs, Opts), !.

view_in_emacs(EmacsFun, Path) :-
	% Escape path (for elisp expression)
	( has_protocol(Path) -> Path0 = Path
	; absolute_file_name(Path, Path00),
	  ( EmacsFun = eww -> % force protocol
	      Path0 = ~atom_concat('file://', Path00)
	  ; Path0 = Path
	  )
	),
	atom_codes(Path0, Path1),
	esc_codes(Path1, Path2, []),
	atom_codes(Path3, Path2),
	Code = ~atom_concat(['(', EmacsFun, ' \"', Path3, '\")']),
	%
	process_call(path(emacsclient), ['-n', '--eval', Code], []).

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

