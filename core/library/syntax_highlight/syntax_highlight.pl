:- module(syntax_highlight, [], [assertions, doccomments, fsyntax]).

%! \title A syntax highlighter
%
%  \module This module implements a syntax highlighter for different
%    source code languages. Currently it depends on external tools
%    like \apl{emacs}.

:- use_module(library(system), [file_exists/1]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(emacs/emacs_batch), [emacs_path/1]).

% TODO: Implement as an interface; define different backend
% TODO: Adds dependency to 'ide' bundle
% TODO: We really need support for assets for generation of static binaries
	
:- export(highlight_to_html/3).
:- pred highlight_to_html(ModeName, Input, Output) # "Produce HTML
   `Output` file with syntax highlight from `Input` file (using emacs
   and htmlfontify".

highlight_to_html(ModeName, Input, Output) :-
	find_asset(library(syntax_highlight/'emacs-htmlfontify.el'), HfyEl),
	find_asset(bundle_src(ide)/'emacs-mode'/'ciao-site-file.el', SiteFileEl),
	process_call(~emacs_path,
	     ['-batch',
              '-l', SiteFileEl,
	      '-l', HfyEl,
	      ModeName,
	      Input,
	      Output],
	     [noenv(['EMACSLOADPATH', 'EMACSDOC'])]).

% Locate some code-related asset
find_asset(F, Path) :-
	F = library(_),
	absolute_file_name(F, Path0),
	!,
	Path = Path0.
find_asset(F, Path) :-
	fsR(F, Path0),
	file_exists(Path0),
	!,
	Path = Path0.
find_asset(F, _) :-
	% Probably we are running it from a statically binary and Ciao
	% is not installed
	throw(error(not_found(F), highlight_to_html/2)).
