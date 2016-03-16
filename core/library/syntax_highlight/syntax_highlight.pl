:- module(syntax_highlight, [], [assertions, doccomments, isomodes, regtypes, fsyntax]).

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
:- pred highlight_to_html(Lang, Input, Output) # "Produce HTML
   `Output` file with syntax highlight from `Input` file (using emacs
   and htmlfontify, and `Lang`-mode mode)".

highlight_to_html(Lang, Input, Output) :-
	find_asset(library(syntax_highlight/'emacs-htmlfontify.el'), HfyEl),
	find_asset(bundle_src(ide)/'emacs-mode'/'ciao-site-file.el', SiteFileEl),
	process_call(~emacs_path,
	     ['-batch',
              '-l', SiteFileEl,
	      '-l', HfyEl,
	      Lang,
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

:- export(lang/1).
:- regtype lang(L) # "`L` is a language for syntax highlight".

lang('ciao').
lang('c++').
lang('c').
lang('java').
lang('javascript').
lang('xml').
lang('html').
lang('css').
lang('sh').
lang('bash').
lang('text').

% ---------------------------------------------------------------------------

% TODO: Does not look inside contents, just the extension

:- use_module(library(pathnames), [path_splitext/3]).

:- export(detect_language/2).
:- pred detect_language(+File,-Lang) :: atm * lang # "Detect language
   `Lang` of file `File` (may look at contents)".

detect_language(File, Lang) :-
	path_splitext(File, _, Ext),
	( detect_language_(Ext, Lang) ->
	    true
	; Lang = text
	).
	
detect_language_('.pl', 'ciao').
detect_language_('.c', 'c').
detect_language_('.h', 'c').
detect_language_('.cc', 'c++').
detect_language_('.hh', 'c++').
detect_language_('.cpp', 'c++').
detect_language_('.java', 'java').
detect_language_('.js', 'javascript').
detect_language_('.xml', 'xml').
detect_language_('.html', 'html').
detect_language_('.css', 'css').
detect_language_('.sh', 'sh').
detect_language_('.bash', 'sh').
% detect_language_('.lpdoc', 'lpdoc').
% detect_language_('.md', 'markdown').
