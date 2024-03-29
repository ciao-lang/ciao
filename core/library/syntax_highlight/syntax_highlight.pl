:- module(syntax_highlight, [], [assertions, doccomments, isomodes, regtypes, fsyntax, datafacts]).

%! \title A syntax highlighter
%
%  \module This module implements a syntax highlighter for different
%    source code languages. Currently it depends on external tools
%    like \apl{emacs}.


:- use_module(library(lists), [append/3]).
:- use_module(library(system), [file_exists/1]).
:- use_module(engine(stream_basic), [absolute_file_name/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(emacs/emacs_batch), [
    emacs_path/1, emacs_batch_call/2]).

% TODO: uses files! it can be quite slow
% TODO: Implement as an interface; define different backend
% TODO: adds dependency to 'ciao_emacs' bundle and emacs (should be weak refs)
% TODO: We really need support for assets for generation of static binaries

% ---------------------------------------------------------------------------

:- export(lang/1).
:- regtype lang(L) # "`L` is a language for syntax highlight".

lang('ciao').
lang('ciao-inferior').
lang('c++').
lang('c').
lang('java').
lang('javascript').
lang('xml').
lang('html').
lang('css').
lang('lisp').
lang('sh').
lang('bash').
lang('text').

% ---------------------------------------------------------------------------

ciao_mode_el := ~bundle_path(ciao_emacs, 'elisp/ciao-site-file.el').

% ---------------------------------------------------------------------------

:- data can_highlight/2.

:- export(can_highlight/1).
:- pred can_highlight(Lang) : atm => lang
   # "Check if `Lang` is a supported language (silently fails if any
      dependency for highlighting, e.g., emacs, ciao-mode, is not
      installed)".

can_highlight(Lang) :- var(Lang), !, fail.
can_highlight(Lang) :- can_highlight(Lang, S), !, S = yes.
can_highlight(Lang) :-
    ( lang(Lang),
      catch(emacs_path(E), _, fail),
      file_exists(E),
      ciao_mode_el(F), % TODO: this one only for 'ciao'?
      file_exists(F) ->
        S = yes
    ; S = no
    ),
    assertz_fact(can_highlight(Lang, S)),
    S = yes.

% ---------------------------------------------------------------------------

% TODO: allow other mode selection? precompile .el files?
% TODO: this starts a new emacs process, reuse a emacs daemon instead!

:- pred emacs_htmlfontify(+Lang, +Input, +Output) :: lang * atm * atm
   # "Produce HTML `Output` file with syntax highlight from `Input`
      file (using emacs and htmlfontify, and `Lang`-mode mode)".

emacs_htmlfontify(Lang, Input, Output) :-
    HfyEl = ~absolute_file_name(library(syntax_highlight/'emacs-htmlfontify.el')), % TODO: find asset?
    SiteFileEl = ~ciao_mode_el,
    emacs_batch_call(
        ['-Q',
         '-l', SiteFileEl,
         '-l', HfyEl,
         Lang,
         Input,
         Output],
        [stdout(null), stderr(null), status(_)]).

% ---------------------------------------------------------------------------

:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).
:- use_module(library(stream_utils), [string_to_file/2, file_to_string/2]).

:- export(highlight_file_to_html_string/3).
:- pred highlight_file_to_html_string(+Lang, +Input, ?Output) :: lang * atm * string
   # "Produce HTML `Output` string with syntax highlight from `Input`
      file (see @pred{highlight_to_html/3})".

highlight_file_to_html_string(Lang, Input, Output) :-
    mktemp_in_tmp('highlight-out-XXXXXX', OutF),
    once_port_reify(emacs_htmlfontify(Lang, Input, OutF), Port),
    file_to_string(OutF, Output0), % TODO: may fail?
    del_file_nofail(OutF),
    port_call(Port),
    Output = ~str_remove_pre(Output0).

:- export(highlight_string_to_html_string/3).
:- pred highlight_string_to_html_string(+Lang, +Input, ?Output) :: lang * string * string
   # "Produce HTML `Output` string with syntax highlight from `Input`
      string (see @pred{highlight_to_html/3})".

% TODO: uses files! it can be quite slow

highlight_string_to_html_string(Lang, Input, Output) :-
    mktemp_in_tmp('highlight-in-XXXXXX', InF),
    mktemp_in_tmp('highlight-out-XXXXXX', OutF),
    string_to_file(Input, InF),
    once_port_reify(emacs_htmlfontify(Lang, InF, OutF), Port),
    file_to_string(OutF, Output0), % TODO: may fail?
    del_file_nofail(InF),
    del_file_nofail(OutF),
    port_call(Port),
    Output = ~str_remove_pre(Output0).

% Try remove <pre></pre> (we add ours later)
% TODO: not very nice (fix emacs elisp code instead?)
str_remove_pre(X) := Y :-
    append("\n<pre>"||Y0, "</pre>\n", X), !,
    Y = Y0.
str_remove_pre(X) := X.

% ---------------------------------------------------------------------------

:- use_module(library(pillow/html), [html_term/1, html2terms/2]).

:- export(highlight_file_to_html_term/3).
:- pred highlight_file_to_html_term(+Lang, +Input, ?Output) :: lang * atm * html_term
   # "Produce HTML `Output` term with syntax highlight from `Input`
      file (see @pred{highlight_to_html/3})".

highlight_file_to_html_term(Lang, Input, Output) :-
    highlight_file_to_html_string(Lang, Input, Output0),
    Output = ~html2terms(Output0).

% TODO: generalize/reuse process_channel.pl; as casting of streamed data

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
detect_language_('.el', 'lisp').
detect_language_('.sh', 'sh').
detect_language_('.bash', 'sh').
% detect_language_('.lpdoc', 'lpdoc').
% detect_language_('.md', 'markdown').
