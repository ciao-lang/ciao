:- package(doccomments).

% Ensure that assertions are enabled.
:- use_package(assertions).
% Tell LPdoc that contents of @include need to be translated from markdown.
:- doc(pragma, partially_parsed_markdown).

% NOTE: this package must be executed after 'condcomp' and before any
%       other expansions (otherwise it could inject fake facts in the
%       program).
:- load_compilation_module(library(doccomments/doccomments_tr)).
:- add_sentence_trans(doccomments_tr:doccomments_sentence/3, 150).
:- add_term_trans(doccomments_tr:doccomments_term/3, 150).

% Tell the reader to parse '%!' as special sentences.
:- set_prolog_flag(doccomments, on).

