:- package(contextual).
:- use_package(hiord).

:- use_module(library(contextual/contextual_rt)).

:- op( 1150, fx,  def_context).
:- op( 1150, fx,  def_interface).
:- op( 1150, fx,  apply_interface).
:- op( 1150, fx,  undef_context).
:- op(  250, xfy, (::)).

:- load_compilation_module(library(contextual/contextual_tr)).
:- add_sentence_trans(contextual_tr:contextual_sentence_tr/3, 750). % TODO: Right priority?

:- multifile '$_context_method'/5.
:- discontiguous '$_context_method'/5.

:- multifile supports_interface/2.
:- discontiguous supports_interface/2.

