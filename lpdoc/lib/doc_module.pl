:- package(doc_module).
% (implementing an interface for doc_module)
% Use this package in module implementing new LPdoc commands
% (see load_doc_module/1 at doccfg_defs.pl)

:- discontiguous doc_cmd_type/1.
:- multifile doc_cmd_type/1.
:- discontiguous doc_cmd_rw/2.
:- multifile doc_cmd_rw/2.
