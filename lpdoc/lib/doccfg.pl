:- package(doccfg).

% Package to define a LPdoc manual configuration settings
:- use_package(fsyntax).
:- use_package(assertions).
:- use_package(regtypes).

:- use_module(library(bundle/paths_extra), [fsR/2]).

% ---------------------------------------------------------------------------
% (the interface for LPdoc settings)

% TODO: use proper interfaces
:- doc(nodoc, '$implements'/1).
:- export('$implements'/1).
'$implements'('doccfg').

:- use_module(lpdoclib(doccfg_props)).
:- include(lpdoclib(doccfg_defs)).

:- load_compilation_module(lpdoclib(doccfg_tr)).
:- add_sentence_trans(doccfg_tr:doccfg_sent/3, 8110).

% ----------------------------------------------------------------------------

:- default_def(datamode/1, [(datamode(_) :- fail)]).
:- default_def(execmode/1, [(execmode(_) :- fail)]).
:- default_def(bibfile/1, [(bibfile(_) :- fail)]).
:- default_def(htmldir/1, [(htmldir(_) :- fail)]).
:- default_def(docdir/1, [(docdir(_) :- fail)]).
:- default_def(infodir/1, [(infodir(_) :- fail)]).
:- default_def(mandir/1, [(mandir(_) :- fail)]).
:- default_def(output_name/1, [(output_name(_) :- fail)]).

:- default_def(doc_mainopts/1, [doc_mainopts(no_bugs)]).
:- default_def(doc_mainopts/1, [doc_mainopts(no_patches)]).
:- default_def(doc_compopts/1, [doc_compopts(no_bugs)]).
:- default_def(doc_compopts/1, [doc_compopts(no_patches)]).

:- default_def(index/1, [
    index(concept),
    index(lib),
    index(pred),
    index(prop),
    index(regtype),
    index(decl),
    index(author),
    index(global)
]).

:- default_def(startpage/1, [
    startpage(1)
]).

:- default_def(papertype/1, [
    papertype(afourpaper)
]).

:- default_def(libtexinfo/1, [
    libtexinfo(yes)
]).

:- default_def(docformat/1, [
    docformat(texi),
    docformat(ps),
    docformat(pdf),
    docformat(manl),
    docformat(info),
    docformat(html)
]).

% ----------------------------------------------------------------------------
% (End of interface)
