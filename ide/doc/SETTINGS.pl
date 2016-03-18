:- module(_, _, [lpdoclib(doccfg)]).

:- doc(title, "Settings for Ciao IDE manual").
:- doc(author, "Jose F. Morales").

datamode(_) :- fail.
execmode(_) :- fail.

% (not customized)
bibfile(_) :- fail.
htmldir(_) :- fail.
docdir(_) :- fail.
infodir(_) :- fail.
mandir(_) :- fail.

filepath := ~fsR(bundle_src(ide)/doc).

output_name := _ :- fail.

doc_structure := 'ide'.

commonopts := no_bugs|no_patches.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

index := concept|lib|pred|prop|regtype|decl|author|global.

startpage := 1.

papertype := afourpaper.

libtexinfo := 'yes'.

docformat := pdf|manl|info|html.

