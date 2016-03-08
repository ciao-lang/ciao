% (included file)
% Common paths and options

:- use_module(library(system), [get_pwnam/1, get_grnam/1]).

ciaofilepath_common := ~fsR(bundle_src(ciao)).
ciaofilepath_common := ~fsR(bundle_src(ciao)/doc/common).
ciaofilepath_common := ~fsR(bundle_src(ciao)/doc/readmes).

index := concept|lib|pred|prop|regtype|decl|author|global.
% index := prop.
% index := modedef.

% commonopts     := verbose.
% commonopts     := no_bugs.
commonopts :=
	modes|
	no_patches|
	no_isoline|
	no_engmods|
	propmods|
	no_changelog.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

startpage := 1.
papertype := afourpaper.

perms := perms(rwX, rwX, rX).

owner := ~get_pwnam.
group := ~get_grnam.

docformat := texi|ps|pdf|manl|info|html.

