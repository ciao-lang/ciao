:- bundle(alldocs).
% General Ciao documentation
% (manuals and tutorials covering main bundles)
version('1.15.0').
depends([core, contrib, lpdoc, ciao_emacs]). % ciaopp?
%
alias_paths([
  ciao_docsrc = '.'
]).
%
manual('ciao', [main='reference/SETTINGS.pl']).
manual('ciao_devel', [main='developers/SETTINGS.pl']).
