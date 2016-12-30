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
readme('INSTALLATION', [main='common/INSTALLATION_CIAO.lpdoc']).
readme('INSTALLATION_Win32', [main='common/INSTALLATION_CIAO_Win32.lpdoc']).
readme('README', [main='common/README_CIAO.lpdoc']).
readme('CHANGELOG', [main='common/CHANGELOG_CIAO.pl']).
