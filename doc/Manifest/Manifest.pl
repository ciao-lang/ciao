:- bundle(doc).
% General Ciao documentation
% (manuals and tutorials covering main bundles)
packname('CiaoDocumentation').
version('1.15.0').
depends([core, contrib, lpdoc]). % ciaopp?
%
alias_paths([
  % TODO: not used yet?
  ciao_docsrc = '.'
]).
%
manual('ciao', [main='reference/SETTINGS.pl']).
manual('ciao_devel', [main='developers/SETTINGS.pl']).
readme('INSTALLATION', [main='common/INSTALLATION_CIAO']).
readme('INSTALLATION_Win32', [main='common/INSTALLATION_CIAO_Win32']).
readme('README', [main='common/README_CIAO']).
