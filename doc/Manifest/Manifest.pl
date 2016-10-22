:- bundle(doc).
% General Ciao documentation
% (manuals and tutorials covering main bundles)
packname('CiaoDocumentation').
version('1.15.0').
requires([core, contrib, lpdoc]). % ciaopp?
%
alias_paths([
  % TODO: not used yet?
  ciao_docsrc = '.'
]).
