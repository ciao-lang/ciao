% General Ciao documentation
% (manuals and tutorials covering main bundles)
bundle_name(doc).
bundle_packname('CiaoDocumentation').
bundle_version('1.15').
bundle_patch('0').
bundle_requires([core, contrib, lpdoc]). % ciaopp?
%
bundle_alias_paths([
  % TODO: not used yet?
  ciao_docsrc = '.'
]).
