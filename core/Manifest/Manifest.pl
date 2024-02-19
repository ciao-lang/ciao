:- bundle(core).
% Manifest file for Ciao core (engine, compiler, basic libraries,
% extra libraries)
alias_paths([
    core_docsrc = 'doc',
    core_tests = 'tests',
    %
    core_cmds = 'cmds',
    %
%    engine = 'engine',
    %
    rtchecks = 'lib/rtchecks'
]).
readme('CHANGELOG', [main='doc/common/CHANGELOG_CIAO.pl']).
%
manual('ciao_internals', [main='doc/internals/SETTINGS.pl']).
