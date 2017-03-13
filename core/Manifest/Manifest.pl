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
readme('INSTALLATION', [main='doc/common/INSTALLATION_CIAO.lpdoc']).
readme('INSTALLATION_Win32', [main='doc/common/INSTALLATION_CIAO_Win32.lpdoc']).
readme('README', [main='doc/common/README_CIAO.lpdoc']).
readme('CHANGELOG', [main='doc/common/CHANGELOG_CIAO.pl']).
