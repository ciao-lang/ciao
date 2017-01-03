:- bundle(core).
% Manifest file for Ciao core (engine, compiler, basic libraries,
% extra libraries)
packname('Core').
alias_paths([
    core_docsrc = 'doc',
    core_tests = 'tests',
    %
    core_cmds = 'cmds',
    %
%    engine = 'engine',
    %
    rtchecks = 'lib/rtchecks',
    unittest = 'lib/unittest',
    predefres = 'lib/resdefs/predefres',
    res_nargs = 'lib/resdefs/predefres/res_nargs',
    res_steps = 'lib/resdefs/predefres/res_steps',
    predefprf = 'lib/resdefs/predefprf',
    prf_ticks = 'lib/resdefs/predefprf/prf_ticks',
    prf_costcenter = 'lib/resdefs/predefprf/prf_costcenter'
]).
readme('INSTALLATION', [main='doc/common/INSTALLATION_CIAO.lpdoc']).
readme('INSTALLATION_Win32', [main='doc/common/INSTALLATION_CIAO_Win32.lpdoc']).
readme('README', [main='doc/common/README_CIAO.lpdoc']).
readme('CHANGELOG', [main='doc/common/CHANGELOG_CIAO.pl']).
