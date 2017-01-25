:- bundle(builder).
version('1.4.0').
depends([core]).
alias_paths([ciaobld = 'src']).
%
cmd('ciao_builder', [main='cmds/ciao_builder']).
%
lib('src').
lib('sh_src'). % TODO: not for windows!
%
manual('ciao_builder', [main='doc/SETTINGS.pl']).
