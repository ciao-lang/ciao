:- bundle(builder).
version('1.20.0').
depends([core]).
alias_paths([ciaobld = 'src']).
%
cmd('ciao_builder', [main='cmds/ciao_builder']).
cmd('ciao_publish', [main='cmds/ciao_publish']).
%
lib('src').
lib('sh_src'). % TODO: not for windows!
%
manual('ciao_builder', [main='doc/SETTINGS.pl']).

