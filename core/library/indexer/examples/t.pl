:- module(t, [foo/4], [indexer]).

:- index foo(+,?,*,i), foo(?,?,?,i).

foo(a,b,c(d),9).
foo(e,f,g(h),11) :- foo(a,b,c(d),9).
foo(_,z,_,_) :- baz.

baz.
