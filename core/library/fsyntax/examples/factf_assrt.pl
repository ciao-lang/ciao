:- module(_,_,[assertions,nativeprops,functional]).

:- pred fact(+int,-int) + is_det.
:- pred fact(-int,+int) + non_det.

fact(N) := N=0 ? 1
         | N>0 ? N * fact(--N).
