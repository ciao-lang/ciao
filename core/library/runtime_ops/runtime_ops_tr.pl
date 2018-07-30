:- module(runtime_ops_tr, [runtime_op/2], [assertions]).

runtime_op((:- Op), RuntimeOp) :-
	Op = op(_, _, _),
	RuntimeOp = [(:- Op), (:- initialization(Op))].
