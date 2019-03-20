:- package(callback).
:- import(c_itf, [add_module_check/1, module_error/0, ensure_imported/4]).

:- redefining(add_module_check/1).

:- meta_predicate add_module_check(pred(1)).

add_module_check(P) :- c_itf:add_module_check(P).
