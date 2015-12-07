:- package(functions).
?- error_in_lns(_,_,warning,
        'Deprecated package, please use fsyntax or functional instead').
:- use_package(fsyntax).
:- fun_eval arith(true).
