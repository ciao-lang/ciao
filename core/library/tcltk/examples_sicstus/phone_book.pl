:- use_module(library(tcltk)).
     
     telephone(fred, '123-456').
     telephone(wilbert, '222-2222').
     telephone(taxi, '200-0000').
     telephone(mary, '00-36-1-666-6666').
     
     go :-
          tk_new([name('Example 2')], T),
          tcl_eval(T, 'entry .name -textvariable name',_),
          tcl_eval(T, 'button .search -text search -command {
                           prolog telephone($name,X);
                           set result $prolog_variables(X) }', _),
          tcl_eval(T, 'label .result -relief raised -textvariable result', _),
          tcl_eval(T, 'pack .name .search .result -side top -fill x', _),
          tk_main_loop.
