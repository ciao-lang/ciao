label .l -textvariable tvar
button .b -text "Go!" -command {run_and_show}
pack .l .b -side top

proc run_and_show {} {

    global prolog_variables
    global tvar 

    prolog hello(X)
    set tvar $prolog_variables(X)
}
