label .l -textvariable tvar
button .b -text "Go!" -command {run}
pack .l .b -side top

proc run {} {

    global prolog_variables
    global tvar 

    prolog hello(X)
    set tvar $prolog_variables(X)
}
