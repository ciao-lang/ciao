proc disable_next {} {
    .next config -state disabled
}
proc disable_back {} {
    .back config -state disabled
}

proc enable_next {} {
    .next config -state active
}
proc enable_back {} {
    .back config -state active
}

# clear the whole window by clearing each widgets
proc clear_board_menu { } {
    destroy .name
    destroy .store
    destroy .edit
    destroy .quit
}

proc clear_store { } {
    global entryname
    global entrytele

    set entryname ""
    set entrytele ""
    destroy .menu
    destroy .save
    destroy .quit
    destroy .name
    destroy .ename
    destroy .telephone
    destroy .etelephone
}
proc clear_name_recall { } {
    global entryname
    global entrytele

    set entryname ""
    set entrytele ""
    destroy .menu
    destroy .next
    destroy .back
    destroy .quit
    destroy .name
    destroy .ename
    destroy .telephone
    destroy .etelephone
}

# clear the whole window by clearing each widgets
proc set_data { } {
    global x
    global entryname
    global entrytele
    global listnames
    global listtelephones

#    puts $x

    set length [llength $listnames]
    incr length -1
    if {$x == $length} { 
	disable_next
    } else { 
	enable_next 
    }
    if {$x == 0} { 
	disable_back
    } else {
	enable_back 
    }

    set entryname [lindex $listnames $x]
    set entrytele [lindex $listtelephones $x]
    
}

# set the search window
proc set_search_menu { names telephones } {

    global x
    global entryname
    global entrytele
    global listnames
    global listtelephones

    label .name -text Name: 
    entry .ename -textvariable entryname
    pack .name .ename
    label .telephone -text Telephone: 
    entry .etelephone -textvariable entrytele
    pack .telephone .etelephone

    set x -1
    set listnames $names
    set listtelephones $telephones

    # button for sending a 'next' message
    button .next -text "next" -command { incr x; set_data}
    pack .next

    # button for sending a 'next' message
    button .back -text "back" -command { incr x -1; set_data}
    pack .back

    # button for sending a 'store' message
    button .quit -text quit -command {prolog_event quit}
    pack .quit

    button .menu -text menu -command {prolog_event menu}
    pack .menu

    set length [llength $listnames]
    incr length -1
    if {$x == $length} { 
	disable_next
    } else { 
	enable_next 
    }
    if {$x == -1} { 
	disable_back
    } else {
	enable_back 
    }

}

# set the search window
proc set_store_menu { } {

    global entryname
    global entrytele

    label .name -text Name: 
    entry .ename -textvariable entryname
    pack .name .ename
    label .telephone -text Telephone: 
    entry .etelephone -textvariable entrytele
    pack .telephone .etelephone

    # button for sending a 'save' message
    button .save -text "save" -command {prolog_event phone_book_store($entryname,$entrytele)}
    pack .save

    # button for sending a 'store' message
    button .quit -text quit -command {prolog_event quit}
    pack .quit

    button .menu -text menu -command {prolog_event menu}
    pack .menu

}

# given a solution as a list of queens in column positions
# place each queen on the board
proc show_solution_search { names telephones } {
    set_search_menu $names $telephones
}

proc show_solution_store { } {
    set_store_menu 
}

proc show_menu { } {
# button for sending a 'name' message
button .name -text "name recall" -command {prolog_event name}
pack .name

# button for sending a 'store' message
button .store -text store -command {prolog_event store}
pack .store

# button for sending a 'edit' message
button .edit -text edit -command {prolog_event edit}
#pack .edit

# button for sending a 'edit' message
button .quit -text quit -command {prolog_event quit}
pack .quit
}
