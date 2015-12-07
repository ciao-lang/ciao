:- use_package( contextual).

:- def_interface collection( Class) =
	[
	    member/1 // +Class,
	    ins/1 // Class,
	    interpolate/0 // (+Class, difflist)
	].

