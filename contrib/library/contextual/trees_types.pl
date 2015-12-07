:- use_package( contextual).

:- types( object).
:- types( collection).

:- default_module( trees).

:- def_context abstract: 'Tree'( _)::(*).

:- apply_interface object( 'Tree'( _)).
:- apply_interface collection( 'Tree'( _)).

:- def_context 'EmptyTree'( E)::'Tree'( E).
:- def_context  'TreeNode'( E)::'Tree'( E) + 
	{n( left::'Tree'(E), pivot::E, right::'Tree'(E))}.

