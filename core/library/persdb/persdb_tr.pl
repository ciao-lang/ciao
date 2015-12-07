:- module(persdb_tr, [persistent_tr/3], []).

persistent_tr((:- persistent(F/A,D)),
	[(:- data(F/A)),
	 '$is_persistent'(F/A,M:D)], M).
persistent_tr(persistent_dir(D,Dir),
	[persistent_dir(M:D,Dir,default,default)], M).
persistent_tr((persistent_dir(D,Dir) :- Body),
	[(persistent_dir(M:D,Dir,default,default) :- Body)], M).

persistent_tr(persistent_dir(D,Dir,DirPerms,FilePerms),
	[persistent_dir(M:D,Dir,DirPerms,FilePerms)], M).
persistent_tr((persistent_dir(D,Dir,DirPerms,FilePerms) :- Body),
	[(persistent_dir(M:D,Dir,DirPerms,FilePerms) :- Body)], M).
