:- module(regtypes_tr, [expand_regtypes/2],
	    [assertions, nortchecks, isomodes]).

:- doc(title, "Regular type definition support").

%% ------------------------------------------------------------------------

%% If a '+' field is present by not recognizing it here as is_type_decl 
%% we simply leave it as is! (old?)
expand_regtypes((:- regtype((T # C))),    (:- prop((T + regtype # C)))).
expand_regtypes((:- regtype(S, (T # C))), (:- prop(S, (T + regtype # C)))).
expand_regtypes((:- regtype(T)),          (:- prop(T + regtype))).
expand_regtypes((:- regtype(S, T)),       (:- prop(S, T + regtype))).
