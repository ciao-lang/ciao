% (Included from engine(internals), keep it small and simple!)

:- doc(section, "Bundle registry database").

% Definition of bundles (as stored in the bundleregs)

:- export('$bundle_id'/1).
:- data '$bundle_id'/1.
% '$bundle_srcdir'(Bundle, Path): path to bundle source
:- export('$bundle_srcdir'/2).
:- data '$bundle_srcdir'/2.
% '$bundle_alias_path'(Alias, Bundle, Path)
:- export('$bundle_alias_path'/3).
:- data '$bundle_alias_path'/3.
% Other bundle properties. Currently supported:
%   requires(Bundles)
%   packname(Packname)
% TODO: Add optional pretty_name and archive_name
:- export('$bundle_prop'/2).
:- data '$bundle_prop'/2.

clean_bundlereg_db :-
	retractall_fact('$bundle_id'(_)),
	retractall_fact('$bundle_prop'(_, _)),
	retractall_fact('$bundle_srcdir'(_, _)),
	retractall_fact('$bundle_alias_path'(_, _, _)).
