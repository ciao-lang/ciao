:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title, "Filed predicates").
:- doc(author, "Francisco Bueno").
:- doc(module,"This package allows using files as a ``@concept{cache}''
   for predicates defined by facts. This is useful for huge tables of
   facts that may push the memory limits of the system too far. Goals of a
   @concept{filed predicate} are executed simply by reading from the
   corresponding file. 

   Anything in the DB file used for the predicate that is different from a
   fact for the corresponding predicate is ignored. Each call
   to a filed predicate forces opening the file, so the use
   of this package is subject to the limit on the number of open files
   that the system can support.

   Dynamic modification of the filed predicates is also allowed during 
   execution of the program. Thus filed predicates are regarded as dynamic,
   data predicates residing in a file. However, dynamic modifications to
   the predicates do not affect the file, unless the predicate is also
   declared persistent.

   The package is compatible with @lib{persdb} in the sense that a predicate
   can be made both filed and persistent. In this way, the predicate can be
   used in programs, but it will not be loaded (saving memory), can also be
   modified during execution, and modifications will persist in the
   file. Thus, the user interface to both packages is the same (so the
   DB file must be one for both filing and persistency). ").

:- doc(usage,"This facility is used as a package, thus either including 
   @lib{factsdb} in the package list of the module, or by using the
   @decl{use_package/1} declaration. The facility predicates are defined
   in library module @lib{factsdb_rt}.").

:- use_package(library(factsdb)).

:- doc(bug,"The DB files for persistent predicates have to be used
	as such from the beginning. Using a DB file for a filed predicate
	first, and then using it also when making the predicate persistent
	won't work. Nor the other way around: using a DB file for a 
	persistent predicate first, and then using it also when making 
        the predicate filed.").

