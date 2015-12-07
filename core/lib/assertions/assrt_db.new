:- module(assrt_db,
	[ assertion_read/9,
	  assertion_body/7
	],
	[ assertions
	]).

:- use_module(library('assertions/assertions_props')).
:- use_module(library('assertions/c_itf_props')).

%% ---------------------------------------------------------------------------
:- pred assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)

:: ( moddesc(M), assrt_status(Status), assrt_type(Type),
     assrt_body(Body), dictionary(Dict), int(LB), filename(Source),
     int(LE) )

# "Each fact represents that an assertion for @var{Goal} has been read
   in module @var{M}, which has status @var{Status} and is of type
   @var{Type}.  @var{Body} is the actual body of the
   assertion. @var{Dict} contains the names of the variables which
   appear in the assertion. @var{Source} is the file in which the
   assertion appears (treats included files correctly). @var{LB} and
   @var{LE} are the first and last line numbers in this source file in
   which the assertion appears (if the source is not available or has
   not been read @var{LB}=@var{LE}=0).  @var{Goal} may be normalized
   or not, i.e., it may contain modes or properties, but it is always
   a term of the same functor and arity as the predicate it represents
   (i.e., it is not in Functor/Arity format). @var{Body} is always
   normalized (but the properties or property conjunctions inside may
   not -- see @pred{normalize_assertions_pass_one/1} and
   @pred{normalize_assertions_pass_two/1}.".
%% ---------------------------------------------------------------------------

:- data assertion_read/9. 

assertion_body(Pred,Compat,Call,Succ,Comp,Comm,
	      (Pred::Compat:Call=>Succ+Comp#Comm)).

