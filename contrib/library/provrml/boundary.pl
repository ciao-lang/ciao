:- module(boundary,[boundary_check/3,
	            boundary_rotation_first/2,
		    boundary_rotation_last/2,
	            reserved_words/1,
                    children_nodes/1],
		    [assertions,isomodes,regtypes,iso]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").

%:- use_module(library(iso_char)).
%:- use_module(library(basicprops)).

:- use_module(library(provrml/internal_types)).
:- use_module(library(provrml/provrmlerror)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(module,"This module offers predicate to check values according to 
              their boundaries and offers lists of possible node ascendents.").


%:- set_prolog_flag(multi_arity_warnings, off).
:- discontiguous([boundary_check_min/3, boundary_check_max/3]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred children_nodes(-List)
   :: list(atm)
    # "Returns a list of all nodes possible as children nodes.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

children_nodes(['Anchor','Background','Billboard','Collision',
                'ColorInterpolator','CoordinateInterpolator',
		'CylinderSensor','DirectionalLight','Fog',
		'Group','Inline','LOD','NavigationInfo','NormalInterpolator',
		'OrientationInterpolator','PlaneSensor','PointLight',
		'PositionInterpolator','ProximitySensor','ScalarInterpolator',
		'Script','Shape','Sound','SpotLight','SphereSensor',
		'Switch','TimeSensor','TouchSensor','Transform',
		'Viewpoint','VisibilitySensor','WorldInfo']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reserved_words(-List)
   :: list(atm)
    # "Returns a list with the reserved words, words prohibited to use 
       in cases not appropiated.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reserved_words(['DEF','EXTERNPROTO','FALSE','IS','NULL','PROTO','ROUTE',
	 'TO','TRUE','USE',eventIn,eventOut,exposedField,field]).
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred geometry_nodes(-List)
   :: list(atm)
    # "Returns the list of all the geographical nodes, i.e. the nodes that 
       can come after the 'Shape' field 'geometry'.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

geometry_nodes(['Box','Cone','Cylinder','ElevationGrid','Extrusion',
	        'IndexedFaceSet','IndexedLineSet','PointSet','Sphere','Text']).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred limit(+Value_name,-True_value)
	:: atm * number
        # "From a given number or name returns the corresponding number. 
           The values stated here are the corresponding bound values given 
           in the dictionary. There should not exist values given there and
           not here in the limits, thus result in an error.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

limit(-1,-1).

limit(0,0).

limit(1,1).

limit(pi_div2,Value) :-
	Value = 1.57079632679489.

limit(pi,Value) :-
	Value = 3.14159265358979.

limit(pi_mult2,Value) :-
	Value = 6.28318530717958.

limit(pi_mult2_neg,Value) :-
	Value = -6.28318530717958.

limit(inf,Value) :-
	Value = inf.

limit(inf_neg, Value) :-
	Value = inf_neg.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred boundary_check(+Value_to_check,+Init_value,+Bound)
:: atm * list(atm) * bound
# "This predicate check the boundaries of the given value according to the 
   boudary values. If the value is wrong according to the boundaries,
   the value is checked according to the initial value given. If the value 
   continues to be wrong, an error will be raised accordingly.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

boundary_check(_Val,_Init,[]).	


boundary_check(Value, Initial_value,bound(Min,Max)) :-
	boundary_check_min(Value, Min, Initial_value),
	boundary_check_max(Value, Max, Initial_value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred boundary_check_min(+Value_to_check,+Lower_bound,+Init_value)
:: atm * atm * bound
# "This predicate check the lower boundary of the given value according to the 
   boudary value. If the value is wrong according to the boundary,
   the value is checked according to the initial value given. If the value 
   continues to be wrong, an error will be raised.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

boundary_check_min(_Value, excl(inf_neg), _Init).


boundary_check_min(Value, incl(Lim), _Init) :-
	limit(Lim, Limit),
	Value >= Limit,
	!.

boundary_check_min(Value, incl(inf_neg), _Init) :-
	error_vrml(inf(Value)).

boundary_check_min(Value, excl(Lim), _Init) :-
	limit(Lim, Limit),
	Value > Limit,
	!.

boundary_check_min(Value, _Boundary, [Initial_value|_Rest]) :-
	Value == Initial_value,
	!.

boundary_check_min(Value, incl(Lim), _Init) :-
	limit(Lim,Limit),
	error_vrml(min_incl(Value,Limit)).

boundary_check_min(Value, excl(Lim), _Init) :-
	limit(Lim, Limit),
	error_vrml(min_excl(Value,Limit)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred boundary_check_max(+Value_to_check,+Upper_bound,+Init_value)
:: atm * atm * bound
# "This predicate check the upper boundary of the given value according to the 
   boudary value. If the value is wrong according to the boundary,
   the value is checked according to the initial value given. If the value 
   continues to be wrong, an error will be raised.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
boundary_check_max(_Value, excl(inf), _Init).

boundary_check_max(Value, incl(Lim),_Init) :-
	limit(Lim, Limit),
	Value =< Limit,
	!.

boundary_check_max(Value, incl(inf), _Init) :-
	error_vrml(inf(Value)).

boundary_check_max(Value, excl(Lim),_Init) :-
	limit(Lim, Limit),
	Value < Limit,
	!.

boundary_check_max(Value, _Boundary, [Initial_value|_Rest]) :-
	Value == Initial_value,
	!.

boundary_check_max(Value, incl(Lim), _Init) :-
	limit(Lim,Limit),
	error_vrml(max_incl(Value,Limit)).

boundary_check_max(Value, excl(Lim), _Init) :-
	limit(Lim, Limit),
	error_vrml(max_excl(Value,Limit)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred boundary_rotation_first(+Bound_double,-Bound)
	:: bound_double * bound
        # "The predicate will extract the first bounds from a double
           bound.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

boundary_rotation_first(bound(Min,Max,_,_),bound(Min,Max)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred boundary_rotation_last(+Bound_double,-Bound)
	:: bound_double * bound
        # "The predicate will extract the last bounds from a double
           bound.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
boundary_rotation_last(bound(_,_,Min,Max),bound(Min,Max)).
