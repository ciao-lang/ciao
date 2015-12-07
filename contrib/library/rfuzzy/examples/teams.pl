:- module(teams,_,[rfuzzy, clpr]).

type_Candidate(john).
type_Candidate(ken).
type_Candidate(bob).
type_Candidate(ann).

rfuzzy_type_for(efficency/1, [type_Candidate/1]).
rfuzzy_type_for(likes_partner/2, [type_Candidate/1, type_Candidate/1]).
rfuzzy_type_for(knowledge/1, [type_Candidate/1]).

rfuzzy_default_value_for(efficency/1, 0.65).
rfuzzy_default_value_for(likes_partner/2, 0.65).
rfuzzy_default_value_for(knowledge/1, 0.65).

likes_partner(john,ken) value 0.3.
likes_partner(john,bob) value 0.9.
likes_partner(john,ann) value 0.7.
likes_partner(ken,ann) value 0.2.
likes_partner(bob,john) value 0.5.
likes_partner(bob,ann) value 0.8.
likes_partner(ann,ken) value 1.

rfuzzy_type_for(get_along/2, [type_Candidate/1, type_Candidate/1]).
rfuzzy_default_value_for(get_along/2, 0.1).
get_along(W1,W2) :~ min((likes_partner(W1,W2),likes_partner(W2,W1))).

rfuzzy_type_for(prepared_team/2, [type_Candidate/1, type_Candidate/1]).
rfuzzy_default_value_for(prepared_team/2, 0.1).
prepared_team(W1,W2) :~ prod((efficency(W1),efficency(W2),knowledge(W1),knowledge(W2))).

rfuzzy_type_for(successful_team/2, [type_Candidate/1, type_Candidate/1]).
rfuzzy_default_value_for(successful_team/2, 0.1).
(successful_team(W1,W2) cred(max,0.5)) :~ dluka((get_along(W1,W2),prepared_team(W1,W2))).

efficency(john) value 0.5.
efficency(ann) value 0.85.

knowledge(ann) value 0.8.
knowledge(bob) value 0.4.
