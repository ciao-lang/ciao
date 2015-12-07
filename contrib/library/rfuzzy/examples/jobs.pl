:- module(jobs,_,[rfuzzy, clpr]).

position(consultant).
position(systems_analyst).
position(developer).
position(programmer).
position(teacher).

rfuzzy_type_for(interest/1, [position/1]).
rfuzzy_default_value_for(interest/1, 0.1).
interest(consultant) value 0.6 .
interest(systems_analyst) value 0.8 .
interest(developer) value 0.6 .
interest(programmer) value 0.4.
interest(teacher) value 0.4.

rfuzzy_type_for(distance/1, [position/1]).
rfuzzy_default_value_for(distance/1, 0.1).
distance(consultant) value 0.4.
distance(systems_analyst) value 0.1 .
distance(developer) value 0.5 .
distance(programmer) value 0.5.
distance(teacher) value 0.85.

rfuzzy_type_for(salary/1, [position/1]).
rfuzzy_default_value_for(salary/1, 0.1).
salary(consultant) value 0.8.
salary(systems_analyst) value 0.9 .
salary(developer) value 0.6 .
salary(programmer) value 0.5.
salary(teacher) value 0.3.

rfuzzy_type_for(future_development/1, [position/1]).
rfuzzy_default_value_for(future_development/1, 0.1).
future_development(consultant) value 0.5.
future_development(systems_analyst) value 0.3 .
future_development(developer) value 0.8 .
future_development(programmer) value 0.7.
future_development(teacher) value 0.5.

rfuzzy_type_for(job_offer/1, [position/1]).
(job_offer(J) cred (min,0.8)) :~ prod((interest(J), distance(J), salary(J), future_development(J))).
