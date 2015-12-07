:- module(which_row,_,[rfuzzy, clpr]).

rows_list(row1).
rows_list(row2).
rows_list(row3).
rows_list(row4).

rfuzzy_type_for(people/1, [rows_list/1]).
rfuzzy_default_value_for(people/1, 0.1).

people(row1) value 0.6.
people(row2) value 0.15.
people(row3) value 0.95.
people(row4) value 0.7.

rfuzzy_type_for(products_people/1, [rows_list/1]).
rfuzzy_default_value_for(products_people/1, 0.5).

products_people(row2) value 0.2.
products_people(row4) value 0.8.

rfuzzy_type_for(which_row/1, [rows_list/1]).
rfuzzy_default_value_for(which_row/1, 0.1).
(which_row(J) cred (luka,0.9)) :~ prod((people(J), products_people(J))).
