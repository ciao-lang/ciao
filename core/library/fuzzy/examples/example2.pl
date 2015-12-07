:- module(example2,_,[fuzzy]).



mature_student(X,Mu):~ 
	{student(X)},
	 age_about_21(X,M2).

age_about_21(john,0.9):~ .
age_about_21(peter,0.4):~ .
age_about_21(pedro,2):~ .

student(john).
student(peter).

non_student_f :# fnot student_f/2.

student_f :# fuzzy student/1.
student_f(josua,0.5).
