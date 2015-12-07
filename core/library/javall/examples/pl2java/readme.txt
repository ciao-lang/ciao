Prolog-to-java interface examples
=================================


calculator.pl
-------------

This example uses the Prolog-to-Java interface to show a calculator
keyboard and allows the user to type in numbers and operations in the same
way a calculator does. The calculations are done in the Prolog side, while
the representation of the calculator on the screen is done in the Java
side. No Java code is needed: the Prolog side just uses the Java awt API.

To start this example start the prolog top-level and type:

CIAO 1.3 #47: Wed Aug 18 17:06:46 MEST 1999
?- use_module(calculator).

yes
?- calculator.


or simply compile calculator.pl to an executable and run it.

Java compiler/interpreter must be accesible at run-time.

