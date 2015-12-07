%
% example to show low-level java interface capabilities.
% On this example can be seen the following features:
%  - java object creation
%  - java method invocation
%  - handling of java events from prolog
%
% This example shows a calculator pad that displays on the calculator
% screen the keys clicked. On the java side are used the jdk standard
% classes, controlled by the prolog side. The action events are also
% controlled by the prolog side via event-handling predicates, posted
% on the java event queue listener through the interface primitive
% java_add_listener/3.
%
% To run this example, just use this module from the top-level and
% type calculator.
%

:- module(calculator,[]).

:- use_module(library(javall/javasock)).
:- use_module(library(javall/javart)).
:- use_module(library(lists)).
:- export(calculator/0).
:- export(main/0).

:- dynamic acumulator/1.
:- dynamic operator/1.
:- dynamic number_editing/1.

acumulator(0).
operator("+").
number_editing(false).

main:-
	calculator,
	pause(3600).

calculator:-
	java_start,

%% Frame.
	java_create_object('java.awt.Frame'('Prueba'),Frame),
	java_invoke_method(Frame,resize(300,300,_)),
	java_invoke_method(Frame,setLocation(1,1,_)),

%% Layout.
	java_create_object('java.awt.BorderLayout',Layout),
	java_invoke_method(Frame,setLayout(Layout,_)),

%% Display.
        java_create_object('java.awt.TextField',Display),
        java_invoke_method(Frame,add('North',Display,_)),

%% Buttons grid.
        java_create_object('java.awt.Container',BtnContainer),
        java_create_object('java.awt.GridLayout'(4,4),BtnLayout),
        java_create_object('java.awt.Button'('1'),Btn1),
        java_create_object('java.awt.Button'('2'),Btn2),
        java_create_object('java.awt.Button'('3'),Btn3),
        java_create_object('java.awt.Button'('4'),Btn4),
        java_create_object('java.awt.Button'('5'),Btn5),
        java_create_object('java.awt.Button'('6'),Btn6),
        java_create_object('java.awt.Button'('7'),Btn7),
        java_create_object('java.awt.Button'('8'),Btn8),
        java_create_object('java.awt.Button'('9'),Btn9),
        java_create_object('java.awt.Button'('0'),Btn0),
        java_create_object('java.awt.Button'('+'),BtnAdd),
        java_create_object('java.awt.Button'('*'),BtnMult),
        java_create_object('java.awt.Button'('/'),BtnDiv),
        java_create_object('java.awt.Button'('-'),BtnSub),
        java_create_object('java.awt.Button'('='),BtnEquals),
        java_create_object('java.awt.Button'('Exit'),BtnEnd),

        java_invoke_method(Frame,add('Center',BtnContainer,_)),
        java_invoke_method(BtnContainer,setLayout(BtnLayout,_)),
        java_invoke_method(BtnContainer,add(Btn1,_)),
        java_invoke_method(BtnContainer,add(Btn2,_)),
        java_invoke_method(BtnContainer,add(Btn3,_)),
        java_invoke_method(BtnContainer,add(BtnAdd,_)),

        java_invoke_method(BtnContainer,add(Btn4,_)),
        java_invoke_method(BtnContainer,add(Btn5,_)),
        java_invoke_method(BtnContainer,add(Btn6,_)),
        java_invoke_method(BtnContainer,add(BtnSub,_)),

        java_invoke_method(BtnContainer,add(Btn7,_)),
        java_invoke_method(BtnContainer,add(Btn8,_)),
        java_invoke_method(BtnContainer,add(Btn9,_)),
        java_invoke_method(BtnContainer,add(BtnMult,_)),

        java_invoke_method(BtnContainer,add(BtnEnd,_)),
        java_invoke_method(BtnContainer,add(Btn0,_)),
        java_invoke_method(BtnContainer,add(BtnEquals,_)),
        java_invoke_method(BtnContainer,add(BtnDiv,_)),

%% Event listeners.
        java_add_listener(Btn1,'java.awt.event.ActionEvent',
	       appendText("1",Display)),
        java_add_listener(Btn2,'java.awt.event.ActionEvent',
	       appendText("2",Display)),
        java_add_listener(Btn3,'java.awt.event.ActionEvent',
	       appendText("3",Display)),
        java_add_listener(BtnAdd,'java.awt.event.ActionEvent',
	       compute("+",Display)),

        java_add_listener(Btn4,'java.awt.event.ActionEvent',
	       appendText("4",Display)),
        java_add_listener(Btn5,'java.awt.event.ActionEvent',
	       appendText("5",Display)),
        java_add_listener(Btn6,'java.awt.event.ActionEvent',
	       appendText("6",Display)),
        java_add_listener(BtnSub,'java.awt.event.ActionEvent',
	       compute("-",Display)),

        java_add_listener(Btn7,'java.awt.event.ActionEvent',
	       appendText("7",Display)),
        java_add_listener(Btn8,'java.awt.event.ActionEvent',
	       appendText("8",Display)),
        java_add_listener(Btn9,'java.awt.event.ActionEvent',
	       appendText("9",Display)),
        java_add_listener(BtnMult,'java.awt.event.ActionEvent',
	       compute("*",Display)),

        java_add_listener(BtnEnd,'java.awt.event.ActionEvent',
	       termination(Frame,[Layout, Display, BtnContainer, BtnLayout,
	       Btn1, Btn2, Btn3, Btn4, Btn5, Btn6, Btn7, Btn8, Btn9, Btn0,
	       BtnAdd, BtnMult, BtnDiv, BtnSub, BtnEquals, BtnEnd])),

        java_add_listener(Btn0,'java.awt.event.ActionEvent',
	       appendText("0",Display)),
        java_add_listener(BtnEquals,'java.awt.event.ActionEvent',
	       compute("=",Display)),
        java_add_listener(BtnDiv,'java.awt.event.ActionEvent',
	       compute("/",Display)),

%% Show the calculator.
	java_invoke_method(Frame,show(_)).

%%--------------------------------------------------
%% Event Handlers.
%%
:- use_module(library(system)).

changeTitle(Frame) :-
	time(X),
	number_codes(X,Xs),
	java_invoke_method(Frame,setTitle(Xs,_)).

termination(Frame, JObjects) :-
	delete_objects(JObjects),
        java_delete_object(Frame),
	java_stop,
	set_fact(acumulator(0)),
	set_fact(operator("+")),
	set_fact(number_editing(false)).

delete_objects([]).

delete_objects([X|Xs]) :-
	java_delete_object(X),
	delete_objects(Xs).

appendText(Txt,Dsp) :-
	(number_editing(true) ->
	 java_invoke_method(Dsp,getText(Txt1)),
	 append(Txt1,Txt,Text),
	 java_invoke_method(Dsp,setText(Text,_))
	;
	 java_invoke_method(Dsp,setText(Txt,_)),
	 set_fact(number_editing(true))
	)
	.

%%--------------------------------------------------
%% Computation predicates. Perform the computation
%% between the acumulator and the number on the
%% screen, and print the result.
%%
compute(NewOperator, Dsp) :-
	set_fact(number_editing(false)),
	java_invoke_method(Dsp,getText(Txt)),
	acumulator(Operand1),
	operator(Operator),
	number_codes(Operand2,Txt),
	perform_calculation(Operator, Operand1, Operand2, Result),
	number_codes(Result,ResultTxt),
	java_invoke_method(Dsp,setText(ResultTxt,_)),
	set_fact(acumulator(Result)),
	set_fact(operator(NewOperator)).

perform_calculation("=", _Operand1, Operand2, Operand2).

perform_calculation("+", Operand1, Operand2, Result) :-
	Result is Operand1 + Operand2.

perform_calculation("-", Operand1, Operand2, Result) :-
	Result is Operand1 - Operand2.

perform_calculation("*", Operand1, Operand2, Result) :-
	Result is Operand1 * Operand2.

perform_calculation("/", Operand1, Operand2, Result) :-
	(Operand2 =\= 0 ->
	 Result is floor(Operand1 / Operand2)
	;
	    Result = 0        %% Division by zero.
	).
