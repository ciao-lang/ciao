:- module(example_fullasr, [echo2/2],
        [assertions, rtchecks, nativeprops, expander]).

% minimal working example that can be used to test the new functionality
% of the rtchecks package that allows to separate property definitions
% from property implementations, where only the latter should be used
% in the instrumented code.
%
% Why is it useful:
% - the assertion has all four possible fields filled (DP, CP, AP, GP, CO);
% - there are both properties for which there exists special rtc
%   implementation and those for which there is none;
% - each assertion part has at least 2 properties in it.

:- use_module(engine(io_basic)).

:- pred echo2(X,Y) :: (integer(X), gnd(Y))
                   :  (gnd(X), gnd(Y))
                   => (gnd(X), gnd(Y))
                   +  is_det # "Comment" .

echo2(X,Y) :-
        display(X),
        display(' is '),
        display(Y), nl.
