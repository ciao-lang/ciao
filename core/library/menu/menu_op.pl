% :- op( 800 , yfx, #           ). has pref 500

%:- op(  500 , yfx, '#'  ). % Defined somehwere :S 
%:- op(  500 , yfx, '-'  ). % Defined somehwere :S
%:- op(  550 , xfx, ':'  ). % Defined somehwere :S
%:- op(  978 , xfx, '::' ). % 978 to avoid clash with assertions package
:- op(  970 , xfx, '::' ). % 978 to avoid clash with assertions package
:- op(  971 , xfx, '<-' ). % priority has to be less than 972 because 
                           % this op is defined as unary in make_ops

:- op(  900 , fy , guard ).
