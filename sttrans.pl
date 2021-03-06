% Author: Bruce Spencer
% Date: 4/12/2012
%stTrans performs structural Transformation of OWL axioms
stTrans(A, A) :-
   class(A).
stTrans('#Bottom', '#Top').
stTrans('#Top', '#Bottom').
stTrans(complementOf(C), ST) :-
   with_output_to(atom(ST), complementOf(C)).
stTrans(