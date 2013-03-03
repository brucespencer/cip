
insertConjunctsIntoSaturate(A, H, L, [sub(H, A) | L]):-
	atom(A).
insertConjunctsIntoSaturate(intersectionOf(E1, E2), H, L1, L0 ):-
	insertConjunctsIntoSaturate(E1, H, L1, L2),
	insertConjunctsIntoSaturate(E2, H, L2, L0).

testcase(Out):-
	 H = intersectionOf(intersectionOf(a, b), intersectionOf(c, intersectionOf(d, e))),
	 insertConjunctsIntoSaturate(H, H, [], Out).