gen(X, Max) :-
	gen(0, Max, X).

gen(X, Max, X) :-
	X =< Max.
gen(X, Max, Next) :-
	X =< Max,
	X2 is X+1,
	gen(X2, Max, Next).

