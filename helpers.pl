is_between(Begin, End, Begin) :-
	Begin =< End.
	
is_between(Begin, End, Result) :-
	Begin =< End,
	is_between(Begin+1, End, Result).
	
conc([], B, B).

conc([H|T],B,[H | T2]) :-
    conc(T, B, T2). 

