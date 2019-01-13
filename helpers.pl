is_between(Begin, End, Begin) :-
	Begin =< End.
	
is_between(Begin, End, Result) :-
	Begin =< End,
	BeginIncr is Begin + 1,
	is_between(BeginIncr, End, Result).
	
conc([], B, B).

conc([H|T],B,[H | T2]) :-
    conc(T, B, T2). 
