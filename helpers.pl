is_between(Begin, End, Begin) :-
	Begin =< End.
	
is_between(Begin, End, Result) :-
	Begin =< End,
	BeginIncr is Begin + 1,
	is_between(BeginIncr, End, Result).
	
conc([], B, B).

conc([H|T],B,[H | T2]) :-
	conc(T, B, T2). 

non_slash(A) :-
	var(A).
	
non_slash(A) :-
	A \= _/_.

safe_diff(A/_, B/_) :-
	dif(A, B).
	
safe_diff(A/_, B) :-
	dif(A, B).

safe_diff(A, B/_) :-
	dif(A, B).

safe_diff(A, B) :-
	dif(A, B).

remove_element(E, [], []).

remove_element(E, [E | Rest], Rest).

remove_element(E, [X | Rest], [X | RestResult]) :-
    remove_element(E, Rest, RestResult).
