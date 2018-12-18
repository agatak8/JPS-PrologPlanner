initialState(
	[on(a, b),
	on(b, c),
	on(c, p1),
	on(d, e),
	on(e, f),
	on(f, g),
	on(g, p2),
	clear(a),
	clear(d),
	clear(p3),
	clear(p4),
	clear(p5)]
).

testModel :-
	consult('project.pl'),
	initialState(State),
	write("State: "),
	write_ln(State),
	write_ln("==============="),
	GoalLists = [
		[clear(Y/on(Y, X/on(X, c)))],
		[clear(Z)],
		[clear(A/on(A, e))],
		[clear(a)],
		[on(a, b)],
		[on(B, b)],
		[on(C, b), on(C, c)],
		[clear(D/on(D,E/on(E,p1)))]
	],
	write("Goals: "),
	write_ln(GoalLists),
	write_ln("============="),
	testAll(GoalLists, State).

testAll([], _).

testAll([Goals | Rest], State) :-
	testGoals(Goals, State),
	testAll(Rest, State).

testGoals(Goals, State) :-
	goals_achieved(Goals, State),
	write_ln(Goals).
	
testGoals(Goals, State) :-
	not(goals_achieved(Goals, State)),
	write(Goals),
	write_ln(" not achieved").
