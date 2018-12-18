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

testReq :-
	consult('project.pl'),
	initialState(State),
	Action = move(X3/on(X3, X2/on(X2,b4)),X2/on(X2,b4), Z3),
	requires(Action, CondGoals, Conds),
	X3 = "X3",
	X2 = "X2",
	Z3 = "Z3",
	write_ln(CondGoals),
	write_ln(Conds),
	Action2 = move(b4, Y1/on(b4, Y1), b2),
	requires(Action2, CG2, C2),
	Y1 = "Y1",
	write_ln(CG2),
	write_ln(C2),
	Action3 = move(X413/on(X413,b4), b4, Z2),
	requires(Action3, CG3, C3),
	X413 = "X413",
	Z2 = "Z2",
	write_ln(CG3),
	write_ln(C3).
