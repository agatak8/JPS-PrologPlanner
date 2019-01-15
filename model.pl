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

imports :-
	consult('project.pl'),
	consult('helpers.pl').

testModel :-
	imports,
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
	imports,
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

testPlanLimit :-
	imports,
	initialState(State),
	not(plan_wrapper(State, [on(a,d)], 0, _, _)),
	plan_wrapper(State, [on(a,b)], 0, _, State),
	not(plan_wrapper(State, [on(g,p5)], 3, Plan, FinState)).

initialConditions([
A/on(A, B/on(B, C)),
A1/on(A1, B1),
A2/on(B2/on(C2, B2), A2),
on(A3, B3),
clear(C4),
clear(D5/on(D5, E5)),
clear(D6/on(D6, E6/on(F6, E6)))
]).

condTest :-
	imports,
	initialState(State),
	% initialConditions(Conds),
	inst_action(move(a, b, On), [clear(On), safe_diff(On, a)], State, InstAction),
	write("InstAction: "),
	write_ln(InstAction),
	inst_action(move(X/on(X,b), b, On2), [clear(On2), safe_diff(X, On2)], State, InstAction2),
	write("InstAction: "),
	write_ln(InstAction2),
	inst_action(move(a, From, d), [clear(d), on(a, From)], State, InstAction3),
	write("InstAction: "),
	write_ln(InstAction3),
	inst_action(move(A/on(A, From2), From2, d), [clear(d), on(A, From2)], State, InstAction4),
	write("InstAction: "),
	write_ln(InstAction4),
	What = X3/on(X3, X2/on(X2,f)),
	From3 = X2/on(X2,f),
	inst_action(move(What, From3, Z3), [clear(Z3), safe_diff(Z3, What)], State, InstAction5),
	write("InstAction: "),
	write_ln(InstAction5).

condTestAll :-
    imports,
	initialState(State),
	What = X3/on(X3, X2/on(X2,f)),
	From3 = X2/on(X2,f),
	findall(InstAction, inst_action(move(What, From3, Z3), [clear(Z3), safe_diff(Z3, What)], State, InstAction), InstActions),
	write("InstActions: "),
	write_ln(InstActions).

condAchieveTest :-
    imports,
    initialState(State),
    conds_achieved([safe_diff(X, X/on(X,c))], State).

testDif :-
    dif(A, B),
    A is 1,
    B is 1.

testDif2 :-
    dif(A, B),
    A is 1,
    B is 2.
    
testDif3 :-
    not(dif(A, B)),
    A is 1,
    B is 2.

testEq :-
    \=(A, B),
    A is 1,
    B is 2.

planTest :-
	imports,
	initialState(State),
	plan_wrapper(State, [on(a, f)], 3, Plan, FinalState),
	write("Plan: "),
	write_ln(Plan).
