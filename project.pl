plan (State, Goals, [  ], State)   :-
	goals_achieved (Goals, State) .

plan (InitState, Goals, Plan, FinalState)   :-
	choose_goal (Goal, Goals, RestGoals, InitState),
	achieves (Goal, Action),
	requires (Action, CondGoals, Conditions), %% Conditions - warunki do ukonkretnienia zmiennych w momencie wykonywania akcji np zeby move(Co, Skad, Dokad) musi byc clear(Dokad) -> trzeba znalezc takie Dokad ktore to spelnia
	%% CondGoals - warunki ktore staja sie celami (nie do ukonkretnienia zmiennych) np zeby miec move(Co, Skad, Dokad) gdzie Co jest konkretne, musimy miec clear(Co)
	plan (InitState, CondGoals, PrePlan, State1),
	inst_action(Action, Conditions, State1, InstAction),
	perform_action (State1, InstAction, State2),
	plan (State2, RestGoals, PostPlan, FinalState),
	conc (PrePlan, [InstAction | PostPlan ], Plan).
	
goals_achieved([], _).

goals_achieved([HeadGoal | Rest], UnitedState) :-
	%% member(HeadGoal, UnitedState), %% ! struktura /
	goal_achieved(HeadGoal, UnitedState),
	goals_achieved(Rest, UnitedState).
	
goal_achieved(clear(A/Goal), State) :-
	goal_achieved(Goal, State),
	member(clear(A), State).
	
goal_achieved(clear(A), State) :-
	member(clear(A), State).

goal_achieved(on(A/Goal, B/Goal2), UnitedState) :-
	goal_achieved(Goal, UnitedState),
	goal_achieved(Goal2, UnitedState),
	member(on(A, B), UnitedState).
	
goal_achieved(on(A, B/Goal2), UnitedState) :-
	goal_achieved(Goal2, UnitedState),
	member(on(A, B), UnitedState).

goal_achieved(on(A/Goal, B), UnitedState) :-
	goal_achieved(Goal, UnitedState),
	member(on(A, B), UnitedState).

choose_goal(Goal, [Goal | Goals], Goals, UnitedState) :-
	not(goals_achieved([Goal], UnitedState)).
	
choose_goal(Goal, [X | Goals], [X | TailRestGoals], UnitedState) :-
	choose_goal(Goal, Goals, TailRestGoals, UnitedState).
	
achieves(Goal, Action) :-

achieves(on(A, B), move(A, Y/(on(A, Y)), B)).
achieves(clear(B), move(A/on(A,B), B, C)).

%% requires(Action, CondGoals, Conditions) :-CELE

requires(move(What, From, On), [clear(What), clear(On)], [on(What, From)]) :-
	nonvar(What),
	nonvar(On).
	%% var(From).
	

requires(move(What, From, On), [clear(What)], [clear(On), On /= What]) :-
	var(What),
	var(On).
	%% nonvar(From).

conc([], B, B).
conc([H|T],B,[H | T2]) :- conc(T, B, T2). 
