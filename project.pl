plan_wrapper(InitState, Goals, MaxLimit, Plan, FinalState) :-
    is_between(0, MaxLimit, Limit),
    plan(InitState, Goals, Limit, Plan, FinalState).

plan(State, Goals, _, [  ], State)   :-
	goals_achieved(Goals, State) .

plan(InitState, Goals, Limit, Plan, FinalState)   :-
    consult('helpers.pl'),
    Limit > 0,
    % wygeneruje LimitPre od 0 do Limit
    is_between(0, Limit, LimitPre),
	choose_goal(Goal, Goals, RestGoals, InitState), % pkt wyboru - kolejnosc goali moze miec znaczenie
	achieves(Goal, Action),
	requires(Action, CondGoals, Conditions), %% Conditions - warunki do ukonkretnienia zmiennych w momencie wykonywania akcji np zeby move(Co, Skad, Dokad) musi byc clear(Dokad) -> trzeba znalezc takie Dokad ktore to spelnia
	%% CondGoals - warunki ktore staja sie celami (nie do ukonkretnienia zmiennych) np zeby miec move(Co, Skad, Dokad) gdzie Co jest konkretne, musimy miec clear(Co)
	plan(InitState, CondGoals, LimitPre, PrePlan, State1),
	inst_action(Action, Conditions, State1, InstAction), % pkt wyboru ponownie (miejsca do odstawienia)
	perform_action(State1, InstAction, State2),
	% reszta idzie do LimitPost
	LimitPost is Limit - LimitPre - 1,
	plan(State2, RestGoals, LimitPost, PostPlan, FinalState),
	conc(PrePlan, [InstAction | PostPlan ], Plan).

goals_achieved([], _).

goals_achieved([HeadGoal | Rest], UnitedState) :-
	%% member(HeadGoal, UnitedState), %% ! struktura /
	goal_achieved(HeadGoal, UnitedState),
	goals_achieved(Rest, UnitedState).
	
goal_achieved(on(A, B), UnitedState) :-
	A \= _/_,
	B \= _/_,
	member(on(A, B), UnitedState).

goal_achieved(clear(A), State) :-
	A \= _/_,
	member(clear(A), State).
	
goal_achieved(clear(A/Goal), State) :-
	% goal_achieved(clear(X), [...])
	
	nonvar(Goal),
	goal_achieved(Goal, State),
	member(clear(A), State).

goal_achieved(on(A, B/Goal2), UnitedState) :-
	A \= _/_,
	nonvar(Goal2),
	goal_achieved(Goal2, UnitedState),
	member(on(A, B), UnitedState).

% generalization only
goal_achieved(on(A/Goal, B), UnitedState) :- 
	nonvar(Goal),
	goal_achieved(Goal, UnitedState),
	member(on(A, B), UnitedState).
	
goal_achieved(on(A/Goal, B/Goal2), UnitedState) :-
	nonvar(Goal),
	nonvar(Goal2),
	goal_achieved(Goal, UnitedState),
	goal_achieved(Goal2, UnitedState),
	member(on(A, B), UnitedState).
	

choose_goal(Goal, [Goal | Goals], Goals, UnitedState) :-
	% member(Goal, [Goal | Goals]),
	not(goal_achieved(Goal, UnitedState)).
	
choose_goal(Goal, [X | Goals], [X | TailRestGoals], UnitedState) :-
	choose_goal(Goal, Goals, TailRestGoals, UnitedState).
	
achieves(on(A, B), move(A, Y/(on(A, Y)), B)).

achieves(clear(B), move(A/on(A,B), B, C)).

%% requires(Action, CondGoals, Conditions) :-CELE

requires(move(What, From, On), [clear(What), clear(On)], [on(What, From)]) :-
    From \= _/_,
	nonvar(What),
	nonvar(On).
	
requires(move(What, From/on(What, From), On), [clear(What), clear(On)], [on(What, From)]) :-
	nonvar(What),
	nonvar(On).

requires(move(What, From, On), [clear(What)], [clear(On), On \= What]) :-
	var(On).
