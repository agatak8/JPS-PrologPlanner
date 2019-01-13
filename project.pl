plan_wrapper(InitState, Goals, MaxLimit, Plan, FinalState) :-
    consult('helpers.pl'),
    is_between(0, MaxLimit, Limit),
    write("Trying Limit: "),
    write_ln(Limit),
    plan(InitState, Goals, [], Limit, Plan, FinalState, 0).

plan(State, Goals, _, _, [], State, RecursionLevel) :-
    write("Recursion level is: "),
    write_ln(RecursionLevel),
	goals_achieved(Goals, State),
	write_ln("Achieved goal").
	% write("Goals: "),
	% write(Goals),
	% write_ln(" achieved!").

plan(InitState, Goals, AchievedGoals, Limit, Plan, FinalState, RecursionLevel) :-
    Limit > 0,
    % wygeneruje LimitPre od 0 do Limit
    is_between(0, Limit-1, LimitPre),
    write("Trying LimitPre: "),
    write_ln(LimitPre),
	choose_goal(Goal, Goals, RestGoals, InitState), % pkt wyboru - kolejnosc goali moze miec znaczenie
	achieves(Goal, Action),
	% write("Chosen goal needs action: "),
	% write_ln(Action),
	requires(Action, CondGoals, Conditions), %% Conditions - warunki do ukonkretnienia zmiennych w momencie wykonywania akcji np zeby move(Co, Skad, Dokad) musi byc clear(Dokad) -> trzeba znalezc takie Dokad ktore to spelnia
	%% CondGoals - warunki ktore staja sie celami (nie do ukonkretnienia zmiennych) np zeby miec move(Co, Skad, Dokad) gdzie Co jest konkretne, musimy miec clear(Co)
	% write("Action needs goals: "),
	% write(CondGoals),
	% write(" and conditions: "),
	% write_ln(Conditions),
	Rec2 is RecursionLevel + 1,
	plan(InitState, CondGoals, LimitPre, PrePlan, State1, Rec2),
	inst_action(Action, Conditions, State1, InstAction), % pkt wyboru ponownie (miejsca do odstawienia)
	check_action(InstAction, AchievedGoals),
	perform_action(State1, InstAction, State2),
	% reszta idzie do LimitPost
	LimitPost is Limit - LimitPre - 1,
	write("LimitPost is: "),
	write_ln(LimitPost),
	plan(State2, RestGoals, [Goal | AchievedGoals], LimitPost, PostPlan, FinalState, Rec2),
	conc(PrePlan, [InstAction | PostPlan ], Plan).
	
plan(_, _, _, _, _, _) :-
    write_ln("Failed to find plan within limit"),
    fail.
% DEBUG - TODO remove
inst_action(_, _, _, _) :-
    fail.
perform_action(_, _, _) :-
    fail.

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

% check_action(InstAction, AchievedGoals)
check_action(_, []).

check_action(InstAction, [Goal|Rest]) :-
    check_if_action_destroys_goal(InstAction, Goal),
    check_action(InstAction, Rest).

% TODO - upewnic sie czy potrzeba zaimplementowac casey struktur _/_?
check_if_action_destroys_goal(move(What, From, _), on(What, From)).
check_if_action_destroys_goal(move(_, _, On), clear(On)).


