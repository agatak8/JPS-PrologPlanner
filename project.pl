plan_wrapper(InitState, Goals, MaxLimit, Plan, FinalState) :-
	consult('helpers.pl'),
	is_between(0, MaxLimit, Limit),
	plan(InitState, Goals, [], Limit, Plan, FinalState, 0).

plan(State, Goals, AchievedGoals, _, [], State, RecursionLevel) :-
	goals_achieved(Goals, State).
	
plan(InitState, Goals, AchievedGoals, Limit, Plan, FinalState, RecursionLevel) :-
	Limit > 0,
	is_between(0, Limit-1, LimitPre),
	choose_goal(Goal, Goals, RestGoals, InitState),
	achieves(Goal, Action),
	requires(Action, CondGoals, Conditions),
	Rec2 is RecursionLevel + 1,
	plan(InitState, CondGoals, AchievedGoals, LimitPre, PrePlan, State1, Rec2),
	inst_action(Action, Conditions, State1, InstAction),
	perform_action(State1, InstAction, State2),
	LimitPost is Limit - LimitPre,
	plan(State2, RestGoals, [Goal | AchievedGoals], LimitPost, PostPlan, FinalState, Rec2),
	conc(PrePlan, [InstAction | PostPlan ], Plan).

goals_achieved([], _).

goals_achieved([HeadGoal | Rest], UnitedState) :-
	goal_achieved(HeadGoal, UnitedState),
	goals_achieved(Rest, UnitedState).
	
goal_achieved(on(A, B), UnitedState) :-
	non_slash(A),
	non_slash(B),
	member(on(A, B), UnitedState).

goal_achieved(clear(A), State) :-
	non_slash(A),
	member(clear(A), State).
	
goal_achieved(clear(A/Goal), State) :-
	nonvar(Goal),
	goal_achieved(Goal, State),
	member(clear(A), State).

goal_achieved(on(A, B/Goal2), UnitedState) :-
	non_slash(A),
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
	not(goal_achieved(Goal, UnitedState)).
	
choose_goal(Goal, [X | Goals], [X | TailRestGoals], UnitedState) :-
	choose_goal(Goal, Goals, TailRestGoals, UnitedState).
	
achieves(on(A, B), move(A, Y/(on(A, Y)), B)).

achieves(clear(B), move(A/on(A,B), B, C)).

requires(move(What, From, On), [clear(What), clear(On)], [on(What, From)]) :-
	non_slash(From),
	nonvar(What),
	nonvar(On).
	
requires(move(What, From/on(What, From), On), [clear(What), clear(On)], [on(What, From)]) :-
	nonvar(What),
	nonvar(On).

requires(move(What, From, On), [clear(What)], [clear(On), safe_diff(On, What)]) :-
	var(On).
	
check_action(_, []).

check_action(InstAction, [Goal|Rest]) :-
	not(action_destroys_goal(InstAction, Goal)),
	check_action(InstAction, Rest).

action_destroys_goal(move(What, From, _), on(What, From)).

action_destroys_goal(move(_, _, On), clear(On)).

inst_action(move(What, From, On), Conds, UnitedState, move(InstWhat, InstFrom, InstOn)) :-
	inst_one(What, UnitedState, InstWhat),
	inst_one(From, UnitedState, InstFrom),
	inst_one(On, UnitedState, InstOn),
	conds_achieved(Conds, UnitedState).

conds_achieved([], _).

conds_achieved([safe_diff(A, B) | Rest], UnitedState) :-
    non_slash(A),
    non_slash(B),
    conds_achieved(Rest, UnitedState), !,
	safe_diff(A, B).
	
conds_achieved([safe_diff(A, B/W) | Rest], UnitedState) :-
    non_slash(A),
	goal_achieved(W, UnitedState),
	conds_achieved(Rest, UnitedState), !,
	safe_diff(A, B).

conds_achieved([safe_diff(A/W, B) | Rest], UnitedState) :-
    non_slash(B),
	goal_achieved(W, UnitedState),
	conds_achieved(Rest, UnitedState), !,
	safe_diff(A, B).

conds_achieved([safe_diff(A/W, B/W2) | Rest], UnitedState) :-
	goal_achieved(W, UnitedState),
	goal_achieved(W2, UnitedState),
	conds_achieved(Rest, UnitedState), !,
	safe_diff(A, B).

conds_achieved([HeadGoal | Rest], UnitedState) :-
	HeadGoal \= safe_diff(_, _),
	goal_achieved(HeadGoal, UnitedState),
	conds_achieved(Rest, UnitedState).

inst_test(A, US, UI) :-
    nonvar(A),
    inst_one(A, US, UI).
	
inst_test(A, US, UI) :-
    var(A),
    prompt_user_input(A, US, UI).

prompt_user_input(A, UnitedState, UserInput) :-
    write_ln(UnitedState),
    write("Wybierz wartosc dla "),
    write(A),
    write_ln(":"),
    read(UserInput),
    process_user_input(A, UnitedState, UserInput), !.

process_user_input(A, US, UserInput) :-
    UserInput \= 'cofnij'.

process_user_input(A, US, UserInput) :-
    UserInput \= 'cofnij',
    prompt_user_input(A, US, UserInput).
      
	
inst_one(A/B, UnitedState, A) :-
    % zeby nie generowac nowych zmiennych postaci Zmienna/Zmienna2 po nawrocie
    var(A),
    var(B),
	fail.
	
inst_one(A/B, UnitedState, A) :-
    nonvar(B),
	goal_achieved(B, UnitedState).
	
inst_one(A, UnitedState, A) :-
	% uncomment for testing 
	% nonvar(A),
	non_slash(A).

perform_action(State1, move(What, From, On), [on(What, On), clear(From) | PartialRes]) :-
    member(clear(On), State1),
    member(on(What, From), State1),
    delete(State1, clear(On), PartialPartialRes),
    delete(PartialPartialRes, on(What, From), PartialRes).

