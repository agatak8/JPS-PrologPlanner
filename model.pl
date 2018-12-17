InitialState = [on(a, b),
on(b, c),
on(c, p1),
on(d, e),
on(e, f),
on(f, g),
on(g, p2),
clear(p3),
clear(p4),
clear(p5)]

goals_achieved(on(Y/on(Y, X/on(X, c))), InitialState).
goals_achieved(clear(Y), InitialState).
goals_achieved(clear(A/on(A, e)), InitialState).
goals_achieved(clear(a), InitialState).
goals_achieved(on(a, b), InitialState).
goals_achieved(on(A, b), InitialState).
