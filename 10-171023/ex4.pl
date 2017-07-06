/* append */
append([], Y, Y).
append([A|X], Y, [A|Z]) :- append(X, Y, Z).

/* reverse */
reverse([], []).
reverse([A|X], Y) :- reverse(X, Z), append(Z, [A], Y).

/* concat */
concat([], []).
concat([A|X], Y) :- concat(X, Z), append(A, Z, Y).


/* hamilton(V, E) */
search([[S|T]|E], S, T, E).
search([[W|Z]|E], S, T, X) :- search(E, S, T, Y), append([[W|Z]], Y, X).

searchV([V|X], V, X).
searchV([W|X], V, Y) :- searchV(X, V, Z), append([W], Z, Y).

hamiltonRec([], [], S).
hamiltonRec(V, E, S) :- search(E, S, T, R), searchV(V, T, N), hamiltonRec(N, R, T).
hamilton(V, [[S|T]|E]) :- searchV(V, S, N), hamiltonRec(N, [[S|T]|E], S).