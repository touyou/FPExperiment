/* append */
append([], Y, Y).
append([A|X], Y, [A|Z]) :- append(X, Y, Z).

/* reverse */
reverse([], []).
reverse([A|X], Y) :- reverse(X, Z), append(Z, [A], Y).

/* concat */
concat([], []).
concat([A|X], Y) :- concat(X, Z), append(A, Z, Y).