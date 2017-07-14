sub(X, z, X).
sub(s(X), s(Y), Z) :- sub(X, Y, Z).

?- sub(s(s(s(z))), s(z), X).
X = s(s(z)) ;
false.

?- sub(z, s(z), X).
false.