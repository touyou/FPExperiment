add(z, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

mult(z, Y, z).
mult(s(X), Y, A) :- add(Z, Y, A), mult(X, Y, Z).
