/* reverse */

reverse([], []).
reverse([A,X], [Y,A]) := reverse(X, Y).