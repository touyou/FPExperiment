judge(P, _) :- eq(P, e), !, false.
judge(P, [P,P,P,_,_,_,_,_,_]).
judge(P, [_,_,_,P,P,P,_,_,_]).
judge(P, [_,_,_,_,_,_,P,P,P]).
judge(P, [P,_,_,P,_,_,P,_,_]).
judge(P, [_,P,_,_,P,_,_,P,_]).
judge(P, [_,_,P,_,_,P,_,_,P]).
judge(P, [P,_,_,_,P,_,_,_,P]).
judge(P, [_,_,P,_,P,_,P,_,_]).

notjudge(P, B) :- judge(P, B), !, false.
notjudge(_, [_,_,_,_,_,_,_,_,_]).

next(P, [e,A,B,C,D,E,F,G,H], [P,A,B,C,D,E,F,G,H]).
next(P, [A,e,B,C,D,E,F,G,H], [A,P,B,C,D,E,F,G,H]).
next(P, [A,B,e,C,D,E,F,G,H], [A,B,P,C,D,E,F,G,H]).
next(P, [A,B,C,e,D,E,F,G,H], [A,B,C,P,D,E,F,G,H]).
next(P, [A,B,C,D,e,E,F,G,H], [A,B,C,D,P,E,F,G,H]).
next(P, [A,B,C,D,E,e,F,G,H], [A,B,C,D,E,P,F,G,H]).
next(P, [A,B,C,D,E,F,e,G,H], [A,B,C,D,E,F,P,G,H]).
next(P, [A,B,C,D,E,F,G,e,H], [A,B,C,D,E,F,G,P,H]).
next(P, [A,B,C,D,E,F,G,H,e], [A,B,C,D,E,F,G,H,P]).

op(x, o).
op(o, x).

eq(P, P).

end(B) :- next(o, B, _), !, false.
end(B) :- next(x, B, _), !, false.
end(_).

win(P, B) :- judge(P, B).
win(P, B) :- op(P, Q), \+judge(Q, B), next(P, B, C), lose(Q, C).

lose(P, B) :- op(P, Q), judge(Q, B).
lose(P, B) :- op(P, Q), \+end(B), next(P, B, C), win(Q, C).