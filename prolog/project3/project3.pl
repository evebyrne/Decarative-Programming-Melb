replace(E1, [E1|L], E2, [E2|L]).
replace(E1, [X|L1], E2, [X|L2]) :- replace(E1, L1, E2, L2).

zip([], [], []).
zip([A|RestA], [B|RestB], [C|RestAB]) :- (C = A-B), zip(RestA, RestB, RestAB).

sublist([], _).
sublist([A|RA], [A|List]) :- sublist(RA, List).
sublist([A|RA], [_|List]) :- sublist([A|RA], List).
