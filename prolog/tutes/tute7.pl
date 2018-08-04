proper_list([]).
proper_list([_|Tail]) :- proper_list(Tail).

/*Q1*/
list_of(_, []).
list_of(Elt, [Elt|Tail]) :-list_of(Elt, Tail).

/*Q2*/
allSame(List) :- list_of(_, List).

/*q3*/
adjacent(E1, E2, List) :- append1(_, [E1,E2|_], List).

append1([], C, C).
append1( [A|B], C, [A|BC]) :- append1(B, C, BC).

/*q4*/

member22(Elt, [Elt|_]).
member22(Elt, [_|Rest]) :- member22(Elt, Rest).

adjacent2(E1, E2, [E1,E2|_]).
adjacent2(E1,E2, [_|Rest]) :- adjacent2(E1, E2, Rest).

/*q5*/

before(E1, E2, [E1|List]) :- member(E2, List).
before(E1, E2, [_|List]) :- before(E1, E2, List).

/*q6*/
intset(N, tree(_,N,_)).
intset(N, tree(L, X, _)) :- X>N, intset(N, L).
intset(N, tree(_, X, R)) :- X<N, intset(N, R).


insert(N, empty, tree(empty, N, empty)).
insert(N, tree(L,N,R), tree(L,N,R)).
insert(N, tree(L0, X, R), tree(L, X,R)) :- X > N, insert(N, L0, L).
insert(N, tree(L, X, R0), tree(L, X,R)) :- X < N, insert(N, R0, R).





