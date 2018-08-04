/*
QUESTION 1

Write a Prolog predicate same_elements(L1, L2) that holds when all
elements of list L1 are in L2 and vice versa, though they may be in
different orders and have different numbers of occurrences of the
elements.  This need only work in modes where both arguments are
ground.
*/

same_elements([], []). 
same_elements([], _).
same_elements(XS, YS) :- allIn(XS, YS), allIn(YS, XS).

allIn([], _).
allIn([X|XS], YS) :- member(X, YS), allIn(XS, YS).


/*
QUESTION 2

Rewrite your same_elements predicate to work in n log(n) time, where n
is the length of the longer list.

*/

same_elements_better(L1,L2) :- 
          sort(L1, Sorted), sort(L2, Sorted).

/*

Q3
W*X + Y = Z
  0 <= Y < |W|
  W \= 0
  |W| <= |Z|
*/


times(W,X,Y,Z) :- 
      (integer(W),  
      0 =< Y,
      Y < abs(W),
      integer(X) -> 
      Z is W*X + Y;
      integer(Z),
      Temp = Z - Y, 
      print(Temp),
      Z = dive(Temp,W),
      print(Z)).
       



containers(Moves) :- 
         /*container1, container2, , , goal ? ?
            new fxn...*/
         containers(3,5,0,0, _, 4, [0-0], Steps).

/* \+ = It's the 'not provable' operator. It succeeds if its argument is not provable (and fails if its argument is provable).
containers(C1=3,C2=5,V1=0,V2=0, T1=_, T2=4, Hist=[0-0], [Move|Steps]=Steps).*/
containers(_,_,V1, V2, V1, V2, _, []).
containers(C1, C2, V1, V2, T1, T2, Hist, [Move|Steps]) :-
          move(C1, C2, V1, V2, N1, N2, Move),
          /*N1, N2 = new vols of 1 and 2*/
          State = N1 - N2,
          \+ member(State, Hist), 
          containers(C1,C2,N1,N2,T1,T2, [State|Hist], Steps).

/*new V1 is 0 if empty(C1)*/
move(C1, _, _, V2, 0, V2, empty(C1)).
move(_,C2, V1, _, V1, 0, empty(C2)).
move(C1, _, _, V2, C1, V2, fill(C1)).
move(_, C2, V1, _, V1, C2, fill(C2)).
move(C1, C2, V1, V2, N1, N2, pour(C1, C2)) :-
          pour(C2, V1, V2, N1, N2).
move(C1, C2, V1, V2, N1, N2, pour(C2, C1)) :-
          pour(C1, V2, V1, N2, N1).


pour(C2, V1, V2, N1, N2) :- 
       (V1 + V2 > C2 -> 
         /*cant pour all in*/
           N2 is C2,
           N1 is V1 - (C2-V2)
         /*pour all of V1 into V2*/
        ;  N1 = 0,
           N2 = V2 + V1
        ).
           
            








































