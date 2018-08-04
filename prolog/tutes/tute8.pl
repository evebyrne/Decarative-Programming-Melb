sumlist([], 0).
sumlist([N|Ns], Sum) :-
	sumlist(Ns, Sum0),
	Sum is N + Sum0.


sumL(N, S) :- sumList2(N,0,S).
sumList2([], S,S).
sumList2([N|Ns], A, S) :- A1 is A + N, sumList2(Ns, A1, S).


/*QUESTION 2

Given a binary tree defined to satisfy the predicate tree*/
tree(empty).
tree(node(Left,_,Right)) :-
	tree(Left),
	tree(Right).
/*
write a predicate tree_list(Tree, List) that holds when List is a
list of all the elements of Tree, in left-to-right order.  This code
need only work in the mode where the tree is input.
*/

append([], C, C).
append( [A|B], C, [A|BC]) :- append(B, C, BC).


tree_list1(empty, []).
tree_list1(node(Left,Elt,Right), List) :- tree_list1(Left, List1), tree_list1(Right, List2), append(List1, [Elt|List2], List).

/*A TREE!!!
tree(node(node(empty,3,empty),4,empty))
TO CALL!!!
tree_list(node(node(empty,3,empty),4,empty),X)

THE VARIABLES(Left, Elt etc) have to be capitalised or else it wont work!!!!
*/
tree_list(empty, []).
tree_list(node(Left,Elt,Right), List) :-
	tree_list(Left, List1),
	tree_list(Right, List2),
	append(List1, [Elt|List2], List).


/*usage: tree_list_tail(node(node(empty,3,empty),4,empty),A,[]).
*/
tree_list_tail(empty, List, List).
tree_list_tail(node(Left, Elt, Right), List, List0) :- 
      tree_list_tail(Left, List, List1),
      List1 = [Elt|List2],      
      tree_list_tail(Right, List2, List0).

len([], 0).
len([X|XS], Len) :- Len1 = Len +1, len(XS, Len1).


list_tree([], empty).
list_tree(XS, node(Left, N, Right)) :-
     length(XS, Len), 
     Len2 is Len // 2, 
     length(Front, Len2),
     append(Front, [N|Back], XS),
     list_tree(Front, Left), 
     list_tree(Back, Right).


















