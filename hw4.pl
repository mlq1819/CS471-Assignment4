/* Homework Assignment 4 - Prolog 2
   Programming Languages
   CS471, Spring 2020
   Binghamton University */

/* Instructions */

/* This section deals with general submission instructions.
First, grab this assignment from the site. BEFORE MOVING ON RENAME this file
to hw4.pl.
You will be able to code in and run the file in the Prolog interpreter directly.
I recommend reading this assignment directly from the source file.

We will be using swipl for our Prolog environment: To load/reload this file,
cd to its directory and run swipl. Then, in the prompt, type [hw4].

cd PATH_TO_FILE
swipl
[hw4].

From then on you may execute queries (goals) in the prompt. As usual, you
should provide your answers in the designated spot. Once you have added some
code to the file, rerun [hw4]. in the swipl prompt to reload.

In addition, there are unit tests for each problem. These are there to help you
better understand what the question asks for, as well as check your code. They
are included in our "database" as queries and are initially commented out
-- % is a Prolog line comment.

%:- member_times(4,[3,3,2,3],0). % SUCCEED
%:- member_times(4,[1,2,3],3).   % FAIL

In this assignment, we also have some queries that should fail silently but give
a warning when they succeed, such as

% :- result([+(3,7), mod(104,7)],[10,13]) -> fail ; true.

Be aware of the difference between this type of fail test case and the original
type of fail test case.

After you have finished a problem and are ready to test, remove the initial %
for each test for the associated problem and reload the assignment file
([hw4].). Each SUCCEED line should silently load and succeed, and each FAIL
line should throw a WARNING. If a SUCCEED line throws a WARNING, or a FAIL line
fails to, then you solution is not correct. If you pass the tests there is a
good chance that your code is correct, but not guaranteed; the tests are meant
as guided feedback and are not a check for 100% correctness. */

/* Submission */

/* Please store hw4.pl within a tarball named username_hw4.tar.gz  */

/* Homework 4 */

/* Due: Tues , 2/18 at 11:59 PM */

/* Purpose: To get comfortable with backtracking, recursion,
   become familar with reflective mechanism of Prolog,
   and Prolog as a symbolic programming language.
*/

/* Problem 0A (NOT GRADED):

(From Learn Prolog NOW!) Binary trees are trees where all internal nodes have 
exactly two children. The smallest binary trees consist of only one leaf node. 
We will represent leaf nodes as leaf(Label). For instance, leaf(3) and leaf(7) 
are leaf nodes, and therefore small binary trees. Given two binary trees B1 and 
B2 we can combine them into one binary tree using the predicate tree: 
tree(B1,B2). So, from the leaves leaf(1) and leaf(2) we can build the binary 
tree tree(leaf(1), leaf(2)). And from the binary trees tree(leaf(1), leaf(2)) 
and leaf(4) we can build the binary tree tree(tree(leaf(1), leaf(2)), leaf(4)).

Now define a predicate isBinaryTree(+BT) which succeeds if BT is a binary tree.
The "+" indicates that it is assumed BT is instantiate in the query.
For example:
If BT = tree( leaf(1), tree( leaf(2),leaf(4)) ), then isBinaryTree(BT) succeeds.

*/
/* Problem 0A Answer: */

isBinaryTree(leaf(_V)).
isBinaryTree(tree(L,R)):-
	isBinaryTree(L), isBinaryTree(R).

/* Problem 0A Test: */
:- isBinaryTree(leaf(1)).                                           %SUCCEED
:- isBinaryTree(tree(leaf(a),leaf(b))).                             %SUCCEED
:- BT = tree( leaf(b), tree( leaf(x),leaf(y)) ), isBinaryTree(BT).  %SUCCEED
:- BT = tree(tree(leaf(1), leaf(2)), tree(leaf(10), tree(leaf(4), leaf(11)))), isBinaryTree(BT).  %SUCCEED

:- isBinaryTree( tree(leaf(1)) ).                                   % FAIL
:- isBinaryTree( tree() ).                                          % FAIL



/* Problem 0B (NOT GRADED):
   Each line is an individual Prolog query; it's a good idea type them in your
   prompt (not the file itself) to get a feel for the way Prolog works. You
   should think about whether or not each query will succeed, and if so how the
   variables will be initialized (unified). It will help in doing some of the
   problems.
   (This will be useful in solving problem 3).

?- number(A), A = 5.6. %false? false!
?- A = 5.6, number(A). %true? A=5.6!
?- integer(4). %true? true!
?- help(functor). %... a bunch of stuff
?- functor(foo(a,b,c),F,N). %true? F = foo, N = 3.
?- functor(T,foo,3). %T = foo? T = foo(_,_,_).
?- help(arg). %lotta stuff
?- arg(3, foo(a,b,c),A). %A=3? A=c.
?- help('=..'). %lotta stuff
?- T =.. [foo,x, y, z]. %true? T = foo(x,y,z).
?- E =.. ['+',2,3], R is E. %E=2+3,R=5? E=2+3,R=5!
?- foo(who, what) =.. T. %T=[foo, who, what]? T=[foo, who, what]!
?- foo(who, what) =.. [A, B,C]. %A=foo,B=who,C=what? A=foo,B=who,C=what!
?- clause(ack(M,0,B),C). %true...? false...
?- clause(H,(B is 2*0)). %false...? Error: insufficient instantiation...
*/

/* Problem 0C:
   Write a predicate computeS/4. computeS(Op, Arg1, Arg2, Result) succeeds if
   Result is the value after computing Arg1 Op Arg2. Use the insight you gained
   in Problem 0B. Op must be a builtin Prolog operator.
*/

/* Problem 0C Answer: */

computeS(Op, Arg1, Arg2, Result):-
	R =.. [Op,Arg1,Arg2], Result is R.

/* Problem 0C Test: */
:- computeS(-, 19, 7, 12).
:- computeS(div, 19, 7, 2).
:- computeS(div, 19, 7, R), R = 2.

:- computeS(/, 19, 7, 2) -> fail ; true.
:- catch((computeS(sin, 90, 1, _), fail), error(_Err, _Context), true).

/* Problem 0D (NOT GRADED): */

my_append([],Ys,Ys).
my_append([X|Xs],Ys,[X|Zs]) :- my_append(Xs,Ys,Zs).

my_prefix(_,[]).
my_prefix([X|Xs], [X|Ys]) :- my_prefix(Xs,Ys).

/* Using the preceding predicates, draw the execution trees for the following
queries (goals). You should also enter the queries in swipl to test.

my_append([a,b],[c],Z).
my_append(Xs,[c],[a,b,c]).
my_append(Xs,Ys,[a,b,c]).
my_prefix([a,b,c],Z).

After drawing the execution trees, enable tracking on my_append and my_prefix
by running (two separate queries)

trace(my_append).
trace(my_prefix).

in swipl. Now, execute the above queries and try and match what you drew to
the way the actual query is executed in swipl. To turn off 'trace', type the
query 'nodebug'.

If you prefer a graphical debugger/trace, enter the query 'manpce.' . You will
see a small XPCE manual window. Under the 'Tools' menu select: "Prolog
graphical tracer".

*/

/* Problem 1: Given a list of subgoals, prove/1 succeeds only if
      all of the subgoals in the list succeeds. Note: the scope of variable
      names is the entire list. 
*/

/* Problem 1 Answer */

prove([]).
prove([H|T]):-
	H, prove(T).

/* Problem 1 Test */

:- prove([=(A,5),is(B,+(4,5)),C is max(5,2),A=C]) , A = 5, B = 9, C = 5. %succeeds
:- prove([member(A,[1,4,5,7]),is(A, max(5,2))]). %succeeds
:- prove([is(A,+(3,2)),is(B,+(4,5)),C is max(5,2),A=C]).  %fails
:- prove([=(A,5),is(B,+(1,2)),B is max(9,A)]). %fails

/* Problem 2:
   In class we discussed the 'is' predicate for evaluating expressions. Write a
   predicate results/2.
   result(Elst,RLst) succeeds if Rlst unifies with the values computed from 
   the list of expressions, Elst.
   Your solution should be able to handle any operators, not just +, *, etc.
*/

/* Problem 2 Answer: */

result([],_).
result([Eh|Et],[Rh|Rt]):-
	Rh is Eh, result(Et,Rt).

/* Problem 2 Test */
:- result([],[]).
:- result([+(3,7), mod(104,7),-(5)],[10, 6, -5]).
:- result([+(3,7), +(15, -(3,11))],X), X = [10, 7].

:- result([+(3,7), mod(104,7)],[10,13]) -> fail ; true.

/* Problem 3:
   Write a predicate sumlist(List,Sum) which succeeds if Sum is the total value
   of all the elements of List. This will be a top down recursion.
   The recursion clause will add the current value to the result of the sum
   of the rest of the list.
   We have already provided the base case for this predicate underneath
   'Problem 3 Answer'. You just need to add the recursive clause.
*/

/* Problem 3 Answer */

sumlist([], 0).
sumlist([Lh|Lt],Sum):-
	sumlist(Lt,Sn), Sum is Lh+Sn.

/* Problem 3 Test */
/* There should be no warnings when compiling,
   tests which are supposed to fail are written as such */

:- sumlist([], 0).
:- sumlist([], 1) -> fail ; true.
:- sumlist([1,2,3,4], 10).
:- sumlist([1], 1).

/* Problem 4:
   Write the predicate sumlist2(List,Sum) which succeeds if Sum is the sum total
   of all the elements of List. Instead of adding the current value to the
   result of the sum of the tail, you will calculate the partial sum of the all
   the elements you have reached so far. You will need an extra argument to
   store the partial sum, so you will write an auxilitary predicate sumlist2/3
   to handle the extra argument.

   Underneath 'Problem 2 Answer' we have provided sumlist2/2, which calls the
   auxiliary predicate sumlist2/3. We have also provided the base case for the
   auxiliary predicate. You just need to add the recursive clause for
   sumlist2/3.

*/

/* Problem 4 Answer */

sumlist2(List,Sum) :- sumlist2(List, 0, Sum).
sumlist2([], Sum, Sum).
sumlist2([Lh|Lt], PartialSum, Sum):-
	PSN is PartialSum+Lh,
	sumlist2(Lt, PSN, Sum).

/* Problem 4 Test */

:- sumlist2([], 0).
:- sumlist2([], 1) -> fail ; true.
:- sumlist2([1,2,3,4], 10).
:- sumlist2([1], 1).

/* Problem 5:
   Write the predicate sumPartialR(N, SumLst), which succeeds as follows:
   given a number N, SumLst is a sequence of sums such that first number in
   S is the sum of all the numbers from N to 1, the second number in S the sum
   of all the numbers from N-1 down to 1, and so on.
   In other words, SumLst = [N+(N-1)+..+1, (N-1)+(N-2)+..+1, ..., 1].
   For example:

     ?- sumPartialR(6,S).
     S = [21, 15, 10, 6, 3, 1] .

   This problem can be solved in 2 clauses.
*/


/* Problem 5 Answer */

sumPartialR(1, [1]).
sumPartialR(N, [Lhh,Lh|Lt]):-
	N>1,
	Lhh is Lh + N,
	Nn is N-1,
	sumPartialR(Nn, [Lh|Lt]).

/* Problem 5 Test */

:- sumPartialR(1, [1]).
:- sumPartialR(1, []) -> fail ; true.
:- sumPartialR(2, [3, 1]).
:- sumPartialR(6, [21, 15, 10, 6, 3, 1]).



/* Problem 6:
   Write the predicate sumPartialL(N, SumLst). This problem is very similar to
   Problem 3, but has one key difference. The sum totals accumulate from left
   to right, so the SumLst generated will be different. For example, the first
   value in S will be N, the second value will be N + (N-1), and so on.
   In other words, SumLst = [N, N+(N-1), ..., N+(N-1)+(N-2)+...+1].
   For example,

     ?- sumPartialL(6,S).
     S = [6, 11, 15, 18, 20, 21]

   It would be helpful to follow the idea used in sumlist2. So your first
   clause should be:

       sumPartialL(N,Lst):-sumPartialL(N,N,Lst).

   You need to add 2 additional clauses.*/

/* Problem 6 Answer */

sumPartialL(N, L):-sumPartialL(N, 0, L).
sumPartialL(0, _, []).
sumPartialL(N, P, [Lh|Lt]):-
	N>0,
	Nn is N-1,
	Lh is N + P,
	sumPartialL(Nn, Lh, Lt).

/* Problem 6 Test */

:- sumPartialL(1, [1]).
:- sumPartialL(1, []) -> fail ; true.
:- sumPartialL(6, [6, 11, 15, 18, 20, 21]).


/* Problem 7:
   We will use a predicate edge(X,Y) to encode a graph.
   edge(X,Y) is true if there is a directed edge from X to Y.
   The following is a mini graph encoded in Prolog. */

edge(a,b).
edge(a,f).
edge(a,c).
edge(b,a).
edge(b,c).
edge(b,d).
edge(c,e).
edge(f,e).

/* Using your knowledge of backtracking and the findall predicate, write
   predicates outgoing/2 and incoming/2.

   outgoing(X,Y) should succeed if Y is a list of all immediate vertices
   reached from X's outgoing edges. incoming(X,Y) should succeed if Y is a
   list of all vertices that have outgoing edges to X.

   You can find definitions of graph terms at 
    https://en.wikipedia.org/wiki/Glossary_of_graph_theory_terms
*/

/* Problem 7 Answer */

outgoing(X,Y):-
	findall(Z, edge(X,Z), Y).
	
incoming(X,Y):-
	findall(Z, edge(Z,X), Y).

/* Problem 7 Test */
:- outgoing(a,X), X = [b,f,c].
:- outgoing(e,X), X = [].
:- incoming(a,X), X = [b].
:- incoming(f,X), X = [a].

:- outgoing(e,X), X = [a] -> fail ; true.
:- incoming(e,X), X = [] -> fail ; true.



/* Problem 8:
   (Exercise 3.5 from Learn Prolog Now!)
   Binary trees are trees where all internal nodes have exactly two children.
   The smallest binary trees consist of only one leaf node. We will represent
   leaf nodes as leaf(Label) . For instance, leaf(3) and leaf(7) are leaf
   nodes, and therefore small binary trees.

   Given two binary trees B1 and B2 we can combine them into one binary tree
   using the functor tree/2 as follows: tree(B1,B2) .
   So, from the leaves leaf(1) and leaf(2) we can build the binary tree
   tree(leaf(1),leaf(2)) .
   From the binary trees tree(leaf(1),leaf(2)) and leaf(4) we can build
   tree( tree(leaf(1), leaf(2)), leaf(4)) .

   Define a predicate swap/2 , which produces the mirror image of the binary
   tree that is its first argument. For example:

   ?-  swap( tree( tree(leaf(1), leaf(2)), leaf(4)), T).
   T  =  tree( leaf(4), tree(leaf(2), leaf(1))).
*/

/* Problem 8 Answer: */

swap(leaf(X), leaf(X)).
swap(tree(Tl, Tr),tree(Bl, Br)):-
	swap(Tl, Br), swap(Tr, Bl).

/* Problem 8 Test: */
:- swap( tree( tree(leaf(1), leaf(2)), leaf(4)), T), T  =  tree( leaf(4), tree(leaf(2), leaf(1))).
:- swap(leaf(1), leaf(1)).
:- swap(tree(leaf(1), leaf(2)), tree(leaf(1), leaf(2))) -> fail ; true.


/* Problem 9:
   Another way to represent a binary tree is use the functors node and leaf.
   The internal node, node(Value,Ltree,Rtree), has a Value and exactly two children. 
   The leaf term has no arguments.
    
   node(5, leaf ,leaf) is a binary tree.  The root of the tree has
   the value 5 and it's children are both leaves.

   node(5,node(3,leaf,leaf),leaf) is a binary tree.  The root of the tree has
   the value 5 and it's child has a value 3 and the other child is a leaf.

   Define a predicate isBinarySearchTree(+BST) that succeeds if BST is a
   binary search tree.  
   You might find:

   (https://www.geeksforgeeks.org/binary-search-tree-data-structure/#basic)

   helpful if you've forgotten the property of a binary search tree.

   
 */

  
/* Problem 9 Answer: */

/* Problem 9 Test: */

t1(T1) :- T1=   node(5,node(3,leaf,leaf),leaf).
t2(T2) :- T2 =  node(5,node(3,node(1,leaf,leaf),leaf),node(7,leaf,leaf)). 
t3(T3) :- T3 =  node(5,node(3,node(2,node(1,leaf,leaf),leaf),leaf),node(7,leaf,leaf)).
t4(T4) :- T4 =  node(5,node(3,node(2,node(3,leaf,leaf),leaf)),node(7,leaf,leaf)).
t5(T5) :- T5 =  node(5,node(3,node(7,node(1,leaf,leaf),leaf),leaf),node(3,leaf,leaf)).

:- t1(T), isBinarySearchTree(T). % SUCCEED
:- t2(T), isBinarySearchTree(T). % SUCCEED
:- t3(T), isBinarySearchTree(T). % SUCCEED
:- t4(T), isBinarySearchTree(T);true. % FAIL
:- t5(T), isBinarySearchTree(T);true. % FAIL



/* Problem 10:
   A good example of symbolic computation is symbolic differentiation. Below
   are the rules for symbolic differentiation where U, V are mathematical
   expressions, C is a number constant, N is an integer constant and x is a
   variable:

        dx/dx = 1
        d(C)/dx = 0.
        d(Cx)/dx = C
        d(-U)/dx = -(dU/dx)
        d(U+V)/dx = dU/dx + dV/dx
        d(U-V)/dx = dU/dx - dV/dx
        d(U*V)/dx = U*(dV/dx) + V*(dU/dx)
        d(U^N)/dx = N*U^(N-1)*(dU/dx)

   Translate these rules into Prolog. (Please keep the order of the rules the
   same for testing purposes).
*/

/* Problem 10 Answer: */

/* Problem 10 Test: */

% :- d(x,x,R), R = 1 .
% :- d(7*x,x,R), R = 7 .
% :- d(x +2*(x^3 + x*x),x,Result), Result = 1+ (2* (3*x^2*1+ (x*1+x*1))+ (x^3+x*x)*0) .
% :- d(-(1.24*x -x^3),x,Result), Result = - (1.24-3*x^2*1) .
% :- d(-(1.24*x -2*x^3),x,Result), Result = - (1.24- (2* (3*x^2*1)+x^3*0)) .

% Pay careful attention to why this fails.
% :- d(x +2*(x^3 + x*x),x,Result), Result = 1+ (2* (3*x^(3-1)*1+ (x*1+x*1))+ (x^3+x*x)*0) -> fail ; true.





