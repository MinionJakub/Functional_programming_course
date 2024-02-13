cat(tome).
cat(finio).
animal(X) :- cat(X).
parent(pawel,asia).
male(pawel).
parent(andrzej,pawel).
male(andrzej).
father(X,Y) :- parent(X,Y),male(X).
grandparent(X,Y) :- parent(X,Z),parent(Z,Y).
?- grandparent(X,Y).
?- animal(X).
?- animal(tome).

male(james1).
male(charles1).
male(charles2).
male(james2).
male(george1).
male(fred).    

female(catherine).
female(elizabeth).
female(sophia).

parent(james1,charles1).
parent(james1,elizabeth).
parent( charles1,charles2).
parent( charles1,catherine).
parent(charles1,james2).
parent(elizabeth,sophia).
parent( sophia,george1).
parent(james1,fred).



?- male(george1).
?- parent(sophia, sophia).
?- parent(charles1,X).

son(X, Y) :- parent(X, Y), male(Y).    
father_son(X, Y) :- father(X, Y), son(X, Y).    

?- father_son(X, Y).
