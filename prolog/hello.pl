sex(male, alex).
sex(male, piter).
sex(female, cate).
sex(female, riley).
sex(female, olga).
sex(female, nina).
sex(male, ostin).

man(X):- sex(male, X).
woman(X):- sex(female, X).

parents(alex, cate, piter).
parents(riley, piter, cate).
parents(olga, riley, ostin).
parents(nina, riley, ostin).

isParent(X, Y):- parents(Y, X, _), X \= Y.
isParent(X, Y):- parents(Y, _, X), X \= Y.

isGrandParent(X, Y):- isParent(X, Z), isParent(Z, Y).

isParents(A, X, Y):- parents(A, X, Y).
isParents(A, X, Y):- parents(A, Y, X).

siblings(X, Y):- isParents(X, A, B), isParents(Y, A, B), X \= Y.

sister(X, Y):- siblings(X, Y), woman(X).

brother(X, Y):- siblings(X, Y), man(X).

father(X, Y):- man(X), isParent(X, Y).
mother(X, Y):- woman(X), isParent(X, Y).

grandPa(X, Y):- isGrandParent(X, Y), man(X).
grandMa(X, Y):- isGrandParent(X, Y), woman(X).
