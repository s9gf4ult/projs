name(semenov).
name(borisov).
name(ivanov).

has_sister(borisov).
%% has_sister(semenov).

younger(X, semenov) :-
    X \== semenov,
    tokar(X).
younger(X, Y) :-
    X \== Y,
    slesar(X).
younger(X, Y) :-
    X \== Y,
    not(younger(Y, X)).
younger(X, Z) :-
    X \== Z,
    younger(X, Y),
    younger(Y, Z).
    
tokar(X) :-
    name(X).
    %% X \== semenov.
    %% svarshik(Y),
    %% slesar(Z),
    %% X \== Y,
    %% X \== Z.

svarshik(X) :-
    name(X).
    %% tokar(Y),
    %% slesar(Z),
    %% X \== Y,
    %% X \== Z.


slesar(X) :-
    name(X),
    %% X \== semenov,
    %% tokar(Y),
    %% svarshik(Z),
    has_sister(I),
    %% X \== Y,
    %% X \== Z,
    X \== I.

solve(X, Y, Z) :-
    tokar(X),
    svarshik(Y),
    slesar(Z),
    X \== Y,
    X \== Z,
    Y \== Z.
