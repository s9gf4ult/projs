name(semenov).
name(borisov).
name(ivanov).

has_sister(borisov).

younger(X, semenov) :-
    tokar(X).
younger(X, _) :-
    slesar(X).
%% younger(X, Z) :-
%%     younger(X, Y),
%%     younger(Y, Z).
    
tokar(X) :-
    name(X),
    not(svarshik(X)),
    not(slesar(X)).

svarshik(X) :-
    name(X),
    not(tokar(X)),
    not(slesar(X)).

slesar(X) :-
    name(X),
    not(tokar(X)),
    not(svarshik(X)),
    not(has_sister(X)).
