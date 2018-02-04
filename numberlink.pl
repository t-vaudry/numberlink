:- use_module(library(thread)).

size(X, N) :-
    X >= 0, X =< N.

insert((X1,Y1), (X2,Y2), Board) :-
    nth0(X1, Board, Row1),
    nth0(Y1, Row1, Number),
    nth0(X2, Board, Row2),
    nth0(Y2, Row2, Number).

take(0,[H|_],H) :- !.
take(N,[_|T],X) :- N1 is N-1, take(N1,T,X).

initialized(X) :-
    integer(X).

not_visited((X1,Y1),(X2,Y2),Board) :-
    nth0(X1, Board, Row1),
    nth0(Y1, Row1, Number),
    nth0(X2, Board, Row2),
    take(Y2,Row2,X),
    X \== Number.

not_empty(X) :-
    maplist(sub_not_empty, X).

sub_not_empty(X) :-
    maplist(initialized, X).

linked(N,(X,Y1),(X,Y2)) :- size(Y1+1,N) -> Y2 is Y1+1.
linked(N,(X,Y1),(X,Y2)) :- size(Y1-1,N) -> Y2 is Y1-1.
linked(N,(X1,Y),(X2,Y)) :- size(X1+1,N) -> X2 is X1+1.
linked(N,(X1,Y),(X2,Y)) :- size(X1-1,N) -> X2 is X1-1.

connect([(X,Y),(Z,W)|_],N,P,Visited,[[W,Z],[Y,X]|Visited]) :-
    linked(N,(X,Y),(Z,W)),
    insert((X,Y),(Z,W),P).

connect([(X,Y),(Z,W)|_],N,P,Visited,Path) :-
    linked(N,(X,Y),(A,B)),
    (A,B) \== (Z,W),
    not_visited((X,Y),(A,B),P),
    insert((X,Y),(A,B),P),
    connect([(A,B),(Z,W)],N,P,[[Y,X]|Visited],Path).

numberlink(N,P,L,Path) :-
    % other(3, [[_,_,_,_],[_,1,2,_],[_,1,2,_],[_,_,_,_]],[[(1,1),(2,1)],[(1,2),(2,2)]], Q).
    % other(4, [[1,2,3,_,_],[_,_,_,4,_],[_,4,2,_,_],[_,_,_,_,_],[_,_,1,3,_]],[[(0,0),(4,2)],[(0,1),(2,2)],[(0,2),(4,3)],[(1,3),(2,1)]], Q).
    %solve(L,N,P,[],Temp),
    first_solution([P,Temp],[solve(L,N,P,[],Temp)],[]),
    reverse(Temp,Path),
    not_empty(P),
    !.

solve([],_,_,Paths,Paths) :- !.
solve([H|T],N,Q,Paths,NewPaths) :-
    connect(H,N,Q,[],Path),
    reverse(Path, NewPath),
    solve(T,N,Q,[NewPath|Paths],NewPaths).