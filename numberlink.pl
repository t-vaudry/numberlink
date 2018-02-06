:- begin_tests(solution).

test(solution) :-
    solution("1").

test(solution) :-
    solution("2").

test(solution) :-
    solution("3").

test(solution) :-
    solution("4").

test(solution) :-
    solution("5").

:- end_tests(solution).

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
    first_solution([P,Temp],[solve(L,N,P,[],Temp)],[]),
    reverse(Temp,Path),
    not_empty(P),
    !.

solve([],_,_,Paths,Paths) :- !.
solve([H|T],N,Q,Paths,NewPaths) :-
    connect(H,N,Q,[],Path),
    reverse(Path, NewPath),
    solve(T,N,Q,[NewPath|Paths],NewPaths).

solution(FileNumber) :-
    readFromFile(Input,FileNumber),
    setup_call_cleanup(
    process_create(path(python),["input.py",Input],[stdout(pipe(Out1))]),
    read_lines(Out1, FormattedInput),
    close(Out1)),
    callNumberlink(FormattedInput,Path),
    getListString(Path,PathStr),
    setup_call_cleanup(process_create(path(python),["output.py",Input, PathStr],[stdout(pipe(Out2))]),
    read_lines(Out2, Output),
    close(Out2)),
    writeToFile(Output,FileNumber),
    !.

writeToFile([H],Num) :-
    string_concat("output/output",Num,Temp),
    string_concat(Temp,".txt",OutFile),
    open(OutFile,append,Stream),
    write(Stream,H), nl(Stream),
    close(Stream).
writeToFile([H|T],Num) :-
    string_concat("output/output",Num,Temp),
    string_concat(Temp,".txt",OutFile),
    open(OutFile,append,Stream),
    write(Stream,H), nl(Stream),
    close(Stream),
    writeToFile(T,Num).

read_lines(Out, Lines) :-
    read_line_to_codes(Out, Line1),
    read_lines(Line1, Out, Lines).

read_lines(end_of_file, _, []) :- !.
read_lines(Codes, Out, [Line|Lines]) :-
    atom_codes(Line, Codes),
    read_line_to_codes(Out, Line2),
    read_lines(Line2, Out, Lines).

callNumberlink([N,L,P|_],Path) :-
    atom_number(N,Num),
    read_term_from_atom(L,Board,[]),
    read_term_from_atom(P,Points,[]),
    numberlink(Num,Board,Points,Path).

getListString(List,Str) :-
    string_concat("","[",Temp),
    convertList(List,Temp2),
    string_concat(Temp,Temp2,Temp3),
    string_concat(Temp3,"]",Str).

convertList([H],Str) :-
    string_concat("","[",Temp),
    convertSubList(H,NewTemp),
    string_concat(Temp,NewTemp,Temp2),
    string_concat(Temp2,"]",Str).
    
convertList([H|T],Str) :-
    string_concat("","[",Temp1),
    convertSubList(H,Temp2),
    string_concat(Temp1,Temp2,Temp3),
    string_concat(Temp3,"]",Temp4),
    convertList(T,NewTemp),
    string_concat(Temp4,",",Temp5),
    string_concat(Temp5,NewTemp,Str),
    !.

convertSubList([H],Str) :-
    convertPoint(H,Str).
convertSubList([H|T],Str) :-
    convertPoint(H,Temp),
    convertSubList(T,NewTemp),
    string_concat(Temp,",",Temp2),
    string_concat(Temp2,NewTemp,Str).

convertPoint([H,T|_],Str) :-
    string_concat("","[",Temp1),
    atom_number(Num1,H),
    atomic_concat(Temp1,Num1,Temp2),
    string_concat(Temp2,",",Temp3),
    atom_number(Num2,T),
    atomic_concat(Temp3,Num2,Temp4),
    string_concat(Temp4,"]",Str).

readFromFile(Str,Num) :-
    string_concat("input/input",Num,Temp),
    string_concat(Temp,".txt",InFile),
    open(InFile, read, Lines),
    read_string(Lines,_,Str),
    close(Lines).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).