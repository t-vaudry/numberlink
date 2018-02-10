:- begin_tests(solution).

test(solution) :- prolog_while(5).

:- end_tests(solution).

prolog_while(0) :- !.
prolog_while(N) :-
    N1 is N - 1,
    atom_string(N,Str),
    solution(Str),
    prolog_while(N1).

% ----------------------------------------------------------------------------- %
% Name: solution
% Input: FileNumber - where FileNumber represents the suffix, X, to the input 
%        file "inputX.txt"
% Output: None
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of the function is to read the input from the input
% file, and pass the raw input to Python. Using the Python script, the input
% is formatted to the prolog lists, and is used in the game function,
% "numberlink", which is called from the "callNumberlink" function. From the
% returned list of the game, a string representation is created, and passed to
% the output Python script, which formats the output as specified, and then
% writes it to a file, named "outputX.txt" using the same suffix from the
% input file. The function stops after finding the first solution to the
% numberlink puzzle.
% ----------------------------------------------------------------------------- %
solution(FileNumber) :-
    readFromFile(FileNumber,RawInput),
    setup_call_cleanup(
        process_create(path(python2),["input.py",RawInput],[stdout(pipe(PyIn))]),
        read_lines(PyIn, FormattedInput),
        close(PyIn)),
    callNumberlink(FormattedInput,Paths),
    getListString(Paths,PathsStr),
    setup_call_cleanup(
        process_create(path(python2),["output.py",RawInput,PathsStr],[stdout(pipe(PyOut))]),
        read_lines(PyOut, Output),
        close(PyOut)),
    writeToFile(FileNumber,Output),
    !.

% ----------------------------------------------------------------------------- %
% Name: callNumberlink
% Input: [BoardStr, PointsStr] - where BoardStr is the NxN board as a string,
%        with the values of the points inserted, and PointsStr is a list of
%        connecting points to solve the puzzle.
% Output: Paths - where Paths represents the various paths to connect the
%         original points from the puzzle.
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of this function is to convert the input from python
% into usable Prolog atoms. The "numberlink" function is then called to solve
% the puzzle. 
% ----------------------------------------------------------------------------- %
callNumberlink([BoardStr,PointsStr],Paths) :-
    read_term_from_atom(BoardStr,Board,[]),
    read_term_from_atom(PointsStr,Points,[]),
    numberlink(Board,Points,Paths).

% ----------------------------------------------------------------------------- %
% Name: numberlink
% Input: Board, Points - where Board is the NxN board as a list, with 
%        the values of the points inserted, and Points is a list of connecting
%        points to solve the puzzle.
% Output: Paths - where Paths represents the various paths to connect the
%         original points from the puzzle.
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of this function is to call the recursive "solve"
% function, to complete the paths between the points, and fill the board. The
% reverse call is used to see the paths in ascending order, and checking the
% board is done using the "not_empty" function.
% ----------------------------------------------------------------------------- %
numberlink(Board,Points,Paths) :-
    solve(Points,Board,[],Temp),
    reverse(Temp,Paths),
    not_empty(Board),
    !.

% ----------------------------------------------------------------------------- %
% Name: solve
% Input: Points as [H|T], Board, Paths - where Points is the list of points
%        that need to be connected, Board is the NxN board to be filled, and
%        Paths is a list used to keep track of the paths.
% Output: NewPaths - where NewPaths represents the various paths to connect
%         the original points from the puzzle.
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of the function is to recursively connect the points in
% the list, and place the values on the board. The recursive function ends
% when there are no more points to connect.
% ----------------------------------------------------------------------------- %
solve([],_,Paths,Paths) :- !.
solve([H|T],Board,Paths,NewPaths) :-
    connect(H,Board,[],Path),
    reverse(Path, NewPath),
    solve(T,Board,[NewPath|Paths],NewPaths).

% ----------------------------------------------------------------------------- %
% Name: not_empty
% Input: Board - where Board is the NxN board
% Output: True or False
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of the function is to check if all elements on the board
% have an integer value.
% ----------------------------------------------------------------------------- %
not_empty(Board) :-
    flatten(Board, FlatBoard),
    maplist(integer, FlatBoard).

% ----------------------------------------------------------------------------- %
% Name: connect
% Input: P1 as (X,Y), P2 as (Z,W), Board, Visited - where P1 is the first
%        point to be connected to P2, Board is the NxN board with values, and
%        Visited is a list of the points that have been visited for this path.
% Output: Path
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of this function is to recursively connect 2 points, P1
% and P2. If the points are not linked by default, the recursive function
% finds another point P3 (denoted as (A,B)) to attempt to connect the points.
% ----------------------------------------------------------------------------- %
connect([(X,Y),(Z,W)|_],Board,Visited,[[W,Z],[Y,X]|Visited]) :-
    linked((X,Y),(Z,W)),
    insert((X,Y),(Z,W),Board).

connect([(X,Y),(Z,W)|_],Board,Visited,Path) :-
    linked((X,Y),(A,B)),
    (A,B) \== (Z,W),
    not_visited((X,Y),(A,B),Board),
    insert((X,Y),(A,B),Board),
    connect([(A,B),(Z,W)],Board,[[Y,X]|Visited],Path).

% ----------------------------------------------------------------------------- %
% Name: linked
% Input: P1 as (X,Y), P2 as (X',Y') - where P1 is the point to becconnected to
%        P2
% Output: True or False
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of this function is to check for adjacency between two
% points, P1 and P2. If the X coordinates are equal, the check is done to see
% if the Y values are above or below. If the Y coordinates are equal, the
% check is done to see if the X values are to the right of left.
% ----------------------------------------------------------------------------- %
linked((X,Y1),(X,Y2)) :- Y2 is Y1+1.
linked((X,Y1),(X,Y2)) :- Y2 is Y1-1.
linked((X1,Y),(X2,Y)) :- X2 is X1+1.
linked((X1,Y),(X2,Y)) :- X2 is X1-1.

% ----------------------------------------------------------------------------- %
% Name: insert
% Input: P1 as (X1,Y1), P2 as (X2,Y2), Board - where P1 is the point used to
%        get the value for P2, which is being place on the board, and Board is
%        the NxN board with values.
% Output: True or False
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of this function is to attempt to insert the value from
% point P1 into the point P2. If the position is already filled, this will
% fail, otherwise the value is inserted into P2 and returns successfully.
% ----------------------------------------------------------------------------- %
insert((X1,Y1), (X2,Y2), Board) :-
    nth0(X1, Board, Row1),
    nth0(Y1, Row1, Number),
    nth0(X2, Board, Row2),
    nth0(Y2, Row2, Number).

% ----------------------------------------------------------------------------- %
% Name: not_visited
% Input: P1 as (X1,Y1), P2 as (X2,Y2), Board - where P1 is the point used to
%        get the value for P2, which is being place on the board, and Board is
%        the NxN board with values.
% Output: True or False
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of this function is to see whether the point P2 has
% already been visited in the current path. That is, is the value at point P2
% equal to the value at P1. Uses function "take" to retrieve the value at the
% nth position.
% ----------------------------------------------------------------------------- %
not_visited((X1,Y1),(X2,Y2),Board) :-
    nth0(X1, Board, Row1),
    nth0(Y1, Row1, Number),
    nth0(X2, Board, Row2),
    take(Y2,Row2,X),
    X \== Number.

% ----------------------------------------------------------------------------- %
% Name: take
% Input: N, List as [_|T] - where N is the nth value, and List is the row on
%        the board.
% Output: X - where X is the nth element value.
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of this function is to get the value of the nth element.
% ----------------------------------------------------------------------------- %
take(0,[H|_],H) :- !.
take(N,[_|T],X) :- N1 is N-1, take(N1,T,X).

% ----------------------------------------------------------------------------- %
% Name: read_lines
% Input: Out - where Out is the output from Python
% Output: Lines - where Lines is the lines read from Python output
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of this function is to get the Python output converted
% into usable Prolog variables.
% ----------------------------------------------------------------------------- %
read_lines(Out, Lines) :-
    read_line_to_codes(Out, Line1),
    read_lines(Line1, Out, Lines).

read_lines(end_of_file, _, []) :- !.
read_lines(Codes, Out, [Line|Lines]) :-
    atom_codes(Line, Codes),
    read_line_to_codes(Out, Line2),
    read_lines(Line2, Out, Lines).

% ----------------------------------------------------------------------------- %
% Name: readFromFile
% Input: Num - where Num is the suffix for the input file.
% Output: Str - where Str is the content of the input file.
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of this function is to read the input file.
% ----------------------------------------------------------------------------- %
readFromFile(Num,Str) :-
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

% ----------------------------------------------------------------------------- %
% Name: getListString
% Input: List - where List is the List to be converted
% Output: Str - where Str is the string copy of the List
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of this function is to convert the Prolog list into a
% string for output parsing and formatting.
% ----------------------------------------------------------------------------- %
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

% ----------------------------------------------------------------------------- %
% Name: writeToFile
% Input: Num - where Num is the suffix for the output file.
% Output: List as [H|T] - where List is the content for the output file.
% ----------------------------------------------------------------------------- %
% Purpose: The purpose of this function is to writ to the output file.
% ----------------------------------------------------------------------------- %
writeToFile(Num,[H]) :-
    string_concat("output/output",Num,Temp),
    string_concat(Temp,".txt",OutFile),
    open(OutFile,append,Stream),
    write(Stream,H), nl(Stream),
    close(Stream).
writeToFile(Num,[H|T]) :-
    string_concat("output/output",Num,Temp),
    string_concat(Temp,".txt",OutFile),
    open(OutFile,append,Stream),
    write(Stream,H), nl(Stream),
    close(Stream),
    writeToFile(Num,T).