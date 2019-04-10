% This is very similar to the range function in python or 
% [a..b] in many functional languages. It creates a list of Integers
% starting from Start and until End inclusive of both Start and End
range(Start, End, Jump, [Start]) :-
  Start + Jump > End, !.
range(Start, End, Jump, [Start|Rest]) :- 
  NextStart is Start + Jump, !,
  range(NextStart, End, Jump, Rest).

% N is the number of queens and Lst is a list of
% clauses.
nqueensDIMACS(N, Lst) :-
  row_clauses(N, RowClauses),
  col_clauses(N, ColumnClauses),
  diagonal_clauses(N, DiagonalClauses),
  append(RowClauses, ColumnClauses, AllButDiagonalClauses),
  append(AllButDiagonalClauses, DiagonalClauses, Lst).


row_clauses(N, Lst) :- row_clause_helper(N, 1, Lst).
row_clause_helper(N, Start, []) :- Start > N * N, !.
row_clause_helper(N, Start, [Row|OtherAns]) :-
  NewStart is Start + N,
  RowEnd is Start + N - 1,
  range(Start, RowEnd, 1, Row),
  row_clause_helper(N, NewStart, OtherAns).


col_clauses(N, Lst) :- col_clause_helper(N, 1, Lst).
col_clause_helper(N, Start, []) :- Start > N, !.
col_clause_helper(N, Start, [Col|OtherAns]) :-
  NewStart is Start + 1,
  ColEnd is Start + ((N - 1) * N),
  range(Start, ColEnd, N, Col),
  col_clause_helper(N, NewStart, OtherAns).


add_to_lst([], [], _).
add_to_lst([H1|Lst], [H2|NewList], Offset) :-
  H2 is H1 + Offset, add_to_lst(Lst, NewList, Offset).

without_last([], []).
without_last([_], []) :- !.
without_last([X|Xs], [X|WithoutLast]) :- 
    without_last(Xs, WithoutLast).

diagonal_clauses(1, [[1]]) :- !.
diagonal_clauses(N, Lst) :-
  diagonal_right_clauses(N, RightDiagonalClauses),
  diagonal_left_clauses(N, LeftDiagonalClauses),
  append(RightDiagonalClauses, LeftDiagonalClauses, Lst).


diagonal_right_clauses(N, [MainDiagonal|AllSideDiagonals]) :-
  LastTile is N * N, Offset is N + 1,
  range(1, LastTile, Offset, MainDiagonal),
  diagonal_helper(MainDiagonal, AllRightDiagonals, 1),
  diagonal_helper(MainDiagonal, AllLeftDiagonals, -1),
  append(AllRightDiagonals, AllLeftDiagonals, AllSideDiagonals).

diagonal_left_clauses(N, [LeftMainDiagonal|AllSideDiagonals]) :-
  LastTile is N * (N - 1) + 1, Offset is N - 1, Start = N,
  range(Start, LastTile, Offset, MainDiagonal),
  reverse(MainDiagonal, LeftMainDiagonal),
  diagonal_helper(LeftMainDiagonal, AllRightDiagonals, 1),
  diagonal_helper(LeftMainDiagonal, AllLeftDiagonals, -1),
  append(AllRightDiagonals, AllLeftDiagonals, AllSideDiagonals).


diagonal_helper([_], [], _) :- !.
diagonal_helper(Diagonal, [NextDiagonal|AllOtherDiagonals], Offset) :-
  Offset > 0,
  add_to_lst(Diagonal, NewList, Offset),
  without_last(NewList, NextDiagonal), !,
  diagonal_helper(NextDiagonal, AllOtherDiagonals, Offset).
diagonal_helper(Diagonal, [NextDiagonal|AllOtherDiagonals], Offset) :-
  add_to_lst(Diagonal, NewList, Offset),
  NewList = [_|NextDiagonal],
  diagonal_helper(NextDiagonal, AllOtherDiagonals, Offset).
