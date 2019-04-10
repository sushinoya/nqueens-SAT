
% pick/3 takes three arguments - Elem, List, Rest
% Elem is a member of the List and Rest is the List without Elem
pick(X, [Head|Tail], Rest) :-
    pick_helper(Tail, Head, X, Rest).

pick_helper(Tail, Head, Head, Tail).
pick_helper([Head2|Tail], Head, X, [Head|Rest]) :-
    pick_helper(Tail, Head2, X, Rest).

% This is very similar to the range function in python or 
% [a..b] in many functional languages. It creates a list of Integers
% starting from Start and until End inclusive of both Start and End
enuemrate_integers(Start,Start,[Start]).
enuemrate_integers(Start,End,[Start|Rest]) :- 
  Start < End, NextStart is Start + 1, 
  enuemrate_integers(NextStart, End, Rest).

% Our intermediate answer is in the form of a list of row numbers
% where the first element is the row number of column one and the
% second element is the row nmber of column two and so on.
% This procedure converts that row_num list into a list of tiles num
% as expected by the assignment.
row_to_tile_num(RowNumLst, TileNumLst) :-
  counter_helper(RowNumLst, TileNumLst, 0).

counter_helper([], [], _).
counter_helper([RowNum|RowNumLst], [TileNum|TileNumLst], ColNum) :-
  TileNum is (ColNum * 8) + RowNum, NextCol is ColNum + 1,
  counter_helper(RowNumLst, TileNumLst, NextCol).

% This procedure takes in a row number, Queen and a list of 
% other row numbers, OtherQueens and ensures that Queen is not
% diagonally attacking any of the other queens.
not_confronting_diagonally(Queen, OtherQueens) :- \+ diagonal_helper(Queen, OtherQueens, 1).
diagonal_helper(Queen, [FirstQueen|_], Offset) :- FirstQueen is Queen + Offset.
diagonal_helper(Queen, [FirstQueen|_], Offset) :- FirstQueen is Queen - Offset.
diagonal_helper(Queen, [_|Rest], Offset) :-
  NextCol is Offset + 1,
  diagonal_helper(Queen, Rest, NextCol).

% This prodecure takes in three arguments - SolSoFar, PossiblePositions and Sol.
% Sol is the final list of row numbers which is a solution to the nqueens problem
% PossiblePositions are a possible list of Row Numbers which initially is just [1..N]
% but as each column is assigned a row number, the PossiblePositions shrink until all
% columns have a row_number.
nqueens_helper(Sol, [], Sol).
nqueens_helper(SolSoFar, PossiblePositions, Sol) :-
  pick(Pos, PossiblePositions, OtherPos), % Pick a row number
  not_confronting_diagonally(Pos, SolSoFar), % Check it doesn't confront with our row number assignments so far
  nqueens_helper([Pos|SolSoFar], OtherPos, Sol). % Add the position to SolSoFar and recurse until we exhaust PossiblePositions.


% Main procedure which first computes the row numbers of the solutions
% then converts them into tile numbers.
nqueens(N, Sol) :-
  enuemrate_integers(1, N, Integers),
  nqueens_helper([], Integers, RowNums),
  row_to_tile_num(RowNums, Sol).
