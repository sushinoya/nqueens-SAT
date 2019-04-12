:- use_module(library(lists)).

% This is very similar to the range function in python or 
% [a..b] in many functional languages. It creates a list of Integers
% starting from Start and until End inclusive of both Start and End.
% It also has a Jump which is basically how many numbers to "jump"
% between each element.
range(Start, End, Jump, [Start]) :-
  Start + Jump > End, !.
range(Start, End, Jump, [Start|Rest]) :- 
  NextStart is Start + Jump, !,
  range(NextStart, End, Jump, Rest).

% Computes the sum of the first n natural numbers
sum_to_n(N, Res) :- Res is N * (N+1) / 2 .

% NUMBER OF CLAUSES SECTION: This section computes the number of clauses for N
% so that we can write in the "p cnf <..> <..>" clause.
number_of_clauses(N, NumClauses) :-
  num_of_row_or_col_clauses(N, NumRowOrColClauses),
  num_of_diagonal_clauses(N, NumDiagonalClauses),
  FloatNumClauses is (2 * NumRowOrColClauses) + NumDiagonalClauses,
  fix(FloatNumClauses, NumClauses).

num_of_row_or_col_clauses(N, NumClauses) :-
  NMinusOne is N - 1,
  sum_to_n(NMinusOne, SumToNMinus1),
  NumClauses is SumToNMinus1 * N.

num_of_diagonal_clauses(N, NumClauses) :-
  num_of_diagonal_clauses_in_one_dir(N, NumClausesOneDir),
  NumClauses is NumClausesOneDir * 2.

num_of_aux_diagonal_clauses(N, 0) :- N < 3, !.
num_of_aux_diagonal_clauses(N, Res) :- 
  NMinusOne is N - 1, NMinusTwo is N - 2,
  num_of_aux_diagonal_clauses(NMinusOne, Acc),
  sum_to_n(NMinusTwo, SumToNMinus1),
  Res is SumToNMinus1 + Acc.

num_of_diagonal_clauses_in_one_dir(N, NumClauses) :-
  NMinusOne is N - 1, sum_to_n(NMinusOne, SumToNMinus1),
  num_of_aux_diagonal_clauses(N, NumAuxClauses),
  NumClauses is SumToNMinus1 + (2 * NumAuxClauses).


% MAIN EXECUTION PROCEDURES
nqueensFile(N) :-
  create_open_DIMACS_file(N, FileStream),
  row_clauses(N, FileStream), % Write at least one in each row and at most one in each row constraints
  col_clauses(N, FileStream), % Write at least one in each col and at most one in each col constraints
  diagonal_clauses(N, FileStream), % Write at most one in each diagonal constraints
  close(FileStream).


nqueensFindAllSolutions(N) :-
  nqueensFile(N), term_string(N, StrN), 
  join_string(["nqueens", StrN, ".cnf"], "", CNFFileName),
  join_string(["nqueens", StrN, ".out"], "", OutputFile),
  join_string(["all_sol_", StrN, ".out"], "", AllSolutionsFile),
  generate_next_answer(CNFFileName, OutputFile, AllSolutionsFile).

% Reads the CNF File, Generates an output, appends the negation of the output to the
% CNF file and keeps doing this until UNSAT. 
% NOTE: There is a mutual recursion between generate_next_answer and handle_multiple_answers
generate_next_answer(CNFFileName, OutputFile, AllSolutionsFile) :-
  join_string(["minisat", CNFFileName, OutputFile], " ", MiniSatShellCommand),
  sh(MiniSatShellCommand),
  read_output_file(OutputFile, IsSat, Answer),
  handle_multiple_answers(CNFFileName, OutputFile, AllSolutionsFile, IsSat, Answer).

handle_multiple_answers(_, _, _, "UNSAT", _). % Terminate mutual recursion
handle_multiple_answers(CNFFileName, OutputFile, AllSolutionsFile, "SAT", Answer) :-
  % Write down the solution
  open(AllSolutionsFile, append, FileStream),
  write_list_to_file(FileStream, Answer),
  close(FileStream),

  % Append negated clause to input file
  negate(Answer, NegatedAnswer),
  open(CNFFileName, append, InputFileStream),
  write_list_to_file(InputFileStream, NegatedAnswer),
  close(InputFileStream),
  
  % Generate the next solution (mutual recursion)
  generate_next_answer(CNFFileName, OutputFile, AllSolutionsFile).


% Takes a list and writes to file
write_list_to_file(FileStream, []) :- write(FileStream, "\n").
write_list_to_file(FileStream, [Head|Rest]) :- 
  term_string(Head, StrHead),
  concat_strings(StrHead, " ", HeadWithSpace),
  write(FileStream, HeadWithSpace),
  write_list_to_file(FileStream, Rest).

% Negates every element in the List.
negate([], []).
negate([Head|Lst], [NegHead|NegatedList]) :- 
  NegHead is -1 * Head,
  negate(Lst, NegatedList).


% Read minisat output files. IsSat is either "SAT" or "UNSAT". Answer is the integer list
% which represents the assignments generated by the minisat
read_output_file(FileName, IsSat, []) :-
  open(FileName, read, ReadFileStream),
  read_string(ReadFileStream, "\n", _, IsSat),
  IsSat == "UNSAT", !,
  close(ReadFileStream).

read_output_file(FileName, IsSat, Answer) :-
  open(FileName, read, ReadFileStream),
  read_string(ReadFileStream, "\n", _, IsSat),
  IsSat == "SAT", !,
  read_string(ReadFileStream, "\n", _, StrAnswerList),
  split_string(StrAnswerList, " ", "", StrList),
  maplist(number_string, Answer, StrList),
  write(StrList),
  close(ReadFileStream).

% MAIN CONSTRAINT GENERATION SECTION

% Let's say this is your board

% 13 14 15 16
%  9 10 11 12
%  5  6  7  8
%  1  2  3  4

% Row Clauses - 
% (1 or 2 or 3 or 4) AND (5 or 6 or 7 or 8) AND (9 or 10 or 11 or 12) AND (13 or 14 or 15 or 16)

% Also, we also need to make sure that there is at most one queen per row. So we add -
% (-1 or -2) AND (-1 or -3) AND (-1 or -4) AND (-2 or -3) AND (-2 or -4) AND (-3 or -4) AND

% We do this for every row



% Col Clauses - 
% Same as row clauses basically



% Diagonal Clauses - 
% For every diagonal like (1, 6, 11, 16) or (2, 7, 12) or (9, 6, 3),
% we generate only the clauses we need to ensure at most one queen. 
% Similar to rows and columns, we go ahead and find all pairs in the diagonals and negate them.
% every negated pair is a clause


% Adds at least one queen in a row and at most one queen in a row.
row_clauses(N, FileStream) :- row_clause_helper(N, 1, FileStream).
row_clause_helper(N, Start, _) :- Start > N * N, !.
row_clause_helper(N, Start, FileStream) :-
  NewStart is Start + N,
  RowEnd is Start + N - 1,
  range(Start, RowEnd, 1, Row),
  write_tiles_and_other_constraints(Row, FileStream),
  row_clause_helper(N, NewStart, FileStream).

% Adds at least one queen in a col and at most one queen in a col.
col_clauses(N, FileStream) :- col_clause_helper(N, 1, FileStream).
col_clause_helper(N, Start, _) :- Start > N, !.
col_clause_helper(N, Start, FileStream) :-
  NewStart is Start + 1,
  ColEnd is Start + ((N - 1) * N),
  range(Start, ColEnd, N, Col),
  write_tiles_and_other_constraints(Col, FileStream),
  col_clause_helper(N, NewStart, FileStream).


add_offset_to_every_element([], [], _).
add_offset_to_every_element([H1|Lst], [H2|NewList], Offset) :-
  H2 is H1 + Offset, add_offset_to_every_element(Lst, NewList, Offset).

without_last([], []).
without_last([_], []) :- !.
without_last([X|Xs], [X|WithoutLast]) :- 
    without_last(Xs, WithoutLast).

diagonal_clauses(1, [[1]]) :- !.
diagonal_clauses(N, FileStream) :-
  diagonal_right_clauses(N, FileStream),
  diagonal_left_clauses(N, FileStream).

diagonal_right_clauses(N, FileStream) :-
  LastTile is N * N, Offset is N + 1,
  range(1, LastTile, Offset, MainDiagonal),
  write_tiles_constraints(MainDiagonal, FileStream),
  diagonal_helper(MainDiagonal, FileStream, 1),
  diagonal_helper(MainDiagonal, FileStream, -1).

diagonal_left_clauses(N, FileStream) :-
  LastTile is N * (N - 1) + 1, Offset is N - 1, Start = N,
  range(Start, LastTile, Offset, MainDiagonal),
  reverse(MainDiagonal, LeftMainDiagonal),
  write_tiles_constraints(LeftMainDiagonal, FileStream),
  diagonal_helper(LeftMainDiagonal, FileStream, 1),
  diagonal_helper(LeftMainDiagonal, FileStream, -1).

diagonal_helper([_, _], _, _) :- !.
diagonal_helper(Diagonal, FileStream, Offset) :-
  Offset > 0,
  add_offset_to_every_element(Diagonal, NewList, Offset),
  without_last(NewList, NextDiagonal), !,
  write_tiles_constraints(NextDiagonal, FileStream),
  diagonal_helper(NextDiagonal, FileStream, Offset).

diagonal_helper(Diagonal, FileStream, Offset) :-
  add_offset_to_every_element(Diagonal, NewList, Offset),
  NewList = [_|NextDiagonal],
  write_tiles_constraints(NextDiagonal, FileStream),
  diagonal_helper(NextDiagonal, FileStream, Offset).


% FILE I/O

create_open_DIMACS_file(N, FileStream) :-
  term_string(N, StrN), join_string(["nqueens", StrN, ".cnf"], "", File),
  open(File, append, FileStream),
  writeln(FileStream, "c Generated by nqueensDIMACS.pl"),
  number_of_clauses(N, NumClauses), 
  print(NumClauses),
  term_string(NumClauses, StrNumClauses),
  join_string(["p", "cnf", StrN, StrNumClauses], " ", OpeningLine),
  writeln(FileStream, OpeningLine).


write_tiles_and_other_constraints([Tile|OtherTiles], FileStream) :-
  write_clause_to_file(FileStream, [Tile|OtherTiles]),
  write_tiles_constraints([Tile|OtherTiles], FileStream).

write_tiles_constraints([], _).
write_tiles_constraints([Tile|OtherTiles], FileStream) :-
  get_negated_tile_pairs(Tile, OtherTiles, Pairs),
  write_clauses_to_file(FileStream, Pairs),
  write_tiles_constraints(OtherTiles, FileStream).

write_clauses_to_file(_, []). 
write_clauses_to_file(FileStream, [Clause|Clauses]) :- 
  write_clause_to_file(FileStream, Clause),
  write_clauses_to_file(FileStream, Clauses).

write_clause_to_file(FileStream, Clause) :-
  maplist(term_string, Clause, StrAtoms),
  append(StrAtoms, ["0"], StrAtomsWithLineDelimiter),
  join_string(StrAtomsWithLineDelimiter, " ", StrClause),
  writeln(FileStream, StrClause).

get_negated_tile_pairs(_, [], []).
get_negated_tile_pairs(Tile, [AnotherTile|OtherTiles], Pairs) :-
  NegatedTileA is -1 * Tile, NegatedTileB is -1 * AnotherTile,
  get_negated_tile_pairs(Tile, OtherTiles, OtherPairs),
  Pairs = [[NegatedTileA, NegatedTileB]|OtherPairs].
