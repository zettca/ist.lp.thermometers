% ========== HELPERS ========== %

sameLine(I, (L,_)) :- I = L.
sameColumn(I, (_,C)) :- I = C.
possBeforeLine(L, (L2, _)) :- L > L2.
allLinePoss(Line, Dim, Poss) :-
	findall((Line, I), between(1, Dim, I), Poss).
propagaTerm(Term, Pos, Poss) :-
	nth1(I, Term, Pos),
	findall(E, (nth1(J, Term, E), J =< I), Poss).
verifyColumn(I, ColVals, Poss) :-
	include(sameColumn(I), Poss, PossColI),
	length(PossColI, LenCol),
	nth1(I, ColVals, ColVal),
	LenCol =< ColVal.
possLine([Terms,LineVals,ColVals], Line, Poss, Ja_Preenchidas) :-
	%format("POSSARGS: ~w ~w~n", [Line, Poss]),
	findall(Posi, (member(Pos, Poss), propaga([Terms,LineVals,ColVals], Pos, Posi)), ReqPoss1),
	flatten(ReqPoss1, ReqPoss2), sort(ReqPoss2, ReqPoss3),
	%format("ReqPoss3: ~w~n", [ReqPoss3]),
	include(sameLine(Line), ReqPoss3, PossLineI),
	union(Poss, PossLineI, Poss), % same line doesn't change
	%format("passed ~w~n", [ReqPoss3]),
	length(ColVals, Dim),
	verifica_parcial([Terms,LineVals,ColVals], Ja_Preenchidas, Dim, ReqPoss3).

subset2([], []).
subset2([E|Tail], [E|NTail]) :-
	subset2(Tail, NTail).
subset2([_|Tail], NTail) :-
	subset2(Tail, NTail).

% ========== PROJECT ========== %

propaga([Terms|_], Pos, PossSorted) :-
	member(Term, Terms), member(Pos, Term),	% find Term with position Pos
	propagaTerm(Term, Pos, Poss),						% find Poss from Term root to Pos
	sort(Poss, PossSorted).									% make sure result is sorted

nao_altera_linhas_anteriores(Poss, L, Ja_Preenchidas) :-
	include(possBeforeLine(L), Poss, PossLinhaAnterior),	% get Poss from Lines < L
	subtract(PossLinhaAnterior, Ja_Preenchidas, []).			% check if all Poss are already filled

verifica_parcial([_,_,ColVals], Ja_Preenchidas, Dim, Poss) :-
	union(Ja_Preenchidas, Poss, FillOpt),		% get the option
	foreach(between(1, Dim, I), verifyColumn(I, ColVals, FillOpt)). % verify for each column

possibilidades_linha(Puzz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L) :-
	findall(Poss, (
		subset2(Posicoes_linha, Poss), length(Poss, Total), % find a combination
		nth1(1, Posicoes_linha, (Line,_)),		% dumb enunciado
		possLine(Puzz, Line, Poss, Ja_Preenchidas)),				% that respects popaga
		Possib),
	sort(Possib, Possibilidades_L).

% Abordagem:
% foreach Line, find Poss
resolve([Terms,LineVals,ColVals], Sol) :-
	length(LineVals, NumLines),
	length(LineVals, NumCols),
	resolveAux([Terms,LineVals,ColVals], 1, NumLines, NumCols, [], Sol).

% Puzz, Linha, Linhas, Sol
resolveAux(_, Line, Lines, _, Sol, Sol) :- Line is Lines + 1.
resolveAux([Terms,LineVals,ColVals], Line, Lines, Cols, SolAux, Sol) :-
	Line =< Lines,
	%format("~n~nARGS: ~w ~w ~w ~w~n", [Line, Lines, SolAux, Sol]),
	% get possibilities
	allLinePoss(Line, Lines, AllLinePoss),
	nth1(Line, LineVals, LineVal),
	possibilidades_linha([Terms,LineVals,ColVals], AllLinePoss, LineVal, SolAux, PossLine),

	% pick a possiblity
	member(PossLine1, PossLine),
	% check if it's valid and good to continue
	nao_altera_linhas_anteriores(PossLine1, Line, SolAux),

	%format("Pos for Line ~w: ~w~n", [Line, PossLine1]),

	% check next line
	Line1 is Line+1,
	append(SolAux, PossLine1, SolAux1),
	resolveAux([Terms,LineVals,ColVals], Line1, Lines, Cols, SolAux1, Sol).
	
