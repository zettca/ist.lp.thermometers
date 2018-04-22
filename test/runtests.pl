:- [puzzles].
:- [thermometers].

runtests :- !, puzzle(P, Puzz), solucao(P, Sol), resolve(Puzz, Sol), format("Test ~w", [P]).
