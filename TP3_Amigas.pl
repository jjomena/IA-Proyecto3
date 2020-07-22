:- autoload(library(lists), [select/3, member/2]).

estructura(amigas,[amiga(_,_,secretaria),
                      amiga(_,_,actriz),
                      amiga(_,_,maestra)]).


resolver1(Acertijo, Structure) :-test_puzzle1(Acertijo,Puzzle),
         Puzzle=puzzle(Structure,_),
         resolver_puzzle(Puzzle),
         mostrar(Structure).

test_puzzle1(Name,puzzle(Structure,Clues)):-
   estructura(Name,Structure),
   pistas(Name,Structure,Clues).


resolver_puzzle(puzzle(Structure, Clues)):-
   solve(Clues).


solve([Clue|Clues]):-Clue,solve(Clues).
solve([]).

mostrar([]).
mostrar([C|Cs]) :- writeln(C),mostrar(Cs).

pistas(
      amigas,
      Amigas,

   [ pista1(Amigas),
     pista2(Amigas),
     pista3(Amigas),
     pista4(Amigas),
     pista5(Amigas)
   ]).

%---------------------------------------------------------------------------------------
% pista1: beatriz no es garcia
pista1(E):-nombre(beatriz,V1), select(V1,E,E2),
           apellido(garcia,V2), member(V2,E2).
%-------------------------------------------------------------------------------------------


% pista2: La señora de apellido López es secretaria en una oficina
pista2(E) :- apellido(lopez,V1),profesion(secretaria,V1),member(V1,E).
%-------------------------------------------------------------------------------------------


%------------------------------------------------------------------------------------------
% pista3: La actriz se llama claudia
pista3(E) :- nombre(claudia,V1),profesion(actriz,V1),member(V1,E).
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
% pista4: La maestra no es Mendez
pista4(E):-apellido(mendez,V1), select(V1,E,E2),
           profesion(maestra,V2), member(V2,E2).
%-------------------------------------------------------------------------------------------


%-------------------------------------------------------------------------------------------
% pista5: Alicia llegó antes que las demas amigas
pista5(E) :- nombre(alicia,V1),member(V1,E).
%-------------------------------------------------------------------------------------------

%----------------------------------------
nombre(N,amiga(N,_,_)).
apellido(A,amiga(_,A,_)).
profesion(P,amiga(_,_,P)).
%----------------------------------------
