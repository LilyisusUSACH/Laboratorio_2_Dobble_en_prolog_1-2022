/*
██       █████  ██████      ██████         ██████  ██████   ██████  ██       ██████   ██████  
██      ██   ██ ██   ██          ██ ██     ██   ██ ██   ██ ██    ██ ██      ██    ██ ██       
██      ███████ ██████       █████         ██████  ██████  ██    ██ ██      ██    ██ ██   ███ 
██      ██   ██ ██   ██     ██      ██     ██      ██   ██ ██    ██ ██      ██    ██ ██    ██ 
███████ ██   ██ ██████      ███████        ██      ██   ██  ██████  ███████  ██████   ██████  

Nombre: Alejandro Cortés
Profesor: Roberto Gonzalez Ibañez
Curso: 13310-0
Seccion: A-1
*/
% A pesar de usar referencias a otros Tda, como podria ser
% Un tda Card, Cards, Player, Area, etc... no es necesaria ni util
% Su implementacion en prolog.

/* 
 █████  ██    ██ ██   ██ ██ ██      ██  █████  ██████  ███████ ███████ 
██   ██ ██    ██  ██ ██  ██ ██      ██ ██   ██ ██   ██ ██      ██      
███████ ██    ██   ███   ██ ██      ██ ███████ ██████  █████   ███████ 
██   ██ ██    ██  ██ ██  ██ ██      ██ ██   ██ ██   ██ ██           ██ 
██   ██  ██████  ██   ██ ██ ███████ ██ ██   ██ ██   ██ ███████ ███████ 
*/
% Son usados en distintos Predicados y distintos TDAs
% Dominios
    % L, L1,L2,L3, Cs : Listas
    % E : Cualquier tipo de dato / elemento, ~a type
    % Pos, Count : Enteros, Z+ +{0}
    % Str : String

% Predicados
    % addListToList(L1,L2,L3).
    % estaen(E,L,Count).
    % posicionE(Pos,E,L).
    % listToString(L,Str).
    % cardsToString(Cs,Str)
    % largo(L, Count)
% Metas
    % Secundarias
        % addListToList(L1,L2,L3). -> añade una lista de listas a una lista de listas.
        % estaen(E,L,Count). -> Busca si un elemento esta en una lista y cuantas veces esta
        % posicionE(Pos,E,L). -> retorna el Elemento que esta en una Posicion, o tambien 
                                % la Posicion de un Elemento
        % listToString(L,Str). -> Convierte una lista en un string, separado por un -
        % cardsToString(Cs,Str) -> Convierte una lista de listas en string.
        % largo(L, Count) -> Devuelve en el segundo argumento el largo de una lista 
        
    % Primarias

% Clausulas

    % Hechos
addListToList([],L2,L2).

    % Reglas    
addListToList([X|L1],L2,[X|L3]):- addListToList(L1,L2,L3).

estaen(_,[],0):-!.
estaen(X,[X|Xs],C):-estaen(X,Xs,Count),C is Count + 1,!.
estaen(X,[_|Xs],C):-estaen(X,Xs,C).

posicionE(1,X,[X|_]).
posicionE(N,E,[_|L]):-posicionE(N2,E,L),N is N2 + 1.

listToString(C,Str):-atomics_to_string(C, ' - ',Str2),string_concat(Str2,"\n",Str).

cardsToString([],""):-!.
cardsToString([X|Cs],Str):- listToString(X,Res),cardsToString(Cs,Str2),string_concat(Res, Str2, Str).

largo([],0):-!.
largo([_|L],R):-largo(L,R2), R is R2+1.

/* 
████████ ██████   █████       ██████  █████  ██████  ██████  ███████     ███████ ███████ ████████ 
   ██    ██   ██ ██   ██     ██      ██   ██ ██   ██ ██   ██ ██          ██      ██         ██    
   ██    ██   ██ ███████     ██      ███████ ██████  ██   ██ ███████     ███████ █████      ██    
   ██    ██   ██ ██   ██     ██      ██   ██ ██   ██ ██   ██      ██          ██ ██         ██    
   ██    ██████  ██   ██      ██████ ██   ██ ██   ██ ██████  ███████     ███████ ███████    ██   
*/

%%%%%%%%%%%%%
%% Generador de cartas
%%%%%%%%%%%%%
% Dominios
    % N,I,J,K : Numeros/Interger, Z+ : Siempre positivos.
    % C : Listas de enteros.
    % Cs : Listas de Listas
% Predicados
    % firstCard(N,C).
    % eachNcard(N,J,K,C).
    % nCards(N,J,Cs).
    % eachNNcard(N,I,J,K,C).
    % eachJcycle(N,I,J,Cs).
    % nnCards(N,I,Cs).
    % cardSet(N,Cs).
    
% Metas
    % Secundarias
        % firstCard(N,C). -> crea la primera carta del mazo.
        % eachNcard(N,J,K,C). -> crea cada una de las cartas "n".
        % nCards(N,J,Cs). -> llama al eachNcards con distintos parametros y lo unifica.
        % eachNNcard(N,I,J,K,C). -> crea cada una de las cartas "nn".
        % eachJcycle(N,I,J,Cs). -> llama a eachNNcard por cada cambio de j y lo unifica.
        % nnCards(N,I,Cs). llama a eachJcycle modificando el i en cada llamda, y lo unfica.

    % Primarias
        % cardSet(N,Cs). -> Crea un set de cartas dobble valido, y completo,
        %               ->  llamando a las funciones anteriores y unificandolas.

% Clausulas

    % Hechos

    % Reglas

firstCard(0,[]):-!.
firstCard(N,[N|C]):-N2 is N-1,firstCard(N2,C).

eachNcard(N,_,N2,[1]):-N2 is N+1,!.
eachNcard(N,J,K,[X|Rc]):- X is N * J + (K + 1), K2 is K + 1,
                            eachNcard(N,J,K2,Rc).

nCards(N,N2,[]):-N2 is N+1,!.
nCards(N,J,[Rc|Cs]):-eachNcard(N,J,1,Rc),J2 is J+1,nCards(N,J2,Cs).

eachNNcard(N,I,_,N2,[I2]):-N2 is N+1,I2 is I+1,!.
eachNNcard(N,I,J,K,[X|Rc]):-X is (N+2+N*(K-1)+(((I-1)*(K-1)+J-1) mod N)),K2 is K+1,
                            eachNNcard(N,I,J,K2,Rc).

eachJcycle(N,_,N2,[]):-N2 is N+1,!.
eachJcycle(N,I,J,[Rc|Rcs]):-eachNNcard(N,I,J,1,Rc),J2 is J+1,
                        eachJcycle(N,I,J2,Rcs).

nnCards(N,N2,[]):-N2 is N+1,!.
nnCards(N,I,NcsR):- eachJcycle(N,I,1,Rcs), addListToList(Rcs,Ncs,NcsR),
                I2 is I+1, nnCards(N,I2,Ncs).

cardSet(N,Rf):-N2 is N+1,firstCard(N2,Rc1) , nCards(N,1,Rcs1),nnCards(N,1,Rcs2),
            addListToList([Rc1|Rcs1],Rcs2,Rf).

%%%%%%%%%%%%%
%% Verificador de si es dobble
%%%%%%%%%%%%%

% Dominios

% Predicados
    % serepite(E,Cs).
    % repite(Cs). -> aridad 1
    % repite(Cs,Cs). -> aridad 2
    % repiteCartas(Cs). -> aridad 1
    % repiteCartas(Cs,Cs). -> aridad 2
    % unaVez(E,Cs,Count).
    % elementosComun(Cs1,Cs2,Count).
    % comunCardACards(C,Cs).
    % comunCards(Cs, Cs).
    % cardsSetIsDobble(Cs).

% Metas
    % Secundarias
        % serepite(E,Cs). ->  Verifica si un elemento se repite dentro de una lista
        % repite(Cs). -> aridad 1 -> se llama a si misma con aridad 2, pasando el argumento 2 veces.
        % repite(Cs,Cs). -> aridad 2 -> es true cuando cualquier elemento de una lista, esta repetido en otra lista
        % repiteCartas(Cs). -> aridad 1 -> llama a repiteCartas con aridad 2, pasando el argumento 2 veces.
        % repiteCartas(Cs,Cs). -> aridad 2 -> es true cuando alguna lista dentro de las listas de listas que recibe
                                % tiene algun elemento repetido dentro de la misma lista
        % unaVez(E,Cs,Count). -> Si recibe un E y un Count con un numero, retorna true solo si E esta Count veces en el Cs
        % elementosComun(Cs1,Cs2,Count). -> El Count corresponde a la cantidad de elementos en comun que tienen 2 listas
        % comunCardACards(C,Cs). -> Verifica que una lista tenga un solo elemento en comun con cada lista de una lista de listas
        % comunCards(Cs, Cs). -> Verifica que cada elemento del Cs cumpla con el comunCardACards, osea, cada lista de una lista de listas
                                %  tiene un solo elemento en comun con todas las otras listas de la lista de listas.
    % Primarias
        % cardsSetIsDobble(Cs). -> recibe un cardsSet y verifica si este es un cardsSet valido y ademas, cumpla con
                                % las reglas del juego dobble.
% Clausulas

    % Hechos

    % Reglas

% N elementos diferentes en cada carta
serepite(X,Cs):-estaen(X,Cs,Count),Count>1.

repite(X):-repite(X,X).
repite([X|Xs],Cs):-serepite(X,Cs),!;repite(Xs,Cs).

repiteCartas(X):- repiteCartas(X,X).
repiteCartas([X|Xs],Cs):-repite(X),!; repiteCartas(Xs,Cs). 
% si hay alguna carta con elementos repetidos

unaVez(X,Cs,Count):-estaen(X,Cs,Count),!.
% par de carta existe un solo elemento en comun
elementosComun([],_,0):-!.
elementosComun([X|L],Cs,C):-unaVez(X,Cs,C2),elementosComun(L,Cs,C3),C is C2+C3.

comunCardACards(_,[]):-!.
comunCardACards(X,[C|Cs]):-elementosComun(X,C,1),comunCardACards(X,Cs).

comunCards([],_):-!.
comunCards([X|L],Cs):-comunCardACards(X,L),comunCards(L,Cs).

cardsSetIsDobble([_|Cs]):- not(repiteCartas(Cs)),comunCards(Cs,Cs).

%%%%%%%%%%%%%
%% TDA cardsSet 
%%%%%%%%%%%%%

/*      Representacion:
Null | [Lista Elementos] X Listas (Cartas)
*/

% Dominios
    % E : Elemento sin importar el tipo, ~a-type
    % C, C1, C2 : Lista (representacion de una carta)
    % Cs, Cs2, CAC : Lista de listas (representacion de varias cartas)
    % X, X1, Seed : Enteros, Z+ + {0}
    % L, L1,L2 : Lista
    % Str : String
% Predicados
    % addFinal(E,L1,L2).
    % myrandom(X,X1).
    % randomizar(Cs, X, Cs2).
    % cortar(Cs, X, Cs2).
    % linkearC(L, C, C2).
    % linkearA(L, Cs, Cs2).
    % cardsSet(L,E,C,Seed,CardsSet).

    % cardsSetToString(CardsSet, Str).
    % cardsSetNthCard(CardsSet, X, C).
    % cardsSetFindTotalCards(CardsSet, X).

    % isCardEqualCard(C1,C2).
    % isCardInCards(C1,Cs).
    % notInterseccion(Cs, CAC, Cs2).
    % cardsSetMissingCards(Cs, Cs2).

% Metas
    % Secundarias
        % addFinal(E,L1,L2). -> Añade un Elemento en una lista
        % myrandom(X,X1). -> Genera un numero "aleatorio" a partir de otro
        % randomizar(Cs, X, Cs2). -> Segun un numero X, "revuelve" aleatoriamente
                                    % una lista de listas
        % cortar(Cs, X, Cs2). -> Deja solamente una X cantidad de Listas, eliminando el resto
        % linkearC(L, C, C2). -> Asocia una lista de elementos con una lista de numeros, basandose en la posicion
                            % de cada elemento.
        % linkearA(L, Cs, Cs2). -> Aplica lo anterior para cada lista de una lista de listas
        % isCardEqualCard(C1,C2). -> Compara si una lista tiene exactamente los mismos elementos que otra lista
        % isCardInCards(C1,Cs). -> Revisa si una lista esta en una lista de listas, usando el predicado anterior
                                % Para hacer la comparacion
        % notInterseccion(Cs, CAC, Cs2). -> Crea una lista de listas, donde sus elementos son todos los que estan en
                                % Cs, pero no estan en CAC.
      
    % Primarias
        % cardsSet(L,E,C,Seed,CardsSet). -> Crea un CardsSet valido, con E elementos en cada carta, con una cantidad C
                                    % de cartas, revolviendolas segun una Seed, y donde sus elementos seran los entregados
                                    % en L.
        % cardsSetToString(CardsSet, Str). -> Crea un string de un CardsSet
        % cardsSetNthCard(CardsSet, X, C). -> Devuelve la Carta en una posicion X de un cardsSet
        % cardsSetFindTotalCards(C, X). -> A partir de una Carta, calcula cuanto seria el maximo de cartas validas para dobble
        % cardsSetMissingCards(Cs, Cs2). -> Recibe un cardsSet valido pero incompleto, generando asi, un CardsSet alternativo
                                        % El cual si debe estar completo, y despues en Cs2 unifica todas las cartas que
                                        % faltan para que el Cs sea un CardsSet completo

% Clausulas

    % Hechos

    % Reglas

addFinal(X,[],[X]):-!.
addFinal(X,[Y|L],[Y|Res]):-addFinal(X,L,Res).

myrandom(Xn,Xn1):- AX is 1103515245 * Xn, AXC is AX + 12345,Xn1 is (AXC mod 2147483647).
randomizar([],_,[]):-!.
randomizar([X|L],Seed,Lf):-Rs is (Seed mod 2),Rs is 0,Seed3 is Seed + 1,myrandom(Seed3,Seed2),randomizar(L,Seed2,R),addFinal(X,R,Lf),!.
randomizar([X|L],Seed,[X|Lf]):-Seed3 is Seed + 1,myrandom(Seed3,Seed2),randomizar(L,Seed2,Lf),!.

cortar(L,N,L):- var(N),!;N<0,!.
cortar(_,0,[]):-!.
cortar([X|L],N,[X|L2]):- N2 is N-1, cortar(L,N2,L2).

%linkear(Ls,Lc,Lf):-
linkearC(_,[],[]):-!.
linkearC(Ls,[Y|Lc],Lf2):-posicionE(Y,S,Ls),linkearC(Ls,Lc,Lf),addFinal(S,Lf,Lf2),!.

linkearA(_,[],[]):-!.
linkearA(Ls,[Y|Lcs],Lf2):-linkearC(Ls,Y,R),linkearA(Ls,Lcs,Lf),addFinal(R,Lf,Lf2).

% Seed recomendada 13213
cardsSet([],E,C,Seed,[[]|Cs]):- N is E-1, cardSet(N,Cs1),randomizar(Cs1,Seed,Cs2),cortar(Cs2,C,Cs),!. 
cardsSet(L,E,C,Seed,[L|Csf]):- N is E-1, cardSet(N,Cs1),randomizar(Cs1,Seed,Cs2),cortar(Cs2,C,Cs),linkearA(L,Cs,Csf). 

% cardsSetToString
cardsSetToString([_|Cs],Str):-cardsToString(Cs,Str).

% cardsetNthCard
cardsSetNthCard([_|Cs],N,C):-N2 is N+1,posicionE(N2,C,Cs),!.

% cardsSetFindTotalCards
cardsSetFindTotalCards(C,TC):-largo(C,R), R2 is R-1, TC is R2*R2+R2+1.

isCardEqualCard([],[]):-!.
isCardEqualCard([X|C1],C2):-estaen(X,C2,1),subtract(C2,[X],C3),isCardEqualCard(C1,C3).

isCardInCards(C1,[C2|Cs]):-isCardEqualCard(C1,C2),!;isCardInCards(C1,Cs).

notInterseccion([],_,[]).
notInterseccion([C|Cs],CAC,Csf):-isCardInCards(C,CAC),!,notInterseccion(Cs,CAC,Csf).
notInterseccion([C|Cs],CAC,[C|Csf]):-notInterseccion(Cs,CAC,Csf).

% Missing cards (CS, CS2) , CAC = Cards A comparar
% Debe tener al menos una carta
cardsSetMissingCards([X,Fc|Cs],[X|Csf]):- largo(Fc,Largo),cardsSet(X,Largo,-1,13213,[_|CAC]),notInterseccion(CAC,[Fc|Cs],Csf),!.

/* 
████████ ██████   █████       ██████   █████  ███    ███ ███████ 
   ██    ██   ██ ██   ██     ██       ██   ██ ████  ████ ██      
   ██    ██   ██ ███████     ██   ███ ███████ ██ ████ ██ █████   
   ██    ██   ██ ██   ██     ██    ██ ██   ██ ██  ██  ██ ██      
   ██    ██████  ██   ██      ██████  ██   ██ ██      ██ ███████ 
*/

%%%%%%%%%%%%%
%% TDA Game
%%%%%%%%%%%%%
/*      Representacion: 
un Game es [ Area , cardsSet , numero_jugadores, turno , jugadores, estado, modo, Seed]
    [Lista de Listas, Lista de Listas, Entero, Entero, Lista de listas , String , String, Entero]
*/
% Dominios

% Predicados
    % dobbleGame(NP, CardsSet, Mode, Seed, Game).
    % getPlayers(Game, Pls).
    % setPlayers(Pls, Game, NewGame).
    % getArea(Game, Area).
    % setArea(Area,Game, NewGame).
    % getMode(Game, Mode).
    % getCards(Gane, Cs).
    % setCards(Cs, Game, NewGame).
    % dobbleGameStatus(Game, Str).
    % setStatus(Str, Game, NewGame).
    % isPlayerInPlayers(Name, Pls).
    % dobbleGameRegister(Name, Game, NewGame).
    % dobbleGameWhoseTurnIsIt(Game,Name).
    % scorePlayer(Name,Pls,Score).
    % dobbleGameScore(Game,User,Score).
    % dobbleGameToString(G,Str).

% Metas
    % Secundarias
        % getPlayers(Game, Pls). -> obtiene una lista de "Players" (Tda no implementado)
        % setPlayers(Pls, Game, NewGame). -> Cambia la lista de Players de un Game
        % getArea(Game, Area). -> obtiene el "Area" (es una lista de listas) de un Game
        % setArea(Area,Game, NewGame).  -> Cambia el Area de un Game
        % getMode(Game, Mode). -> Obtiene el string que representa el modo de un Game
        % getCards(Gane, Cs). -> Obtiene las cartas que estan en el "stack" de un Game
        % setCards(Cs, Game, NewGame). -> Modifica las cartas del mazo
        % setStatus(Str, Game, NewGame). -> Cambia el estado del juego
        % isPlayerInPlayers(Name, Pls). -> Verifica segun el Name si un Player esta en una lista de Players
        % scorePlayer(Name,Pls,Score). -> Muestra el Score de un Player segun su Name

    % Primarias
        % dobbleGame(NP, CardsSet, Mode, Seed, Game). -> Crea un Game inicial.
        % dobbleGameRegister(Name, Game, Game2). -> Registra un nuevo Player en el Game
        % dobbleGameWhoseTurnIsIt(Game,Name). -> Dice de quien es el turno (No aplica para el modo de juego implementado)
        % dobbleGameStatus(Game, Str). -> Muestra cual es el estado del Game actual
        % dobbleGameScore(Game,Name,Score). -> Muestra el Score de un Player segun su nombre.
        % dobbleGameToString(G,Str). -> Genera un string para visualizar el Game.

% Clausulas
        
    % Hechos

    % Reglas

dobbleGame(Np, [_|Cs], Mode, Seed, Game):-Np > 0, cardsSetIsDobble(Cs), Seed > 0,
            Game = [[], Cs, Np, 0, [],"No empezado", Mode, Seed].

getPlayers([_,_,_,_,X|_],X).
setPlayers(NewPlayer,[A,B,C,D,_|Rest],[A,B,C,D,NewPlayer|Rest]):- largo(NewPlayer,L) ,L < C+1.

getArea([A|_],A).
setArea(A,[_|Resto],[A|Resto]).

getMode([_,_,_,_,_,_,X|_],X).

getCards([_,Cs|_],Cs).
setCards(Cs,[A,_|Resto],[A,Cs|Resto]).

dobbleGameStatus([_,_,_,_,_,E|_],E).

setStatus(S,[A,B,C,D,E,_|Resto],[A,B,C,D,E,S|Resto]).

isPlayerInPlayers(Name,[[Name|_]|_]):-!.
isPlayerInPlayers(Name,[_|Pls]):-isPlayerInPlayers(Name,Pls).

dobbleGameRegister(Name,G1,GF):-getPlayers(G1,Pls),largo(Pls,_),not(isPlayerInPlayers(Name,Pls)),setPlayers([[Name,0,[]]|Pls], G1, GF).

dobbleGameWhoseTurnIsIt([_,_,_,X,Pls|_],P):- largo(Pls,Largo),I is X mod Largo, I2 is I+1, posicionE(I2,[P|_],Pls),!.

scorePlayer(Name,[[Name,Score|_]|_],Score):-!.
scorePlayer(Name,[_|Pls],Score):-scorePlayer(Name,Pls,Score).
dobbleGameScore(G,User,Score):-getPlayers(G,Pls),scorePlayer(User,Pls,Score).

dobbleGameToString(G,Str):- dobbleGameStatus(G,Sta),string_concat("Estado de juego: \n", Sta,Str1) ,getArea(G,Area), cardsToString(Area,AreaStr), string_concat("\nArea de juego: \n", AreaStr,Str2),
                            string_concat(Str1, Str2,Str).

%%%%%%%%%%%%%%%%%%%%%
%% Modo de juego implementado - StackMode
%%%%%%%%%%%%%%%%%%%%%

% Dominios
    % Los mismos que estan detallados anteriormente
    % action: lista de 3 elementos o lista nula.

% Predicados
    % isElementInAllArea(E,Area).
    % modifyPlayerForSM(Name,Area,Pls,Pls2).
    % stackMode(Game, Action, NewGame).
    % dobbleGamePlay(Game, Action, NewGame).

% Metas
    % Secundarias
        % isElementInAllArea(E,Area). -> Verifica si un elemento dado esta en todas las
                                % listas del Area.
        % modifyPlayerForSM(Name,Area,Pls,Pls2). -> Al Pls le añade las cartas del Area a un jugador
                                % que coincida con su Name, y le suma 1 al Score, esto ya que esta
                                % pensado para este modo de juego, esto queda en el Pls2.
        % stackMode(Game, Action, NewGame). -> modo de juego, verifica que hacer segun cada Action.

    % Primarias
        % dobbleGamePlay(Game, Action, NewGame). -> Segun el modo de un Game, llama a ese modo con el Action
                                % correspondiente, es el principal predicado para jugar.

% Clausulas
        
    % Hechos

    % Reglas

isElementInAllArea(_,[]):-!.
isElementInAllArea(E,[X|R]):- estaen(E,X,1),isElementInAllArea(E,R).

modifyPlayerForSM(Name,NC,[[Name,Score,Cs]|Resto],[[Name,Score2,Cs2]|Resto]):-addListToList(NC,Cs,Cs2),Score2 is Score+1,!.
modifyPlayerForSM(Name,NC,[X|Pls],[X|Pls2]):-modifyPlayerForSM(Name,NC,Pls,Pls2).

stackMode(G,_,G):- dobbleGameStatus(G,S), S == "Finalizado".

stackMode([A,Cs|Resto],[],Gf):- largo(Cs,L), L < 2, setStatus("Finalizado",[A,Cs|Resto],Gf), A == [],!.
stackMode(G,[],Gf):-getArea(G,Area), Area == [] ,setStatus("Jugando",G,G1),getCards(G1,[C1,C2|Resto]),setArea([C1,C2],G1,G2),setCards(Resto,G2,Gf),!. %saco dos cartas del mazo y las pongo en el area
stackMode([A,Cs|Resto],[],[A,Cs|Resto]):-!.

stackMode(G,[X,U,E],Gf):- X == spotit,getArea(G,Area),isElementInAllArea(E,Area),getPlayers(G,Pls),modifyPlayerForSM(U,Area,Pls,Pls2),setArea([],G,G2), setPlayers(Pls2,G2,Gf),!.
stackMode(G,[X,_,E],G2):- X == spotit,getArea(G,Area),not(isElementInAllArea(E,Area)),stackMode(G,[pass],G2),!. % Si no acierta se devuelven al mazo

stackMode([Area,Cs,Np,Turn|Rest],[X|_],[[],Ncs,Np,Turn2|Rest]):- X == pass,addListToList(Cs,Area,Ncs), Turn2 is Turn + 1. % NO TIENE LOGICA IMPLEMENTARLO, PERO LO HARE PARA COMPROBAR EL dobbleGameWhoseTurnIsIt
stackMode(G,[X|_],Gf):- X == finish, setStatus("Finalizado",G,Gf).

dobbleGamePlay(G,Ac,Gf):-getMode(G,Mode), Mode = "stackMode",stackMode(G,Ac,Gf),!. % Que hacer cuando es stackMode

/* 
███████      ██ ███████ ███    ███ ██████  ██       ██████  ███████ 
██           ██ ██      ████  ████ ██   ██ ██      ██    ██ ██      
█████        ██ █████   ██ ████ ██ ██████  ██      ██    ██ ███████ 
██      ██   ██ ██      ██  ██  ██ ██      ██      ██    ██      ██ 
███████  █████  ███████ ██      ██ ██      ███████  ██████  ███████ 
*/


% Ejemplo juego
%dobbleGame(4,[[],[a,b,c],[b,d,e],[e,f,g]],"stackMode",131412,G),dobbleGameRegister("juan",G,G1),dobbleGameRegister("pedro",G1,G2),dobbleGamePlay(G2,[],G3),
%dobbleGameWhoseTurnIsIt(G3,Turn1),dobbleGamePlay(G3,[pass],G4),dobbleGameWhoseTurnIsIt(G4,Turn2),dobbleGamePlay(G4,[],G5),dobbleGamePlay(G5,[pass],G6),dobbleGameWhoseTurnIsIt(G6,Turn3).

% Otro ejemplo
%dobbleGame(4,[[],[a,b,c],[b,d,e],[e,f,g]],"stackMode",131412,G),dobbleGameRegister("juan",G,G1),dobbleGameRegister("pedro",G1,G2),dobbleGamePlay(G2,[],G3),dobbleGamePlay(G3,[pass],G4),
%dobbleGamePlay(G4,[],G5),dobbleGamePlay(G3,[spotit,"juan",a],G6),dobbleGamePlay(G3,[spotit,"juan",b],G7),dobbleGamePlay(G3,[finish],G8),dobbleGamePlay(G8,[spotit,"pedro",b],G9).