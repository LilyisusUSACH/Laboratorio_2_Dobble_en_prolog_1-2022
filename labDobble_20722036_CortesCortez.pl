%%%%%%%%%%%%%
%% TDA CARD
%%%%%%%%%%%%%
% Dominios

% Predicados

% Metas
    % Secundarias

    % Primarias

% Clausulas

    % Hechos

    % Reglas

%%%%%%%%%%%%%
%% TDA CARDS
%%%%%%%%%%%%%

% Dominios

% Predicados

% Metas
    % Secundarias

    % Primarias

% Clausulas

    % Hechos

    % Reglas

%%%%%%%%%%%%%
%% Constructor set de cartas
%%%%%%%%%%%%%
% Dominios
    % N,I,J,K : Numeros/Interger, Z+ : Siempre positivos.
    % C : Listas de enteros.
    % L1,L2,L3,Cs : Listas de Listas
% Predicados
    %addListToList(L1,L2,L3).
    %firstCard(N,C).
    %eachNcard(N,J,K,C).
    %nCards(N,J,Cs).
    %eachNNcard(N,I,J,K,C).
    %eachJcycle(N,I,J,Cs).
    %nnCards(N,I,Cs).
    %cardSet(N,Cs).
    
% Metas
    % Secundarias
        %addListToList(L1,L2,L3). -> añade una lista de listas a una lista de listas.
        %firstCard(N,C). -> crea la primera carta del mazo.
        %eachNcard(N,J,K,C). -> crea cada una de las cartas "n".
        %nCards(N,J,Cs). -> llama al eachNcards con distintos parametros y lo unifica.
        %eachNNcard(N,I,J,K,C). -> crea cada una de las cartas "nn".
        %eachJcycle(N,I,J,Cs). -> llama a eachNNcard por cada cambio de j y lo unifica.
        %nnCards(N,I,Cs). llama a eachJcycle modificando el i en cada llamda, y lo unfica.

    % Primarias
        %cardSet(N,Cs). -> Crea un set de cartas dobble valido, y completo,
        %               ->  llamando a las funciones anteriores y unificandolas.

% Clausulas

    % Hechos
addListToList([],L2,L2).

    % Reglas
addListToList([X|L1],L2,[X|L3]):- addListToList(L1,L2,L3).

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

% Metas
    % Secundarias

    % Primarias

% Clausulas

    % Hechos

    % Reglas

% N elementos diferentes en cada carta
estaen(_,[],0):-!.
estaen(X,[X|Xs],C):-estaen(X,Xs,Count),C is Count + 1,!.
estaen(X,[_|Xs],C):-estaen(X,Xs,C).

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

cardsSetIsDobble(Cs):- not(repiteCartas(Cs)),comunCards(Cs,Cs).

%%%%%%%%%%%%%
%% TDA CARDSET
%%%%%%%%%%%%%

% Dominios

% Predicados

% Metas
    % Secundarias

    % Primarias

% Clausulas

    % Hechos

    % Reglas

%cortar(Cs1,N,Seed,Cs).
addFinal(X,[],[X]):-!.
addFinal(X,[Y|L],[Y|Res]):-addFinal(X,L,Res).

myrandom(Xn,Xn1):- AX is 1103515245 * Xn, AXC is AX + 12345,Xn1 is (AXC mod 2147483647).
randomizar([],_,[]):-!.
randomizar([X|L],Seed,Lf):-Rs is (Seed mod 2),Rs is 0,Seed3 is Seed + 1,myrandom(Seed3,Seed2),randomizar(L,Seed2,R),addFinal(X,R,Lf),!.
randomizar([X|L],Seed,[X|Lf]):-Seed3 is Seed + 1,myrandom(Seed3,Seed2),randomizar(L,Seed2,Lf),!.

cortar(L,N,L):- var(N),!;N<0,!.
cortar(_,0,[]):-!.
cortar([X|L],N,[X|L2]):- N2 is N-1, cortar(L,N2,L2).

posicion(1,[X|_],X):-!. % Va desde 1 hasta n
posicion(N,[_|L],E):-N2 is N-1,posicion(N2,L,E).

posicionE(1,X,[X|L]).
posicionE(N,E,[X|L]):-posicionE(N2,E,L),N is N2 + 1.

%linkear(Ls,Lc,Lf):-
linkearC(_,[],[]):-!.
linkearC(Ls,[Y|Lc],Lf2):-posicionE(Y,S,Ls),linkearC(Ls,Lc,Lf),addFinal(S,Lf,Lf2),!.

linkearA(_,[],[]):-!.
linkearA(Ls,[Y|Lcs],Lf2):-linkearC(Ls,Y,R),linkearA(Ls,Lcs,Lf),addFinal(R,Lf,Lf2).
% Seed recomendada 13213
cardsSet(L,E,C,Seed,Csf):- N is E-1, cardSet(N,Cs1),randomizar(Cs1,Seed,Cs2),cortar(Cs2,C,Cs),linkearA(L,Cs,Csf). 

%%% cardsSetToString
listToString(C,Str):-atomics_to_string(C, ' - ',Str2),string_concat(Str2,"\n",Str).
cardsSetToString([],""):-!.
cardsSetToString([X|Cs],Str):- listToString(X,Res),cardsSetToString(Cs,Str2),string_concat(Res, Str2, Str).
%%%%%%%
modulo(X,R):-R is (X mod 2),(R is 0).

%%%%%%%%%%%%%
%% TDA Player
%%%%%%%%%%%%%

% Dominios

% Predicados

% Metas
    % Secundarias

    % Primarias

% Clausulas

    % Hechos

    % Reglas

%%%%%%%%%%%%%
%% TDA Players
%%%%%%%%%%%%%

% Dominios

% Predicados

% Metas
    % Secundarias

    % Primarias

% Clausulas

    % Hechos

    % Reglas

%%%%%%%%%%%%%
%% TDA GAME
%%%%%%%%%%%%%

% Dominios

% Predicados

% Metas
    % Secundarias

    % Primarias

% Clausulas

    % Hechos

    % Reglas
