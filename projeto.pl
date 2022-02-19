% 2.1 - extrai_ilhas_linha/3

/*
    extrai_ilhas_linha(N_L, Linha, Ilhas):

    Sendo N_L um inteiro positivo correspondente ao numero de uma linha de um
    puzzle e Linha uma lista que representa essa linha, Ilhas eh a lista
    contendo todas as ilhas dessa mesma linha.
*/
extrai_ilhas_linha(N_L, Linha, Ilhas):-
    findall(ilha(Pontes, (N_L, N_C)),
        (
            nth1(N_C, Linha, Pontes),
            Pontes > 0
        ),
        Ilhas).


% 2.2 - ilhas/2

/*
    ilhas(Puzzle, Ilhas):

    Sendo Puzzle um puzzle, Ilhas eh uma lista contendo as ilhas desse puzzle.
*/
ilhas(Puzzle, Ilhas):-
    findall(ilha(Pontes, (N_L, N_C)), 
        (   
            nth1(N_L, Puzzle, Linha),
            nth1(N_C, Linha, Pontes),
            Pontes > 0
        ),
        Ilhas).


% 2.3 - vizinhas/3

/*
    esta_entre(Limite1, Limite2, Valor):

    Com Limite1, Limite2 e Valor inteiros, eh true se Valor eh diferente
    de ambos os limites e esta entre eles.
*/
esta_entre(Lim1, Lim2, Valor):-
    (
        Lim2 < Lim1,
        N_Lim1 is Lim2 + 1,
        N_Lim2 is Lim1 - 1,
        between(N_Lim1, N_Lim2, Valor)
    );
    (
        Lim2 > Lim1,
        N_Lim1 is Lim1 + 1,
        N_Lim2 is Lim2 - 1,
        between(N_Lim1, N_Lim2, Valor)
    ).
    
/*
    vizinhas(Ilhas, Ilha, Vizinhas):

    Sendo Ilhas uma lista de ilhas de um puzzle e Ilha uma dessas ilhas,
    Vizinhas eh uma lista de ilhas pertencentes a Ilhas e diferentes de Ilha
    que sao vizinhas de Ilha, ou seja, estao na mesma linha ou na mesma coluna
    que Ilha sem que exista nenhuma outra ilha entre elas.
*/
vizinhas(Ilhas, ilha(P, (N_L, N_C)), Vizinhas):-
    findall(IlhaV,
        (
            member(IlhaV, Ilhas),
            IlhaV \== ilha(P, (N_L, N_C)),
            (
                (  % Ilhas vizinhas na mesma coluna:
                    IlhaV = ilha(_, (N_L2, N_C)),
                    \+ (member(ilha(_, (N_L3, N_C)), Ilhas), 
                        esta_entre(N_L, N_L2, N_L3))
                );
                (  % Ilhas vizinhas na mesma linha:
                    IlhaV = ilha(_, (N_L, N_C2)),
                    \+ (member(ilha(_, (N_L, N_C3)), Ilhas),
                        esta_entre(N_C, N_C2, N_C3)) % Define que nao existe..
                )  % ..nenhuma terceira ilha entre Ilha e IlhaV.
            )
        ),
        Vizinhas_Nao_Ord),
    sort(2, <, Vizinhas_Nao_Ord, Vizinhas). % Ordena ilhas pelas coordenadas.


% 2.4 - estado/2

/*
    estado(Ilhas, Estado):

    Sendo Ilhas uma lista contendo ilhas de um puzzle, Estado eh a lista
    ordenada contendo entradas referentes a cada ilha de Ilhas.
*/
estado(Ilhas, Estado):-
    findall([Ilha, Vizinhas, []],
        (
            member(Ilha, Ilhas),
            vizinhas(Ilhas, Ilha, Vizinhas)
        ),
        Estado).


% 2.5 - posicoes_entre/3

/*
    poicoes_entre(Pos1, Pos2, Posicoes):

    Sendo Pos1 e Pos2 posicoes, Posicoes eh uma lista contendo todas as
    posicoes do puzzle que se encontram entre Pos1 e Pos2. Eh false caso Pos1
    e Pos2 nao pertencam ah mesma linha ou coluna.
*/
posicoes_entre((N_L1, N_C1), (N_L2, N_C2), Posicoes):-
    bagof((N_L3, N_C3),
        (
            (
                N_L1 == N_L2,
                N_L3 = N_L1,
                esta_entre(N_C1, N_C2, N_C3)
            );
            (
                N_C1 == N_C2,
                N_C3 = N_C1,
                esta_entre(N_L1, N_L2, N_L3)
            )
        ),
        Posicoes).


% 2.6 - cria_ponte/3

/*
    cria_ponte(Pos1, Pos2, Ponte):

    Com Pos1 e Pos2 posicoes de um puzzle, Ponte eh uma ponte entre essas duas
    posicoes.
*/  
cria_ponte(Pos1, Pos2, Ponte):-
    sort([Pos1, Pos2], [Pos_Ord1, Pos_Ord2]),
    Ponte = ponte(Pos_Ord1, Pos_Ord2).


% 2.7 - caminho_livre/5

/*
    caminho_livre(Pos1, Pos2, Posicoes, Ilha, Vizinha):

    Com Pos1 e Pos2 posicoes de um puzzle, Posicoes a lista de posicoes entre
    Pos1 e Pos2 e Ilha e Vizinha ambas ilhas do puzzle, o predicado eh true se
    a relacao de vizinhanca entre Ilha e Vizinha nao for perturbada pela
    adicao de uma ponte entre Pos1 e Pos2.
*/  
caminho_livre(Pos1, Pos2, Posicoes, ilha(_, PosI), ilha(_, PosV)):-
    Lista_Pos = [Pos1, Pos2, PosI, PosV], % Este bloco faz com que o predicado
    sort(Lista_Pos, LP_Sem_Dupes), % ------- seja true caso Pos1 e Pos2 forem as
    length(LP_Sem_Dupes, 2); % ------------ posicoes da ilha e da sua vizinha.
    posicoes_entre(PosI, PosV, Pos_Entre_Ilhas),
    \+ (member(Random_Pos, Posicoes), member(Random_Pos, Pos_Entre_Ilhas)).


% 2.8 - actualiza_vizinhas_entrada/5

/*
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada):

    Com Pos1 e Pos2 posicoes de um puzzle, Posicoes a lista de posicoes entre
    Pos1 e Pos2 e Entrada uma entrada desse puzzle, Nova_Entrada eh a entrada
    resultante depois de se removerem da lista das vizinhas as ilhas que
    deixam de ser vizinhas da ilha de Entrada apos a adicao da ponte
    ponte(Pos1, Pos2).
*/  
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada):-
    Entrada = [Ilha, Vizinhas, Pontes],
    Nova_Entrada = [Ilha, Novas_Viz, Pontes],
    findall(Viz,
        (
            member(Viz, Vizinhas),
            caminho_livre(Pos1, Pos2, Posicoes, Ilha, Viz)
        ),
        Novas_Viz).


% 2.9 - actualiza_vizinhas_apos_pontes/4

/*
    actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado):

    Sendo Estado um estado de um puzzle e Pos1 e Pos2 posicoes desse puzzle
    entre as quais vai ser estabelecida uma ponte, Novo_Estado eh esse mesmo
    estado apos a aplicacao do predicado actualiza_vizinhas_entrada/5 a cada
    uma das suas entradas.
*/
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado):-
    posicoes_entre(Pos1, Pos2, Posicoes),
    findall(N_Entr,
        (
            member(Entr, Estado),
            actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entr, N_Entr)
        ),
        Novo_Estado).


% 2.10 - ilhas_terminadas/2

/*
    ilhas_terminadas(Estado, Ilhas_Term):

    Sendo Estado um estado de um puzzle, Ilhas_Term eh uma lista de ilhas que
    pertencem as entradas de Estado e que se encontram num estado terminado,
    ou seja, que ja teem todas as suas pontes atribuidas.
*/
ilhas_terminadas(Estado, Ilhas_Term):-
    findall(Ilha,
        (
            Entrada = [Ilha, _, Pontes],
            Ilha = ilha(N_Pontes, _),
            member(Entrada, Estado),
            N_Pontes \== 'X',
            length(Pontes, N_Pontes)
        ),
        Ilhas_Term).


% 2.11 - tira_ilhas_terminadas_entrada/3

/*
    tira_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada):

    Sendo Ilhas_Term uma lista de ilhas terminadas e Entrada uma entrada de um
    puzzle, Nova_Entrada eh essa mesma entrada apos se retirarem as ilhas de
    Ilhas_Term da lista de ilhas vizinhas de Entrada.
*/
tira_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada):-
    Entrada = [Ilha, Vizinhas, Pontes],
    Nova_Entrada = [Ilha, N_Vizinhas, Pontes],
    subtract(Vizinhas, Ilhas_Term, N_Vizinhas).


% 2.12 - tira_ilhas_terminadas/3

/*
    tira_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado):

    Sendo Estado um estado de um puzzle e Ilhas_Term uma lista de ilhas
    terminadas, Novo_Estado eh o estado resultante da aplicacao do predicado
    tira_ilhas_terminadas_entrada/3 a cada uma das entradas de Estado.
*/
tira_ilhas_terminadas([], _, []).
tira_ilhas_terminadas([Entr | Estado], Ilhas_Term, [N_Entr | Novo_Estado]):-
    tira_ilhas_terminadas_entrada(Ilhas_Term, Entr, N_Entr),
    tira_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado).


% 2.13 - marca_ilhas_terminadas_entrada/3

/*
    marca_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada):

    Sendo Ilhas_Term uma lista de ilhas terminadas e Entrada uma entrada de um
    puzzle entao, se a ilha de Entrada pertencer a Ilhas_Term, Nova_Entrada
    eh igual a Entrada com a excecao do numero de pontes da sua ilha que sera
    'X'. Se, pelo contrario, a ilha de Entrada nao pertencer a Ilhas_Term,
    Nova_Entrada eh igual a Entrada.
*/
marca_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada):-
    Entrada = [Ilha, Vizinhas, Pontes],
    Ilha = ilha(_, PosI),
    member(Ilha, Ilhas_Term),
    Nova_Entrada = [ilha('X', PosI), Vizinhas, Pontes];
    Entrada = Nova_Entrada.


% 2.14 - marca_ilhas_terminadas/3

/*
    marca_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado):

    Sendo Estado um estado de um puzzle e Ilhas_Term uma lista de ilhas
    terminadas, Novo_Estado eh o estado resultante da aplicacao do predicado
    marca_ilhas_terminadas_entrada/3 a cada uma das entradas de Estado.
*/
marca_ilhas_terminadas([], _, []).
marca_ilhas_terminadas([Entr | Estado], Ilhas_Term, [N_Entr | Novo_Estado]):-
    marca_ilhas_terminadas_entrada(Ilhas_Term, Entr, N_Entr),
    marca_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado).


% 2.15 - trata_ilhas_terminadas/2

/*
    trata_ilhas_terminadas(Estado, Novo_Estado):

    Sendo Estado um estado de um puzzle, Novo_Estado eh o estado resultante da
    aplicacao dos predicados tira_ilhas_terminadas e marca_ilhas_terminadas a
    Estado.
*/
trata_ilhas_terminadas(Estado, Novo_Estado):-
    ilhas_terminadas(Estado, Ilhas_Term),
    tira_ilhas_terminadas(Estado, Ilhas_Term, Estado_Mod),
    marca_ilhas_terminadas(Estado_Mod, Ilhas_Term, Novo_Estado).

    
% 2.16 - junta_pontes/5

/*
    junta_pontes(Estado, N_Pontes, Ilha1, Ilha2, Novo_Estado):

    Sendo Estado um estado de um puzzle, Ilha1 e Ilha2 duas ilhas desse
    puzzle e N_Pontes um inteiro entre 1 e 2, Novo_Estado eh o estado que
    resulta da adicao de N_Pontes pontes entre Ilha1 e Ilha2.
*/
junta_pontes(Estado, N_Pontes, Ilha1, Ilha2, Novo_Estado):-
    Ilha1 = ilha(_, Pos1),
    Ilha2 = ilha(_, Pos2),
    cria_ponte(Pos1, Pos2, Ponte),
    ((N_Pontes =:= 1 -> Ponte_Final = [Ponte]);
    Ponte_Final = [Ponte, Ponte]),
    pontes_aux(Estado, Ponte_Final, Ilha1, Ilha2, Estado_Mod1),
    actualiza_vizinhas_apos_pontes(Estado_Mod1, Pos1, Pos2, Estado_Mod2),
    trata_ilhas_terminadas(Estado_Mod2, Novo_Estado).

/*
    pontes_aux(Estado, Ponte, Ilha1, Ilha2, Novo_Estado):

    Sendo Estado um estado de um puzzle, Ilha1 e Ilha2 duas ilhas desse
    puzzle e Ponte uma lista com a(s) ponte(s) que existem entre Ilha1 e Ilha2,
    Novo_Estado eh o estado que resulta da adicao de Ponte ah lista de pontes
    das entradas de Estado correspondentes ah Ilha1 e ah Ilha2.
*/
pontes_aux([], _, _, _, []).
pontes_aux([Entr | Estado], N_Ponte, Ilha1, Ilha2, [N_Entr | Novo_Estado]):-
    Entr = [Ilha, Viz, P],
    (Ilha == Ilha2; Ilha == Ilha1),
    append(P, N_Ponte, Ponte_Final),
    N_Entr = [Ilha, Viz, Ponte_Final],
    pontes_aux(Estado, N_Ponte, Ilha1, Ilha2, Novo_Estado).

pontes_aux([Entr | Estado], Ponte, Ilha1, Ilha2, [N_Entr | Novo_Estado]):-
    Entr \== [Ilha1, _, _],
    Entr \== [Ilha2, _, _],
    N_Entr = Entr,
    pontes_aux(Estado, Ponte, Ilha1, Ilha2, Novo_Estado).
