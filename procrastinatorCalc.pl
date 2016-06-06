% Autor: Igor Quintela Santana
% DNI: 78925640J
% Fecha: 17/04/2016
% UNED 2015/2016
% Tema: Los videojuegos
% Título: PROCRASTINATOR CALCULATOR

%-------------------------------------------------------------------------------
%            PREDICADOS DINAMICOS
% Usados para la inserción y borrado de elementos en la base de conocimiento (BC)
% y escogidos mediante el menu de gestion de la calculadora
%
%-------------------------------------------------------------------------------
:- dynamic(info/5,juegos/2,rol/1,accion/1,fps/1,aventura/1,deportes/1,estrategia/1,puzzle/1,peleas/1).

%-------------------------------------------------------------------------------
%            CONSTANTES: PLATAFORMAS
%-------------------------------------------------------------------------------
playstation4.
pc.
xboxone.
wiiu.

%-------------------------------------------------------------------------------
%            GÉNEROS
%-------------------------------------------------------------------------------

% ROL
rol(skyrim).
rol(finalFantasy13).
rol(darkSouls3).
rol(stardewValley).
rol(hyperLightDrifter).
rol(baldursGate2).
rol(vampireTheMasquerade).
rol(diablo2).
rol(theWitcher3).
rol(dragonAge).
rol(deusEx).
rol(massEffect2).
rol(finalFantasy7).

% ACCION (se han incluido accion-aventura, third person, ...)
accion(theLastOfUs).
accion(gta5).
accion(bastion).
accion(batmanArkhamAsylum).
accion(hotLineMiami).
accion(darksiders).
accion(metalGearSolid5).
accion(quantumBreak).
accion(theDivision).

% FIRST PERSON SHOOTER (fps)
fps(doom).
fps(farCry).
fps(bioshock).
fps(crysis).
fps(halLife2).
fps(borderlands2).
fps(codBlackOps3).
fps(dishonored).
fps(deadIsland).

% AVENTURA (gráfica)
aventura(grimFandango).
aventura(theWalkingDead).
aventura(fahrenheit).
aventura(theLongestJourney).
aventura(monkeyIsland).
aventura(lifeIsStrange).
aventura(goneHome).
aventura(brokenSword).
aventura(loom).
aventura(dayOfTheTentacle).
aventura(theStanleyParable).

% DEPORTES
deportes(marioKart8).
deportes(nba2K16).
deportes(rocketLeague).
deportes(pes2016).
deportes(tonyHawk5).
deportes(forzaMotorsport6).
deportes(ufc).
deportes(wiiSports).

% ESTRATEGIA
estrategia(starcraft2).
estrategia(civilization5).
estrategia(warcraft3).
estrategia(xcom).
estrategia(commandConquer3).
estrategia(commandos).
estrategia(ageOfEmpires2).
estrategia(companyOfHeroes).
estrategia(riseOfNations).

% PUZZLE
puzzle(portal2).
puzzle(tetris).
puzzle(theWitness).
puzzle(fez).
puzzle(braid).
puzzle(theTalosPrinciple).
puzzle(limbo).

% PELEAS
peleas(mortalKombatX).
peleas(streetFighterV).
peleas(superSmashBros).
peleas(tekken6).
peleas(guiltyGearXrd).
peleas(soulCaliburV).
peleas(pokkenTournament).


%-------------------------------------------------------------------------------
%            INFORMACION EXTRA VIDEOJUEGOS
%(nombre, año de salida, desarrollador, DuracionCorta, DuracionLarga)
%-------------------------------------------- ----------------------------------
info(skyrim,2011,bethesda,31,105).
info(finalFantasy13,2009,squareEnix,50,62).
info(darkSouls3,2016,fromSoftware,26,37).
info(stardewValley,2016,concernedApe,56,62).
info(hyperLightDrifter,2016,concernedApe,8,9).
info(baldursGate2,2000,bioWare,75,88).
info(vampireTheMasquerade,2004,troikaGames,24,29).
info(diablo2,2000,blizzard,35,43).
info(theWitcher3,2015,cdProjekt,46,97).
info(dragonAge,2009,bioware,41,59).
info(deusEx,2000,ionStorm,24,28).
info(massEffect2,2010,bioware,25,36).
info(theLastOfUs,2013,naughtyDog,15,18).
info(gta5,2013,rockstar,31,45).
info(doom,1993,idSoftware,4,5).
info(farCry,2004,crytek,14,17).
info(bioshock,2007,irrationalGames,12,16).
info(crysis,2007,crytek,11,13).
info(bastion,2011,supergiantGames,7,9).
info(batmanArkhamAsylum,2010,rocksteady,11,17).
info(halLife2,2004,valve,14,16).
info(hotLineMiami,2012,dennatonGames,5,7).
info(grimFandango,1998,lucasArts,12,12).
info(theWalkingDead,2012,telltale,13,13).
info(fahrenheit,2005,quanticDream,8,9).
info(theLongestJourney,2000,funcom,17,21).
info(monkeyIsland,1990,lucasfilm,7,7).
info(lifeIsStrange,2015,squareEnix,15,19).
info(goneHome,2013,theFullbrightCompany,2,3).
info(brokenSword,2006,sumoDigital,12,13).
info(loom,1990,lucasfilm,3,3).
info(dayOfTheTentacle,1993,lucasArts,7,14).
info(marioKart8,2014,nintendo,5,16).
info(nba2K16,2015,visualConcepts,11,11).
info(rocketLeague,2015,psyonix,4,12).
info(pes2016,2015,pesProductions,33,96).
info(tonyHawk5,2015,robomodo,3,4).
info(forzaMotorsport6,2015,turn10,21,53).
info(ufc,2014,ea,12,16).
info(wiiSports,2013,namco,3,3).
info(starcraft2,2010,blizzard,18,21).
info(civilization5,2010,firaxis,36,69).
info(warcraft3,2002,blizzard,22,27).
info(xcom,2012,firaxis,26,33).
info(commandConquer3,2009,ea,11,19).
info(commandos,1998,pyroStudios,22,22).
info(ageOfEmpires2,1999,ensemble,47,66).
info(companyOfHeroes,2006,relic,19,23).
info(portal2,2011,valve,9,13).
info(tetris,1986,alexeyPajitnov,7,13).
info(theWitness,2016,thekla,18,28).
info(fez,2012,polytron,6,9).
info(braid,2008,numberOne,5,6).
info(theTalosPrinciple,2014,croteam,16,20).
info(riseOfNations,2003,bigHugeGames,15,36).
info(borderlands2,2012,gearboxSoftware,31,52).
info(darksiders,2010,vigilGames,18,21).
info(metalGearSolid5,2015,kojimaProductions,43,79).
info(theStanleyParable,2013,daveyWreden,2,3).
info(limbo,2010,playdead,3,4).
info(finalFantasy7,1997,square,39,57).
info(mortalKombatX,2015,netherRealm,5,13).
info(streetFighterV,2016,capcom,2,3).
info(superSmashBros,2014,sora,4,42).
info(soulCaliburV,2012,projectSoul,2,26).
info(tekken6,2009,namco,9,13).
info(guiltyGearXrd,2009,arcSystem,6,6).
info(quantumBreak,2016,remedy,10,15).
info(pokkenTournament,2016,gameFreak,11,14).
info(theDivision,2016,ubisoft,16,35).
info(codBlackOps3,2015,treyarch,9,12).
info(dishonored,2012,arkane,13,18).
info(deadIsland,2011,techland,18,27).

%-------------------------------------------------------------------------------
%            VENTAS POR GÉNERO (estadistica del 2014 en EEUU)
%       Existen categorías que no se han tenido en cuenta o incluido en otras
%-------------------------------------------------------------------------------

 ventasGenero(accion,fps).
 ventasGenero(fps,deportes).
 ventasGenero(deportes,rol).
 ventasGenero(rol,aventura).
 ventasGenero(aventura,peleas).
 ventasGenero(peleas,estrategia).
 ventasGenero(estrategia,puzzle).
 
% comparacion de ventas por genero
 mejorVentas(X,Y) :- ventasGenero(X,Y).
 mejorVentas(X,Y) :- ventasGenero(X,Z), mejorVentas(Z,Y).
 
%-------------------------------------------------------------------------------
%     CRITICA POR GÉNERO (estadistica del 2015 de reviews oficiales mundial)
%       Existen categorías que no se han tenido en cuenta o incluido en otras
%-------------------------------------------------------------------------------

 criticaGeneroOficial(accion,rol).
 criticaGeneroOficial(rol,aventura).
 criticaGeneroOficial(aventura,deportes).
 criticaGeneroOficial(deportes,puzzle).
 criticaGeneroOficial(puzzle,peleas).
 criticaGeneroOficial(peleas,fps).
 criticaGeneroOficial(fps,estrategia).
 
 % comparacion de criticas oficiales
 mejorCriticaOficial(X,Y) :- criticaGeneroOficial(X,Y).
 mejorCriticaOficial(X,Y) :- criticaGeneroOficial(X,Z), mejorCriticaOficial(Z,Y).
 
%-------------------------------------------------------------------------------
%   CRITICA POR GÉNERO (estadistica de users mundial de los 50 primeros juegos)
%       Existen categorías que no se han tenido en cuenta o incluido en otras
%-------------------------------------------------------------------------------

 criticaGeneroUsers(fps,rol).
 criticaGeneroUsers(rol,accion).
 criticaGeneroUsers(accion,aventura).
 criticaGeneroUsers(aventura,deportes).
 criticaGeneroUsers(deportes,puzzle).
 criticaGeneroUsers(puzzle,estrategia).
 criticaGeneroUsers(estrategia,peleas).

 % comparacion de criticas de usuarios
 mejorCriticaUsers(X,Y) :- criticaGeneroUsers(X,Y).
 mejorCriticaUsers(X,Y) :- criticaGeneroUsers(X,Z), mejorCriticaUsers(Z,Y).

 % definimos la comparacion entre las tres categorías, habiendo hecho una ponderación de importancia
 % basada en valores numéricos, 3 en la opinión de los usuarios, 
 % un 2 en la critica oficial y un 1 en las ventas por género.
 % Dependiendo de cual se cumpla se escoge imprimir en pantalla el mejor de ambos (o ninguno si existe empate)
 mejorTotal(_,_,0,0).
 mejorTotal(Genero1,Genero2,Var1,Var2):- 
	mejorVentas(Genero1,Genero2),
	Var1 is Var1+1,
	mejorVentas(Genero2,Genero1),
	Var2 is Var2+1,
	criticaGeneroOficial(Genero1,Genero2),
	Var1 is Var1+2,
	criticaGeneroOficial(Genero2,Genero1),
	Var2 is Var2+2,
	criticaGeneroUsers(Genero1,Genero2),
	Var1 is Var1+3,
	criticaGeneroUsers(Genero2,Genero1),
	Var2 is Var2+3,
	imprimirMejorTotal(Genero1,Genero2,Var1,Var2).
	
% imprimimos en pantalla el mejor de los dos géneros
imprimirMejorTotal(Genero1,Genero2,Var1,Var2) :-
	Var1 > Var2,
	format('El mejor género para escoger es: ~w',[Genero1]), nl,
	Var1 < Var2,
	format('El mejor género para escoger es: ~w',[Genero2]), nl,
	Var1 = Var2,
	format('No hay diferencia entre los géneros a escoger'),nl.
 
%-------------------------------------------------------------------------------
%            LISTADO DE JUEGOS
%-------------------------------------------------------------------------------

juegos(rol,[skyrim,finalFantasy13,darkSouls3,stardewValley,hyperLightDrifter,baldursGate2,vampireTheMasquerade,diablo2,theWitcher3,dragonAge,deusEx,massEffect2,finalFantasy7]).
juegos(accion,[theLastOfUs,gta5,bastion,batmanArkhamAsylum,hotLineMiami,darksiders,metalGearSolid5,quantumBreak,theDivision]).
juegos(fps,[doom,farCry,bioshock,crysis,halLife2,borderlands2,codBlackOps3,dishonored,deadIsland]).
juegos(aventura,[grimFandango,theWalkingDead,fahrenheit,theLongestJourney,monkeyIsland,lifeIsStrange,goneHome,brokenSword,loom,dayOfTheTentacle,theStanleyParable]).
juegos(deportes,[marioKart8,nba2K16,rocketLeague,pes2016,tonyHawk5,forzaMotorsport6,ufc,wiiSports]).
juegos(estrategia,[starcraft2,civilization5,warcraft3,xcom,commandConquer3,commandos,ageOfEmpires2,companyOfHeroes,riseOfNations]).
juegos(puzzle,[portal2,tetris,theWitness,fez,braid,theTalosPrinciple,limbo]).
juegos(peleas,[mortalKombatX,streetFighterV,superSmashBros,tekken6,guiltyGearXrd,soulCaliburV,pokkenTournament]).

%-------------------------------------------------------------------------------
%            MENU CALCULADORA
%-------------------------------------------------------------------------------
% el menu se inicia con el comando inicio. Se borra la pantalla y se llama al menu Inicial
inicio :- clear, menuInicio. % comienza el programa
clear :- write('\033[2J'). % borrar pantalla

% Menu inicial para la introduccion de inputs del usuario, que llevan a otros submenus
menuInicio :-
	nl,nl,
	write('**************************************'),nl,
	write('      PROCRASTINATOR CALCULATOR'),nl,
	write('**************************************'),nl,
	write(' MENU --> Inicio'),nl,nl, % menu principal de opciones
	write(' 1 - Quiero procrastinar'),nl,
	write(' 2 - Gestionar Base de Conocimiento'),nl,
	write(' 3 - Comparar géneros de manera estadística'),nl,nl,
	write(' s - Salir'),nl,nl,
	write(' Introduzca una opción: '),
	read(O), clear, opcion(O).

% Submenu para escoger las horas, el tipo de duracion del juego y el tipo de genero por parte del usuario	
menuProcrastinar :-
	nl,nl,
	write('MENU --> Procrastinar:'),nl,nl,
	write('A continuación, inserte los parámetros que se le piden:'),nl,
	write('¿Cuántas horas tiene disponibles en el período a procrastinar?'),nl,
	read(Horas), nl,
	write('¿Quiere completar todo en un juego? (responda s o n)'),nl,
	read(Duracion), nl,
	write('Escoja cualquiera de los siguientes géneros juegos (escriba la palabra): '),nl,
	write(' rol'),nl,
	write(' accion'),nl,
	write(' fps'),nl,
	write(' aventura'),nl,
	write(' deportes'),nl,
	write(' estrategia'),nl,
	write(' puzzle'),nl,
	write(' peleas'),nl,
	read(Genero), nl,
	escogerGenero(Genero,Horas,Duracion),
	menuInicio.

% Submenu para escoger las opciones de gestion de la Base de Conocimiento
menuGestion :-
	nl,nl,
	write('MENU --> Gestion Base de Conocimiento:'),nl,nl,
	write(' 1 - Consultar un juego'),nl,
	write(' 2 - Insertar juego'),nl,
	write(' 3 - Borrar juego'),nl,
	write(' 4 - Listar todos los juegos'),nl,nl,
	write(' s - Salir'),nl,nl,
	write(' Indique la opción que desea: '),
	read(BC), clear, gestion(BC),
	menuGestion.

% Submenu para escoger las opciones de comparaciones	
menuComparar :- 
	write('MENU --> Comparar géneros de manera estadística:'),nl,nl,
	write(' 1 - Por Ventas'),nl,
	write(' 2 - Por Críticas oficiales'),nl,
	write(' 3 - Por Críticas de usuarios'),nl, 
	write(' 4 - Evaluando las tres opciones anteriores'),nl, nl,
	write(' s - Salir'),nl,nl,
	write(' Indique la opción que desea: '),
	read(CP), clear, comparar(CP).

% CONFIGURACION ACCIONES MENUS
% Inicio, se borra siempre la pantalla y se llama a la regla de su menu correspondiente
opcion(1) :-  clear, menuProcrastinar.
opcion(2) :-  clear, menuGestion.
opcion(3) :-  clear, menuComparar.
opcion(s) :-  halt.
% si se introduce un caracter no reflejado en cualquier momento vuelve a Inicio
opcion(_) :-  write('Parámetro/Género incorrecto, vuelva a intentarlo'),nl,nl,
menuInicio.

% Gestion de Base de Conocimiento
% consulta en el listado de juegos por el nombre especificado (input usuario, C)
gestion(1) :-
	write('¿Qué juego quieres consultar? (Nombre)'), nl, read(C), nl,
	listing(info(C,_,_,_,_)),
	menuGestion. % consulta el juego en BC por el parametro de Nombre

% pregunta primero al usuario todos los parametros necesarios para introducir el juego en la base de datos	
gestion(2) :-
	write('Introduce los datos del juego a añadir. '), nl,
	write('Nombre: '), read(Nombre), nl,
	write('Año: '), read(Ano), nl,
	write('Desarrollador: '), read(Desarr), nl,
	write('Duración sólo Historia: '), read(HCorta), nl,
	write('Duración completa: '), read(HLarga), nl,
	write('Género al que pertenece: '), read(G), nl,
	insertar(Nombre,Ano,Desarr,HCorta,HLarga),
	insertarG(G,Nombre),
	menuGestion.

% elimina el juego de la BC por nombre recogido	
gestion(3) :-
	write('Indique el Nombre del juego a eliminar. '), nl,
	write('Nombre: '), read(Nombre), nl,
	borrar(Nombre,_,_,_,_),
	menuGestion.

% lista con el predicado definido listing todos los datos de la BC
gestion(4) :-
	listing(info),
	menuGestion.
	
% salir de la aplicacion
gestion(s) :-  
	halt.

% Gestion de Menu Comparacion
% recoge los dos generos deseados y nos muestra cual se encuentra en mejor posicion
comparar(1) :-
	write('Introduce el primer género a comparar: '), read(Genero1), nl,
	write('Introduce el segundo género a comparar: '), read(Genero2), nl,
	mejorVentas(Genero1,Genero2), % se llama a la función de comparación con los input del usuario
	format('El género ~w se encuentra mejor posicionado.', [Genero1]), nl,
	mejorVentas(Genero2,Genero1),
	format('El género ~w se encuentra mejor posicionado.', [Genero2]), nl,
	menuComparar.

comparar(2) :-
	write('Introduce el primer género a comparar: '), read(Genero1), nl,
	write('Introduce el segundo género a comparar: '), read(Genero2), nl,
	mejorCriticaOficial(Genero1,Genero2),
	format('El género ~w se encuentra mejor posicionado.', [Genero1]), nl,
	mejorCriticaOficial(Genero2,Genero1),
	format('El género ~w se encuentra mejor posicionado.', [Genero2]), nl,
	menuComparar.

comparar(3) :-
	write('Introduce el primer género a comparar: '), read(Genero1), nl,
	write('Introduce el segundo género a comparar: '), read(Genero2), nl,
	mejorCriticaUsers(Genero1,Genero2),
	format('El género ~w se encuentra mejor posicionado.', [Genero1]), nl,
	mejorCriticaUsers(Genero2,Genero1),
	format('El género ~w se encuentra mejor posicionado.', [Genero2]), nl,
	menuComparar.
	
comparar(4) :-
	write('Introduce el primer género a comparar: '), read(Genero1), nl,
	write('Introduce el segundo género a comparar: '), read(Genero2), nl,
	mejorTotal(Genero1,Genero2,_,_),
	menuComparar.

comparar(s) :-
	gestion(s).
	
%-------------------------------------------------------------------------------
%            GESTION CALCULADORA
%-------------------------------------------------------------------------------

% insertar un juego en la BC al final de la lista, en el predicado info
insertar(Nombre,Ano,Desarr,HCorta,HLarga) :-
	assertz(info(Nombre,Ano,Desarr,HCorta,HLarga)).

% inserta el juego en su correspondiente género (tanto en el listado general como en el predicado juegos)
insertarG(G,Nombre):-
	Aux = juegos(G,[_|Nombre]), % como carallo lo añado a la lista
	retract(juegos(G,_)),
	assert(Aux),
	listing(juegos(rol,_)).

% elimina un juego de la BC
borrar(Nombre,_,_,_,_) :-
	retract((info(Nombre,_,_,_,_))).

% agrupa en una lista el genero que el usuario desea para el submenu procrastinar
escogerGenero(Genero,Horas,Duracion):-
    juegos(Genero,Lista),
    write('Escoja un juego de la siguiente lista:'),nl,
    recorrer(Lista,Horas,Duracion).

% Inicia para posteriormente recorrer toda la lista de la regla de escogerGenero	
recorrer([],_,_).

% calculo (comparacion) para el juego no completo
% El parametro de Cola se usa de manera recursiva para revisar todos los juegos de la lista agrupada
recorrer([X|Cola],Horas,Duracion) :-
    Duracion == (n),  % Duracion Corta de juego cogiendo el primer parametro de horas a comparar
    info(X,_,_,Y,_),Horas>=Y, 
    format('  - ~w',[X]), nl,
    recorrer(Cola,Horas,Duracion).
         
% calculo (comparacion) para el juego completo
recorrer([X|Cola],Horas,Duracion) :-
    Duracion == (s),    % Duracion Corta de juego cogiendo el segundo parametro de horas a comparar
    info(X,_,_,_,Y),Horas>=Y,
    format('  - ~w',[X]), nl,
    recorrer(Cola,Horas,Duracion).

% recursividad en la lista para que vuelva a pasar por los juegos restantes         
recorrer([_|Cola],Horas,Duracion) :-
    recorrer(Cola,Horas,Duracion).
