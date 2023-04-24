:- module(proylcc, 
	[  
		join/4
	]).

:- dynamic minimo/1.
minimo(1).

:- dynamic maximo/1.
maximo(6).
/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, _NumOfColumns, _Path, RGrids):-
	Grid = [N | Ns],
	GridVacia = [N | NS],
	N2 is N * 2,
	updateMax(log(N2)/log(2)),
	updateMin(GridVacia),
	GridNueva = [N2 | NS],
	RGrids = [GridVacia, GridNueva].

/**
 * squareGenerator(+Min, +Max, -Number)
 * Min es la potencia de 2 más baja de la grilla, Max es la potencia de 2 más alta de la grilla y Number es
 * un número aleatorio potencia de 2 entre Min y Max. Se utiliza para generar el valor de un Square nuevo.
 */
squareGenerator(Min, Max, Number):- 
	random(Min, Max, Random),
	Number is 2**Random.

/**
 * updateMax(+Number)
 * Actualiza el hecho maximo, el cuál guarda la potencia de 2 más grande que se encuentra en la grilla
 */
updateMax(Number):-
	retract(maximo(Max)),
	NewMax is max(Max, Number),
	assertz(maximo(NewMax)).

/**
 * updateMin(+Grid)
 *	Actualiza el hecho mínimo, el cuál guarda la potencia de 2 más chica que se encuentra en la grilla
 *  Min representa el indice de una potencia
 */ 
updateMin(Grid):-
	retract(minimo(Min)),
	not(find(2**Min, Grid)),
	NewMin is Min+1,
	assertz(minimo(NewMin)).

/**
 * find(+X, +[Y|Tail])
 * Busca X dentro de la lista pasada como segundo parámetro
 */
find(X,[X|_]). 
find(X,[_|Tail]):- 
  find(X,Tail). 