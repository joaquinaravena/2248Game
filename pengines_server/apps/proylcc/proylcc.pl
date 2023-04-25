:- module(proylcc, 
	[  
		join/4
	]).


:- dynamic minimo/1.
minimo(1).

:- dynamic maximo/1.
maximo(6).

:- dynamic pathResult/1.
pathResult(0).
/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, NumOfColumns, Path, RGrids):-
	Grid = [_ | Ns],
	borrarElementos(Grid, Path, NumOfColumns, GridEliminados, NewValue),
	updateMax(log(NewValue)/log(2)),
	updateMin(GridEliminados),
	assertz(pathResult(0)),
	GridNueva = [NewValue | Ns],
	RGrids = [GridEliminados, GridNueva].

/**
 * borrarElementos(+Grid, +Path, +NumColumnas, -GridElim, -ValorNuevo)
 * 
 */
borrarElementos(Grid, [[I,J]], NumColumnas, GridN, Value):-
	Index is I*NumColumnas+J,
	nth0(Index, Grid, OldValue),
	smallerPow2GreaterOrEqualThan(OldValue, NewValue),
  Value is NewValue,
	replace(Grid, Index, NewValue, GridN).

borrarElementos(Grid, [[I,J]|Tail], NumColumnas, GridElim, Value):-
    Index is I * NumColumnas + J,
		nth0(Index, Grid, OldValue),
		updatePath(OldValue),
    replace(Grid, Index, 0, GridRep),
    borrarElementos(GridRep, Tail, NumColumnas, GridElim, Value).

/**
 * 
 */
smallerPow2GreaterOrEqualThan(Result, Value):-
	Log2num = floor(log(Result)/log(2)),
	Result is 2**Log2num,
	Value is Result;
	Value is 2**(Log2num+1).

/**
 * 
 */
updatePath(Number):-
	retract(pathResult(Result)),
	NewResult  is Result+Number,
	assertz(pathResult(NewResult)).
/**
 * replace(+[H|T], +I, +X, -[H|R])
 * [H|T] es la lista de la cuál se quiere reemplazar el elemento en el índice I por X
 * [H|R] es la lista a retornar con el elemento reemplazado por X
 */

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    NI is I - 1,
    replace(T, NI, X, R).

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
