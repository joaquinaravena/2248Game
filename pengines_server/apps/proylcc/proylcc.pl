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

join(Grid, NumOfColumns, Path, RGrids):-
	Grid = [_ | Ns],
	borrarElementos(Grid, Path, NumOfColumns, 0, GridEliminados, NewValue),
	updateMax(log(NewValue)/log(2)),
	updateMin(GridEliminados),
	GridNueva = [NewValue | Ns],
	RGrids = [GridEliminados, GridNueva].

/**
 * borrarElementos(+Grid, +Path, +NumColumnas, +TotalPath, -GridElim, -NewValue)
 * En la lista Grid, recorre todos los elementos de la lista Path y los reemplaza por un 0, lo cuál
 * representa un bloque vacío. Al llegar al último elemento del Path, este debe ser aumentado utilizando
 * la función "smallerPow2GreaterOrEqualThan".
 * NumColumnas se utiliza para calcular el índice de los elementos eliminados en la grilla.
 * TotalPath se utiliza para mantener el resultado total de los valores recorridos en el Path.
 * GridElim es la nueva grilla con los elementos ya eliminados, mientras que NewValue es el valor del 
 * último bloque el cuál fue aumentado. 
 */
borrarElementos(Grid, [[I,J]], NumColumnas, TotalPath, GridElim, NewValue):-
	Index is I*NumColumnas+J,
	nth0(Index, Grid, OldValue),
	NewTotal is TotalPath+OldValue,
	smallerPow2GreaterOrEqualThan(NewTotal, NewValue),
	replace(Grid, Index, NewValue, GridElim).

borrarElementos(Grid, [[I,J]|Tail], NumColumnas, TotalPath, GridElim, NewValue):-
    Index is I * NumColumnas + J,
	nth0(Index, Grid, OldValue),
	NewTotal is TotalPath+OldValue,
    replace(Grid, Index, 0, GridRep),
    borrarElementos(GridRep, Tail, NumColumnas, NewTotal, GridElim, NewValue).

/**
 * smallerPow2GreatorOrEqualThan(+Result, -Value)
 * Calcula la menor potencia de 2, que sea mayor o igual al Result pasado por parámetro.
 * Este resultado es retornado en Value
 */
smallerPow2GreaterOrEqualThan(Result, Value):-
	Log2num = floor(log(Result)/log(2)),
	Result is 2**Log2num,
	Value is Result;
	Log2num = floor(log(Result)/log(2)),
	Value is 2**(Log2num+1).

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
	asserta(maximo(NewMax)).

/**
 * updateMin(+Grid)
 *	Actualiza el hecho mínimo, el cuál guarda la potencia de 2 más chica que se encuentra en la grilla
 *  Min representa el indice de una potencia
 */ 
updateMin(Grid):-
	retract(minimo(Min)),
	not(find(2**Min, Grid)),
	NewMin is Min+1,
	asserta(minimo(NewMin)).

/**
 * find(+X, +[Y|Tail])
 * Busca X dentro de la lista pasada como segundo parámetro
 */
find(X,[X|_]). 
find(X,[_|Tail]):- 
  find(X,Tail). 
