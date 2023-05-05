:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

 join(Grid, NumOfColumns, Path, RGrids):-
	pathtoIndex(Path, NumOfColumns, [], IndexPath),
	deletePathInGrid(Grid, IndexPath, 0, GridEliminados),
	initializeLists([], NumOfColumns, ColumnsList),
	gridToColumns(GridEliminados, ColumnsList,0, NewColumnsList),
	min_list(Grid, AuxMin),
	max_list(GridEliminados, AuxMax),
	Min is round(log(AuxMin)/log(2)),
	Max is round(log(AuxMax)/log(2)),
	gravityFalls(GridEliminados, NewColumnsList, [], TotalGrids, Min, Max),
	append([GridEliminados], TotalGrids, RGrids).

/**
 * collapse(+Grid, +NumOfColumns, -RGrids)
 * Dados todos los grupos de adyacentes en la grilla utilizando shellAdyacents, 
 * los borra y reemplaza el último valor de cada grupo por la potencia de dos correspondiente
 * Retorna las grilla resultantes utilizando gravedad de a un bloque.
 */
collapse(Grid, NumOfColumns, RGrids):-
	shellAdyacents(Grid, 0, NumOfColumns, [], [], ToCollapse),
	deleteAllPaths(Grid, ToCollapse, GridElim),
	initializeLists([], NumOfColumns, ColumnsList),
	gridToColumns(GridElim, ColumnsList, 0, NewColumnsList),
	min_list(Grid, AuxMin), max_list(Grid, AuxMax),
	Min is round(log(AuxMin)/log(2)), 
	Max is round(log(AuxMax)/log(2))+1,
	gravityFalls(GridElim, NewColumnsList, [], TotalGrids, Min, Max),
	append([GridElim], TotalGrids, RGrids).

/**
 * falta hacer, es para borrar los valores que quedan sueltos cuando va aumentando el square generado
 */
removeLowValues(Grid, Value, UpdatedGrid).

/**
 * deleteAllPaths(+Grid, +[H|Tail], -GridEliminated)
 * Borra todos los Path de la lista pasada por parámetro.
 */
deleteAllPaths(Grid, [], Grid).
deleteAllPaths(Grid, [H|Tail], GridEliminated):-
 	deletePathInGrid(Grid, H, 0, GridAux),
	deleteAllPaths(GridAux, Tail, GridEliminated).

/**
 * shellAdyacents(+Grid, +Index, +NumOfColumns, +Visited, +AuxList, -AdyacentList)
 * Encuentra los grupos de adyacentes para toda la grilla.
 */
shellAdyacents(Grid, Index,_, Visited, AuxList, AuxList):-
    length(Grid, LengthGrid), Index >= LengthGrid.

shellAdyacents(Grid, Index, NumOfColumns, Visited, AuxList, AdyacentList):-
	\+ member(Index, Visited),
	addFirst(Index, [], InitialList),
	findGroups(Grid, [Index], NumOfColumns, InitialList, Group),
	length(Group, LengthGroup), LengthGroup > 1,	
	append(Visited, Group, UpdatedVisited),
	append(AuxList, [Group], UpdatedAuxList),
  	length(Grid, LengthGrid), Index < LengthGrid,
	NewIndex is Index+1,
	shellAdyacents(Grid, NewIndex, NumOfColumns, UpdatedVisited, UpdatedAuxList, AdyacentList);

  	length(Grid, LengthGrid), Index < LengthGrid,
	NewIndex is Index+1,
	shellAdyacents(Grid, NewIndex, NumOfColumns, Visited, AuxList, AdyacentList).

/**
 * findGroups(+Grid, +[Index|Tail], +NumOfColumns, +Visited, -Group)
 * Recorre la lista [Index|Tail], y busca el grupo de adyacentes del valor en la posición Index de la grilla
 * El último valor del grupo es el que se encuentre más abajo-derecha.
 */
findGroups(_, [], _, Visited, Visited).
findGroups(Grid, [Index|Tail], NumOfColumns, Visited, Group):-
	getRow(NumOfColumns, Index, 0, Row),
	nth0(Index, Grid, Value),
	%Starts at midRight
	MidRight is Index+1,
	checkSameGroup(Grid, MidRight, Row, NumOfColumns, Value, Visited, UpdatedList4, UpdatedList5),
	%Up
    UpRight is Index-NumOfColumns+1,
	checkSameGroup(Grid, UpRight, Row-1, NumOfColumns, Value, Visited, [], UpdatedList1),
	UpMid is Index-NumOfColumns,
	checkSameGroup(Grid, UpMid, Row-1, NumOfColumns, Value, Visited, UpdatedList1, UpdatedList2),
	UpLeft is Index-NumOfColumns-1,
	checkSameGroup(Grid, UpLeft, Row-1, NumOfColumns, Value, Visited, UpdatedList2, UpdatedList3),
	%Mid
	MidLeft is Index-1,
	checkSameGroup(Grid, MidLeft, Row, NumOfColumns, Value, Visited, UpdatedList3, UpdatedList4),
	%Down
	DownRight is Index+NumOfColumns+1,
	checkSameGroup(Grid, DownRight, Row+1, NumOfColumns, Value, Visited, UpdatedList5, UpdatedList6),
	DownMid is Index+NumOfColumns,
	checkSameGroup(Grid, DownMid, Row+1, NumOfColumns, Value, Visited, UpdatedList6, UpdatedList7),
	DownLeft is Index+NumOfColumns-1,
	checkSameGroup(Grid, DownLeft, Row+1, NumOfColumns, Value, Visited, UpdatedList7, UpdatedList8),

	concatenateWithoutReps(UpdatedList8, Visited, NewVisited),
	concatenateWithoutReps(UpdatedList8, Tail, NewTail),
	findGroups(Grid, NewTail, NumOfColumns, NewVisited, Group).
    
/**
 * checkSameGroup(+Grid, +Searched, +Row, +NumOfColumns, +Value, +Visited, +ActualList, -UpdatedList)
 * Chequea si el elemento Searched pertenece al grupo correspondiente al valor Value y lo retorna en
 * UpdatedList si pertenece al mismo grupo. 
 */
checkSameGroup(Grid, Searched, Row, NumOfColumns, Value, Visited, ActualList, UpdatedList):-
	getRow(NumOfColumns, Searched, 0, AuxRow),
	AuxRow is Row, 
	nth0(Searched, Grid, Element),
	Value =:= Element,
	\+ member(Searched, Visited),
	addLast(Searched, ActualList, UpdatedList);
	
	UpdatedList = ActualList.

/**
 * getRow(+NumOfColumns, +Index, +ActualRow, -ReturnRow)
 * Retorna la fila a la cual pertenece un Index, utiliza NumOfColumns para calcular en que fila se encuentra,
 * y el valor es retornado en ReturnRow.
 */
getRow(NumOfColumns, Index, ActualRow, ReturnRow):-
	Index < NumOfColumns, ReturnRow = ActualRow.

getRow(NumOfColumns, Index, ActualRow, ReturnRow):-
	Index >= NumOfColumns, NewIndex is Index-NumOfColumns, NewRow is ActualRow+1,
	getRow(NumOfColumns, NewIndex, NewRow, ReturnRow).

/**
 * gravityFalls(+Grid, +ColumnsList, +AuxGrids, -ReturnGrids, +Min, +Max)
 * Recorre cada columna de la lista ColumnsList para aplicarle gravedad a cada bloque de las mismas hasta
 * que no hayan más 0 en la grilla final.  
 */
gravityFalls(Grid, _, AuxGrids, AuxGrids, _, _):-
    \+ member(0, Grid).
gravityFalls(Grid, ColumnsList, AuxGrids, ReturnGrids, Min, Max):-
	member(0, Grid),
	addLast([], ColumnsList, NewColumnsList),
	nth0(0, NewColumnsList, FirstList),
	gravityOneSquare(FirstList,0, NewColumnsList, AuxColumnsGravity, Min, Max),
    remove([], AuxColumnsGravity, ColumnsGravity),
	nth0(0, ColumnsGravity, Column),	
	columnsToGrid(Column, ColumnsGravity,0, [], GridGravity),
	addLast(GridGravity, AuxGrids, NewAuxGrids),
	gravityFalls(GridGravity, ColumnsGravity,  NewAuxGrids, ReturnGrids,Min, Max).

/**
 * gravityOneSquare(+List, +IndexOfList, +ColumnsList, -GravityList, +Min, +Max)
 * Recorre la lista de una columna, busca si hay elementos eliminados (valor = 0),
 * los elimina de la lista y genera un nuevo bloque random en el tope de la columna. 
 */
gravityOneSquare([], _, ColumnsList, ColumnsList,_,_).
gravityOneSquare(List, IndexOfList, ColumnsList, GravityList, Min, Max):-
  List \= [],
	member(0, List),
	nth0(IndexElem, List, 0),
	removeIndex(List, IndexElem, ListElim),
	squareGenerator(Min, Max, AuxValue),
	Value is round(log(AuxValue)/log(2)),
	NewMax is max(Max, Value),
	addFirst(AuxValue, ListElim, Aux),
	replace(ColumnsList, IndexOfList, Aux, NewList),
	NewIndex is IndexOfList+1,
  nth0(NewIndex,ColumnsList, NextList),
	gravityOneSquare(NextList, NewIndex, NewList, GravityList, Min, NewMax);
	
  List \= [],
	NewIndex is IndexOfList+1,
  nth0(NewIndex,ColumnsList, NextList),
	gravityOneSquare(NextList, NewIndex, ColumnsList, GravityList, Min, Max).


/**
 * gridToColumns(+[H|Tail], +ColumnsList, +Index, -NewList)
 * Recibe la lista correspondiente a la grilla y genera una lista de listas,
 * donde cada lista contiene los elementos de cada columna. 
 */
gridToColumns([],ColumnsList,_,ColumnsList).	
gridToColumns([H|Tail], ColumnsList, Index, NewList):-
	nth0(Index, ColumnsList, IndexedList),
	addLast(H, IndexedList, Aux),
	replace(ColumnsList, Index, Aux, ReturnList),
	length(ColumnsList, NumOfColumns),
	NewIndex is (Index+1) mod NumOfColumns,
	gridToColumns(Tail, ReturnList, NewIndex, NewList).

/**
 * columnsToGrid(+[H|Tail], +ColumnsList, +Index, +GridList, -ReturnList)
 * Recibe las columnas separadas por listas y las agrupa en una única lista grilla. 
 * GridList es utilizado para ir almacenando la grilla paso por paso, para luego ser retornada cuando la 
 * lista inicial esté vacía.
 */
columnsToGrid([], _, _, GridList, GridList).
columnsToGrid([H|Tail], ColumnsList, Index, GridList, ReturnList):-
	addLast(H, GridList, UpdatedList),
	remove(H, [H|Tail], UpdatedCurrent),
	replace(ColumnsList, Index, UpdatedCurrent, UpdatedColumnsList),
	length(ColumnsList, NumOfColumns),
	NewIndex is (Index+1) mod NumOfColumns,
  nth0(NewIndex, ColumnsList,NewElement),
	columnsToGrid(NewElement, UpdatedColumnsList, NewIndex, UpdatedList, ReturnList).

/**
 * initializeLists(+List, +NumOfLists, -ReturnList)
 * Retorna una lista de NumOfLists listas vacías ([[],[],[]...])
 */
initializeLists(List, 0, List).
initializeLists(List, NumOfLists, ReturnList):-
	addLast([], List, NewList),
	Aux is NumOfLists-1,
	initializeLists(NewList, Aux, ReturnList).

/**
 * deletePathInGrid(+Grid, +Path, +TotalPath, -GridElim)
 * Borra todos los elementos del Path en Grid, y el último lo reemplaza por la potencia de dos adecuada
 */
deletePathInGrid(Grid, [Index|[]], TotalPath, GridElim):-
	nth0(Index, Grid, OldValue),
	NewTotalPath is TotalPath+OldValue,
	smallerPow2GreaterOrEqualThan(NewTotalPath, NewValue),
	replace(Grid, Index, NewValue, GridElim).

deletePathInGrid(Grid, [Index|Tail], TotalPath, GridElim):-
	nth0(Index, Grid, OldValue),
	NewTotalPath is TotalPath+OldValue,
  replace(Grid, Index, 0, GridRep),
  deletePathInGrid(GridRep, Tail, NewTotalPath, GridElim).

/**
 * pathtoIndex(+[[I,J]|Tail], +NumOfColumns, +AuxList, -IndexList)
 * Convierte una lista de elementos de la forma [I,J] a una lista de indices.
 */
pathtoIndex([], _, AuxList, AuxList).
pathtoIndex([[I,J]|Tail], NumOfColumns, AuxList, IndexList):-
	Index is I*NumOfColumns + J,
	addLast(Index, AuxList, NewAuxList),
	pathtoIndex(Tail, NumOfColumns, NewAuxList, IndexList).

/**
 * smallerPow2GreatorOrEqualThan(+Result, -Value)
 * Calcula la menor potencia de 2, que sea mayor o igual al Result pasado por parámetro.
 */
smallerPow2GreaterOrEqualThan(Result, Value):-
	Log2num = floor(log(Result)/log(2)),
	Result is 2**Log2num,
	Value is Result;

	Log2num = floor(log(Result)/log(2)),
	Value is 2**(Log2num+1).

/**
 * removeIndex(+[H|Tail], +Indice, -[H|Remainder])
 * Remueve el elemento en el indice pasado por parámetro y retorna la lista actualizada.
 */
removeIndex([], _, []).
removeIndex([_|Tail], 0, Tail).
removeIndex([H|Tail], Index, [H|Remainder]) :-
    Index > 0,
    NewIndex is Index - 1,
    removeIndex(Tail, NewIndex, Remainder).

/**
 * replace(+[H|Tail], +Index, +X, -Remainder)
 * [H|Tail] es la lista de la cuál se quiere reemplazar el elemento en el índice Index por X
 * [H|Remainder] es la lista a retornar con el elemento reemplazado por X
 */
replace([_|Tail], 0, X, [X|Tail]).
replace([H|Tail], Index, X, [H|Remainder]) :-
    Index > 0, NewIndex is Index - 1,
    replace(Tail, NewIndex, X, Remainder).

/**
 * squareGenerator(+Min, +Max, -Number)
 * Min es la potencia de 2 más baja de la grilla, Max es la potencia de 2 más alta de la grilla y Number es
 * un número aleatorio potencia de 2 entre Min y Max. Se utiliza para generar el valor de un Square nuevo.
 */
squareGenerator(Min, Max,Number):- 
	random(Min, Max, Random), Number is 2**Random.

/**
 * addLast(+X, +[H|Tail], -[H|Remainder])
 * Agrega el elemento X al final de la lista pasada por parámetro y retorna la nueva lista.
 * CB: Agrega X a una lista vacia. 
 * CR: La lista es no vacía, la recorre recursivamente hasta llegar al final y agrega X. 
 */
addLast(X,[],[X]).
addLast(X,[H|Tail],[H|Remainder]):- addLast(X,Tail,Remainder). 

/**
 * addFirst(+X, +List, -[X|List])
 * Agrega el elemento X al principio de la lista. 
 */
addFirst(X,[],[X]).
addFirst(X,List,[X|List]). 

/**
 * remove(+X,+[H|Tail],[H|NewTail])
 * Elimina el elemento X de la lista. 
 * CB: X es el header de la lista.
 * CR: X se encuentra en el tail de la lista, busco X en tail. 
 */  
remove(X,[X|Tail],Tail).
remove(X, [H|Tail], [H|NewTail]):-
  X \= H, remove(X,Tail,NewTail).

/**
 * removeNegatives(+[H|Tail], -UpdatedList)
 * Remueve todos los elementos negativos de una lista hasta que la lista esté vacía. 
 */
removeNegatives([], []).
removeNegatives([H|Tail], UpdatedList) :-
    H < 0, removeNegatives(Tail, UpdatedList).
removeNegatives([H|Tail], [H|UpdatedTail]) :-
    H >= 0, removeNegatives(Tail, UpdatedTail).

/**
 * concatenateWithoutReps(+[H|Tail], +L2, -[H|Result])
 * Concatena [H|Tail] con L2, pero sin agregar los elementos de L1 que ya aparezcan en L2
 */
concatenateWithoutReps([], L2, L2).
concatenateWithoutReps([H|Tail], L2, Result) :-
    member(H, L2),
    concatenateWithoutReps(Tail, L2, Result).
concatenateWithoutReps([H|Tail], L2, [H|Result]) :-
    \+ member(H, L2),
    concatenateWithoutReps(Tail, L2, Result).