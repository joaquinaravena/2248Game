import { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Square from './Square';
import { joinResult } from './util';
import { ToastContainer, toast } from 'react-toastify';
import 'react-toastify/dist/ReactToastify.css';

let pengine;

function Game() {
  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  const [showScore, setShowScore] = useState(true);

  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    setShowScore(newPath.length === 0);
    setPath(newPath);
    console.log(JSON.stringify(newPath));
  }
  
  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    /*
    Build Prolog query, which will be like:
    join([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        ).
      
    */
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    setShowScore(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        msgFromPathLength(path.length);
        setPath([]);
        animateEffect(response['RGrids']);

      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 350);
    } else {
      setWaiting(false);
    }
  }
  
  /**
   * Colapsa todos los grupos del mismo valor para toda la grilla, no suma puntos
   */
  function collapse(){
    const gridS = JSON.stringify(grid);
    const queryS = "collapse("+gridS+", "+numOfColumns+", RGrids)"; 
    setWaiting(true);
    pengine.query(queryS, (success, response) =>{
      if (success){
        if(response['RGrids'].length === 1){
          msgFromPathLength(-2);
        }
        animateEffect(response['RGrids']);
      }else{
        setWaiting(false);
      }
    });
  }

  /**
   * calcula y muestra el camino que consiga el mayor número a partir de la configuración actual.
   * 
   */
  function maxMove(){
    const gridS = JSON.stringify(grid);
    const queryS = "maxMove("+gridS+", "+numOfColumns+", Path)"; 
    setWaiting(true);
    pengine.query(queryS, (success, response) =>{
      if (success){
        setWaiting(false);
        if(response['Path'].length === 0){
          msgFromPathLength(-1);
        }
        onPathChange(response['Path']);
      }else{
        setWaiting(false);
      }
    });
  }
/**
 * calcule y muestre el camino que consiga generar el número más grande posible adyacente a otro igual 
 * (preexistente). Si hay más de uno que cumpla con esta condición, mostrar cualquiera de ellos. 
 */
  function maxEqual(){
    const gridS = JSON.stringify(grid);
    const queryS = "maxEqual("+gridS+", "+numOfColumns+", Path)"; 
    console.log(queryS);
    setWaiting(true);
    pengine.query(queryS, (success, response) =>{
      if (success){
        setWaiting(false);
        if(response['Path'].length === 0){
          msgFromPathLength(-1);
        }
        onPathChange(response['Path']);
      }else{
        setWaiting(false);
      }
    });
  }

  function msgFromPathLength(length){
    console.log(length);
    let msg = "";
    switch(true){
        case length === -2:
          msg = "No hay bloques para colapsar"; break;
        case length === -1: 
          msg = "No se ha encontrado ningún camino"; break;
        case length === 6: 
          msg = "Genial!"; break;
        case length === 7:
          msg = "Perfecto!"; break;
        case length === 8:
          msg = "Fabuloso!"; break;
        case length > 8 & length < 11:
          msg = "Fantástico!"; break;
        case length >= 11:
          msg= "GOD!!"; break;
        default: 
          msg = ""; break;
      }
    if(msg !== "")
      return toast(msg);
  }

  /**
   * utilizada para mostrar el puntaje o el square que se genera cuando corresponda
   */
  const scoreOrSquare = showScore ? 
    (<div className="score">{score}</div>) : 
    (<Square value={joinResult(path, grid, numOfColumns)} className="score"/>);

  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">  
        {scoreOrSquare}
      </div>
      <ToastContainer
          position="top-center"
          autoClose={600}
          hideProgressBar
          newestOnTop={true}
          closeOnClick
          rtl={false}
          pauseOnFocusLoss
          draggable
          pauseOnHover
          theme="dark"/>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
      <aside className="asideBar"> 
        <div className="asideBox">
          <button className="buttons" onClick={collapse} disabled={waiting | !showScore}>Colapsar <br/>iguales</button>
          <button className="buttons" onClick={maxMove} disabled={waiting}> Movida<br />Máxima</button>
          <button className="buttons" onClick={maxEqual} disabled={waiting}>Máximos iguales<br />adyacentes</button>
        </div>
      </aside>
    </div>
  );
}

export default Game;