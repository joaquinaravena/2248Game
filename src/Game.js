import { useEffect, useState } from "react";
import PengineClient from "./PengineClient";
import Board from "./Board";
import Square from "./Square";
import { joinResult } from "./util";
import { ToastContainer, toast } from "react-toastify";
import "react-toastify/dist/ReactToastify.css";
import { Tooltip } from "react-tooltip";

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
    const queryS = "init(Grid, NumOfColumns)";
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response["Grid"]);
        setNumOfColumns(response["NumOfColumns"]);
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
    const queryS =
      "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    setShowScore(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        msgFromPathLength(path.length);
        setPath([]);
        animateEffect(response["RGrids"]);
      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Displays each grid of the sequence as the current grid with smooth transitions.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 400);
    } else {
      setWaiting(false);
    }
  }

  /**
   * Collapses all groups of the same value for the entire grid, does not add points
   */
  function collapse() {
    // Show warning toast
    toast.warning("Collapsing equal blocks...", {
      position: "top-center",
      autoClose: 2000,
      hideProgressBar: false,
      closeOnClick: true,
      pauseOnHover: true,
      draggable: true,
      progress: undefined,
    });

    const gridS = JSON.stringify(grid);
    const queryS = "collapse(" + gridS + ", " + numOfColumns + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        if (response["RGrids"].length === 1) {
          msgFromPathLength(-2);
        }
        animateEffect(response["RGrids"]);
      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Calculates and shows the path that achieves the highest number from the current configuration.
   *
   */
  function maxMove() {
    const gridS = JSON.stringify(grid);
    const queryS = "maxMove(" + gridS + ", " + numOfColumns + ", Path)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setWaiting(false);
        if (response["Path"].length === 0) {
          msgFromPathLength(-1);
        }
        onPathChange(response["Path"]);
      } else {
        setWaiting(false);
      }
    });
  }
  /**
   * Calculate and show the path that generates the largest possible number adjacent to another equal
   * (preexisting). If there is more than one that meets this condition, show any of them.
   */
  function maxEqual() {
    const gridS = JSON.stringify(grid);
    const queryS = "maxEqual(" + gridS + ", " + numOfColumns + ", Path)";
    console.log(queryS);
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setWaiting(false);
        if (response["Path"].length === 0) {
          msgFromPathLength(-1);
        }
        onPathChange(response["Path"]);
      } else {
        setWaiting(false);
      }
    });
  }

  function msgFromPathLength(length) {
    console.log(length);
    let msg = "";
    switch (true) {
      case length === -2:
        msg = "No blocks to collapse";
        break;
      case length === -1:
        msg = "No path found";
        break;
      case length === 6:
        msg = "Great!";
        break;
      case length === 7:
        msg = "Perfect!";
        break;
      case length === 8:
        msg = "Fabulous!";
        break;
      case (length > 8) & (length < 11):
        msg = "Fantastic!";
        break;
      case length >= 11:
        msg = "AMAZING!";
        break;
      default:
        msg = "";
        break;
    }
    if (msg !== "") return toast(msg);
  }

  /**
   * Used to display the score or the square that is generated when appropriate
   */
  const scoreOrSquare = showScore ? (
    <div className="score">{score}</div>
  ) : (
    <Square value={joinResult(path, grid, numOfColumns)} className="score" />
  );

  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">{scoreOrSquare}</div>
      <ToastContainer
        position="top-center"
        autoClose={2000}
        hideProgressBar={false}
        newestOnTop={true}
        closeOnClick
        rtl={false}
        pauseOnFocusLoss
        draggable
        pauseOnHover
        theme="colored"
        toastClassName="custom-toast"
        bodyClassName="custom-toast-body"
        progressClassName="custom-toast-progress"
      />
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
      <aside className="asideBar">
        <div className="asideBox">
          <button
            className="buttons"
            onClick={collapse}
            disabled={waiting | !showScore}
            data-tooltip-id="collapse-tooltip"
            data-tooltip-content="Collapse equal adjacent numbers to create higher values (NO POINTS)"
          >
            Collapse <br />
            Equals
          </button>
          <button
            className="buttons"
            onClick={maxMove}
            disabled={waiting}
            data-tooltip-id="maxmove-tooltip"
            data-tooltip-content="Find the path that creates the highest possible number"
          >
            Maximum <br />
            Move
          </button>
          <button
            className="buttons"
            onClick={maxEqual}
            disabled={waiting}
            data-tooltip-id="maxequal-tooltip"
            data-tooltip-content="Find the path with maximum adjacent equal numbers"
          >
            Maximum Adjacent
            <br />
            Equals
          </button>
        </div>
        <Tooltip
          id="collapse-tooltip"
          place="top"
          style={{
            backgroundColor: "rgba(255, 255, 255, 0.95)",
            color: "#333",
            fontSize: "12px",
            fontWeight: "500",
            padding: "10px 16px",
            borderRadius: "8px",
            boxShadow: "0 4px 20px rgba(0, 0, 0, 0.3)",
            border: "1px solid rgba(255, 255, 255, 0.8)",
            backdropFilter: "blur(10px)",
            maxWidth: "300px",
            textAlign: "center",
            lineHeight: "1.3",
            zIndex: 1000,
          }}
        />
        <Tooltip
          id="maxmove-tooltip"
          place="top"
          style={{
            backgroundColor: "rgba(255, 255, 255, 0.95)",
            color: "#333",
            fontSize: "12px",
            fontWeight: "500",
            padding: "10px 16px",
            borderRadius: "8px",
            boxShadow: "0 4px 20px rgba(0, 0, 0, 0.3)",
            border: "1px solid rgba(255, 255, 255, 0.8)",
            backdropFilter: "blur(10px)",
            maxWidth: "300px",
            textAlign: "center",
            lineHeight: "1.3",
            zIndex: 1000,
          }}
        />
        <Tooltip
          id="maxequal-tooltip"
          place="top"
          style={{
            backgroundColor: "rgba(255, 255, 255, 0.95)",
            color: "#333",
            fontSize: "12px",
            fontWeight: "500",
            padding: "10px 16px",
            borderRadius: "8px",
            boxShadow: "0 4px 20px rgba(0, 0, 0, 0.3)",
            border: "1px solid rgba(255, 255, 255, 0.8)",
            backdropFilter: "blur(10px)",
            maxWidth: "300px",
            textAlign: "center",
            lineHeight: "1.3",
            zIndex: 1000,
          }}
        />
      </aside>
    </div>
  );
}

export default Game;
