# 2248 in React + Prolog

Interactive 2248 game implementation using React on the client side and Prolog on the server side.

## Running the project

### Method 1: Using Docker (Recommended)

This is the easiest and fastest way to run the project:

1. **Make sure you have Docker installed** on your system.

2. **Run the following command** in the project root directory:

   ```bash
   docker-compose up --build
   ```

3. **Wait for both services to build and start:**

   - The Prolog/Pengines server will be available at http://localhost:3030
   - The React application will be available at http://localhost:3000

4. **Open your browser** and go to [http://localhost:3000](http://localhost:3000) to play.

5. **To see the web admin console** of the Prolog server, go to http://localhost:3030/admin/server.html

Done! You don't need to install Node.js or SWI-Prolog manually.

### Method 2: Manual installation (Alternative)

If you prefer to run the services manually:

#### Setup and execution of the Pengines server

- [Download](https://www.swi-prolog.org/Download.html) and install SWI-Prolog.

- Start the server by running the `run.pl` file in SWI-Prolog from the `pengines_server` folder:

  `cd pengines_server`\
  `swipl run.pl`

  or by double-clicking on `run.pl`.

  Note: don't run `swipl pengines_server/run.pl` because some references won't work later.

  The first time you run run.pl, you'll be asked to define a username and password to access the server's web admin console. Choose any (for example, username: 'lcc' and password: 'lccdcic'), but don't leave them empty.

- The server will listen on http://localhost:3030

- Go to http://localhost:3030/admin/server.html to see the web admin console.

- The `pengines-master/apps/proylcc` folder contains the Prolog code for the 2248 game. Every time you modify this code, you need to stop and restart the server for the changes to take effect.

#### Setup and execution of the React application

- Download a recent version of [Node.js](https://nodejs.org/en/).

- Run

  `npm install`

  in the project directory to install dependencies (libraries)
  locally in the `node_modules` folder.

- Run

  `npm start`

  in the project directory to run the app in development mode.

- Open [http://localhost:3000](http://localhost:3000) to see the application in the browser.

- The page refreshes automatically when the code changes.
