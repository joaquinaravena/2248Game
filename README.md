# 2248 en React + Prolog

Implementación de un 2248 interactivo, usando React del lado del cliente, y Prolog del lado del servidor.

## Para correr el proyecto

### Método 1: Usando Docker (Recomendado)

Esta es la forma más fácil y rápida de ejecutar el proyecto:

1. **Asegúrate de tener Docker instalado** en tu sistema.

2. **Ejecuta el siguiente comando** en el directorio raíz del proyecto:

   ```bash
   docker-compose up --build
   ```

3. **Espera a que se construyan y levanten ambos servicios:**

   - El servidor Prolog/Pengines estará disponible en http://localhost:3030
   - La aplicación React estará disponible en http://localhost:3000

4. **Abre tu navegador** y ve a [http://localhost:3000](http://localhost:3000) para jugar.

5. **Para ver la consola web admin** del servidor Prolog, ve a http://localhost:3030/admin/server.html

¡Listo! No necesitas instalar Node.js ni SWI-Prolog manualmente.

### Método 2: Instalación manual (Alternativa)

Si prefieres ejecutar los servicios manualmente:

#### Setup y ejecución del servidor Pengines

- [Descargar](https://www.swi-prolog.org/Download.html) e instalar el SWI-Prolog.

- Levantar el servidor ejecutando en SWI-Prolog el `run.pl` en la carpeta `pengines_server`:

  `cd pengines_server`\
  `swipl run.pl`

  o haciendo doble click sobre el `run.pl`.

  Aclaración: no hacer `swipl pengines_server/run.pl` porque algunas referencias luego no funcionan.

  La primera vez que se ejecute el run.pl se pedirá definir un username y un password para acceder a la consola web admin del servidor, elegir cualquiera (por ejemplo, username: 'lcc' y password: 'lccdcic'), pero no dejar vacíos.

- El servidor escuchará en http://localhost:3030

- Ir a http://localhost:3030/admin/server.html para ver la consola web admin.

- La carpeta `pengines-master/apps/proylcc` contiene el código prolog del 2248. Cada vez que se modifica este código es necesario bajar y volver a levantar el servidor para que se reflejen los cambios.

#### Setup y ejecución de la aplicación React

- Descargar una versión reciente de [Node.js](https://nodejs.org/en/).

- Ejecutar

  `npm install`

  en el directorio del proyecto para instalar las dependencias (librerías)
  localmente, en la carpeta `node_modules`.

- Ejecutar

  `npm start`

  en el directorio del proyecto para correr la app en modo desarrollo.

- Abrir [http://localhost:3000](http://localhost:3000) para ver la aplicación en el browser.

- La página se refresca automáticamente cuando cambia el código.
