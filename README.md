# CalK-Server
Servidor API básico de una calcula escrita en Haskell con Yesod Web Framework

## Dependencias
Para usar el servidor es necesario tener instalado Stack Haskell, Yesod Web Framework, Wai-Cors y Cabal.

## Correr el servidor
Para usar el servidor se ejecuta el comando `stack runghc calc.hs` y el servicio se iniciará en el puerto 3000, 
puedes comprobar ésto ingresando en tu navegador a `http://localhost:3000/`

## Uso
Puedes hacer uso de cualquiera de las rutas definidas en *calc-Api.hs* para obtener una respuesta JSON del resultado. 
Por ejemplo, al ingresar `http://localhost:3000/seno/5` obtendras un JSON con el contenido:
`{"resultado":-0.9589242746631363}`.
