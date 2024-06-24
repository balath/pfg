# bach-machine: Generación de Música Coral mediante sistemas inteligentes

### ¿Por qué?
bach-machine es el Proyecto Fin de Grado presentado por @balath para el Grado en Ingeniería Informática de la 
Escuela Técnica Superior de Ingeniría Informática de la UNED. La idea surge a raíz de la práctica del mismo nombre 
realizada para la asignatura del de Fundamentos de la IA, dentro del mismo Grado, que consistió en un ejercicio muy
sencillo de programación lógica realizado con Prolog sobre un sistema de reglas para la generación de una estructura 
armónica de un coral.

### ¿Qué?
bach-machine es un generador de corales al estilo de Johann Sebastian Bach, que muestrea la compleja estructura armónica
de una pieza de estilo coral a partir en un modelo probabilista basado en cadenas de Markov, para después armonizar a 4
voces esta estructura mediante un algoritmo de búsqueda. Este sistema se ha montado bajo un servidor http que sirve las
piezas corales generadas a una petición sobre una tonalidad dada. 

El sistema se estructura en seis componentes:

+ [Un conjunto de datos](data/rawData.txt): Anotación de los cifrados armónicos de una colección de corales de J.S. Bach.
+ [Analizador](src/main/scala/Parser.scala): Procesa el conjunto de datos y construye las distribuciones de probabilidad que conforman el modelo.
+ [Modelo](src/main/scala/Model.scala): Descripción del modelo probabilista utilizado para muestrear corales en tonalidades mayores o menores.
+ [Generador](src/main/scala/Model.scala): Aplica un muestreo estocástico simple sobre el modelo para generar las piezas corales. 
+ [Armonizador](src/main/scala/Harmonizer.scala): Emplea un algoritmo de backtracking para buscar una armonización que cumpla con las reglas básicas de armonización de corales.
+ [Interfaz](balath.github.io): Página web mediante la que se utiliza el modelo

### ¿Cómo?
bach-machine puede usarse a través de la [web de la aplicación](balath.github.io) o puede desplegarse en local.

Para el despliegue en local se necesita:
+ [Scala](https://www.scala-lang.org/) y [sbt](www.scala-sbt.org) (en caso de que quieran hacerse modificaciones al código)
+ [Docker](https://www.docker.com/products/docker-desktop/)

Una vez iniciado Docker:
```bash
docker image build -t bach-machine:latest .
docker run bach-machine:latest
```
Con el contenedor ejecutándose, podemos enviar peticiones por `curl`:
```bash
key="c" #Notas en cifrado americano (c,d,e...) terminadas en is para sostenidos o es para bemoles (cis, des...)
mode="minor" #[minor|major]
response=$(curl -s "localhost:8080/choral/$key/$mode")
pdfId=$(echo $response |  awk -F'"' '{print $4}')
midiId=$(echo $response |  awk -F'"' '{print $8}')
curl -o "choral.pdf" "localhost:8080/pdf/$pdfId"
curl -o "choral.midi" "localhost:8080/midi/$midiId"  #En windows cambiar la extensión a .mid
ls choral*
```