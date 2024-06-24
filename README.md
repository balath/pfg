# <center><img src="bach.gif" alt="Bach" width="50" align="center"> Bach-machine </center>

## Generación de música coral mediante sistemas inteligentes

### ¿Por qué?
Bach-machine es el Proyecto Fin de Grado presentado por @balath para el Grado en Ingeniería Informática de la 
Escuela Técnica Superior de Ingeniría Informática de la UNED. La idea surge a raíz de la práctica del mismo nombre 
realizada para la asignatura del de Fundamentos de la IA, dentro del mismo Grado, que consistió en un ejercicio muy
sencillo de programación lógica realizado con Prolog sobre un sistema de reglas para la generación de una estructura 
armónica de un coral.

### ¿Qué?
Bach-machine es un generador de corales al estilo de Johann Sebastian Bach, que muestrea la compleja estructura armónica
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
Bach-machine puede usarse a través de la [web de la aplicación](balath.github.io) o puede desplegarse en local.

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
key="c"
mode="minor"
response=$(curl -s "localhost:8080/choral/$key/$mode")
pdfId=$(echo $response |  awk -F'"' '{print $4}')
midiId=$(echo $response |  awk -F'"' '{print $8}')
curl -o "choral${key}-${mode}.pdf" "localhost:8080/pdf/$pdfId"
curl -o "choral-${key}-${mode}.midi" "localhost:8080/midi/$midiId"
ls choral*
```
Para las peticiones, podemos usar los tonos (`key`):
```
 Do♭ ⇒ ces
 Do ⇒ c
 Do♯ ⇒ cis
 Re♭ ⇒ des
 Re ⇒ d
 Re♯ ⇒ dis
 Mi♭ ⇒ ees 
 Mi ⇒ e
 Fa ⇒ f
 Fa♯ ⇒ fis
 Sol♭ ⇒ ges
 Sol ⇒ g
 Sol♯ ⇒ gis
 La♭ ⇒ aes
 La ⇒ a
 La♯ ⇒ ais
 Si♭ ⇒ bes
 Si ⇒ b
```
y los modos (`mode`):
```
menor ⇒ minor
Mayor ⇒ major
```