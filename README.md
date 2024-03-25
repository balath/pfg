# Generación de Música Coral mediante sistemas inteligentes
### Objetivos
Desarrollo de un sistema que genere corales armonizados a 4 voces, con una estructura armónica coherente con el estilo de composición de corales de Johann Sebastian Bach y siguiendo las reglas de armonización correspondientes.

### Método de desarrollo, fases del trabajo y fechas de realización.
El sistema tiene cuatro partes claramente diferenciadas:
+ Creación de la base de conocimiento (BC): Anotación de estructuras armónicas extraídas de análisis armónicos de alrededor de 400 corales de J.S. Bach.
+ Generación de armonías (GA): Diseño y desarrollo del módulo que genera una nueva estructura armónica, en forma de un bajo cifrado, a partir de la BC y una entrada de parámetros.
+ Armonización: Diseño y desarrollo del algoritmo que implementa las normas de armonización a 4 voces partiendo del cifrado obtenido por la GA.
+ Interfaz: Obtención del resultado en formato musical.

Tras una fase preliminar de investigación sobre el estado del arte, se analizarán y decidirán las aproximaciones y herramientas a utilizar en el diseño de cada parte del proyecto, tras lo que se iniciará la fase de diseño e implementación. Se utilizarán metodologías ágiles en el
desarrollo del proyecto, aplicando un flujo iterativo e incremental sobre las cuatro partes a desarrollar, e integrando las pruebas en este flujo:
+ [BC](data/rawData.txt): Secciones de, aproximadamente, 50 corales anotados por iteración.
+ [GA](src/main/scala/Model.scala): Producto inicial sobre el que se irán añadiendo y ajustando parámetros de forma cualitativa y cuantitativa.
+ [Armonización](src/main/scala/DSL.scala): Algoritmo de búsqueda al que se irán añadiendo requisitos: tesitura de las voces, movimiento de las voces, resoluciones, movimientos prohibidos…
+ Interfaz: Entrada de parámetros, obtención de la salida, aplicación de formatos estándares.

Se estima la realización del proyecto a lo largo de una serie de [8-10 iteraciones de dos semanas](https://github.com/users/balath/projects/4) a desarrollar entre los meses de febrero y junio, con el fin de realizar la defensa a lo largo del mes de julio, si bien a lo largo del primer cuatrimestre se realizará la investigación preliminar y los análisis y decisiones sobre el diseño del sistema.

### Medios a utilizar y breve justificación de la pertinencia de los mismos.
Para la gestión del proyecto se utilizará el entorno ofrecido por Github, que ofrece herramientas de gestión, planificación, control de versiones, automatización de pruebas e integración y repositorios privados, en este caso de forma gratuita con la cuenta de estudiante.

Para la realización de la memoria se utilizará el sistema de creación de documentos LaTeX mediante el editor online [Overleaf](https://www.overleaf.com/project), que permite sincronización con el repositorio.

El proyecto requiere un lenguaje de programación que facilite la descripción de un lenguaje específico del dominio relativo a la armonía musical, a efectos de expresar notas musicales, intervalos, tipos y familias de acordes, su composición y adaptación a distintas tonalidades, etc. Además, debe facilitar el uso posterior de esta información a la hora de aplicar las reglas de armonización a una sucesión de acordes, para lo que sería óptimo la capacidad de anidamiento de funciones y encaje de patrones o pattern matching. Por estas y otras razones, se desarrollará previsiblemente en lenguaje Scala, que encaja perfectamente con estos requisitos. Además, se utilizarán otros estándares de notación musical como MusicXML o [LilyPond](https://lilypond.org/index.es.html) para la salida.