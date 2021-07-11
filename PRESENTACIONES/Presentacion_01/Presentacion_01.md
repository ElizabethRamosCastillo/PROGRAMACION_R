---
title: "Programando con R"

author:
 - name:  "Ceras Robles, Jose Alexander (código : 17160034)"
 - name:  "Condor Melo,Deysy (código : 17160180)"
 - name:  "Lara Huapaya, José Alberto (código : 17160206)"
 - name:  "Miguel Jurado, Milagros (código : 17160191)"
 - name:  "Ramos Castillo,Jacqueline Elizabeth (código : 17160195)"

date: "`r format(Sys.time(), '%d %B, %Y')`"
logo: rmarkdown.png
output: html_document
  
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Ejercicios

## Parte 1 

Cargar las variables almacenadas en el archivo [Rdata](ei1012-1516-la-s1-datos.RData)


**1. Calcular los valores numéricos aproximados de:**


**a)** $\frac{0.3.0.15}{0.3.0.15+0.2.0.8+0.5.0.12}$

```{r}

a<-0.3*0.15/((0.13*0.15)+(0.2*0.8)+(0.5*0.12))
a
```


Se utilizo operadores aritmeticos, siendo el resultado de **0.187**


**b)** $\frac{5^6}{6!}= e^-5$


```{r}

b<-(5**6/(factorial(6)))*exp(-5)
b
```
Con las funciones nativas de R `factorial` y `exp` se obtuvo como resultado  **0.146**

**c)** $\binom {20}{7}$ 0.4^0.7^0.6^13^

```{r}

c<- (factorial(20)/(factorial(7)*factorial(20-7)))*(0.4**7)*(0.6**13)
c
```
Se utilizo la función nativa de R `factorial` para realizar la operación que dio como resultado  **0.166**


**2. Realizar la siguiente suma**


**a)**  *1+2+3+...+1000* 

```{r}
Suma_naturales<-function(sn){
  sn*(sn+1)/2
}
Suma_naturales(1000)
```
El ejercicio es una suma de números naturales, se opto por crear una función de acuerdo a la fórmula
planteada por *Carl Friedrich Gauss* obteniendo como resultado **500500**.

**b)**  *1+2+4+8+...+1024*

```{r}
nt<-(log(1024/1)/log(2))+1
suma_pg<-function(an,r,a1){
  ((an*r)-a1)/(r-1)
}
suma_pg(1024,2,1)
```

El ejercicio es una suma de una progresión geométrica, inicialmente se obtuvo el valor del número de
términos y posteriormente se creó una función obteniendo como resultado **2047**


**3.El vector grupo representa el `grupo` al que pertenece una serie de alumnos**

Se puede cargar la data con la cual se trabajará con `load` o simplemente abriendolo desde la plataforma.

```{r echo=FALSE}
load("ei1012-1516-la-s1-datos.RData")
```

Datos de `grupo`

```{r}
grupo
```

**a)** *¿Cuántos elementos tiene?*


```{r}
length(grupo)
```
Los datos de grupo están en un vector y puede contarse la cantidad de elementos
con el comando `length` que devuelve un resultado de **192**.



**b)** *¿En qué posiciones del vector está la letra “A”?*

```{r}
which(grupo=="A")
```
Con el comando `wich` podemos indexar datos con una condicional, aplicamos y obtenemos las posiciones del grupo A.

**4. El vector nota representa la `nota` de un examen de los alumnos que están en los grupos del vector `grupo`.**

Datos de `notas`

```{r}
nota

```

**a)** *¿Cuánto suman todas las notas?*

```{r}
sum(nota)

```
Se usa el comando `sum` para obtener la suma de notas totales siendo está **962**.

**b)** *¿Cuál es la media aritmética de todas las notas?*

```{r}
mean(nota)

```
Se usa el comando `mean` para obtner la media de las notas totales siendo está

**c)** *¿En qué posiciones están las notas mayores de 7.0?*

```{r}
which(nota>7)

```
Se usa el comando `wich` y se obtuvo el orden de las notas,

**d)** *Visualiza las notas ordenadas de mayor a menor*

```{r}
sort(nota,decreasing = T)

```

Con el comando `sort` se puede ordenar los datos, y al indicar como `True` al parametro
`decreasing` indicamos que debe ser decreciente.

**e)** *¿En qué posición está la nota máxima?*

```{r}
which(nota==max(nota))

```
Con el comando `wich` indexamos la posición y con `max` seleccionamos el mayor valor de las notas, obteniendo como resultado que la mayor nota está en la posición **120**.

**5. A partir de los vectores grupo y nota definidos.**

**a)** *Suma las notas de los 10 primeros alumnos del vector*

Llamamos a las librerias

```{r}
library(tidyverse)
library(dplyr)
```

```{r}
head(nota,10) 
sum(head(nota,10))
```

**b)** *¿Cuantos alumnos hay del grupo C?*
```{r}
sum(grupo== "C")
```

**c)** *¿Cuantos alumnos han aprobado?*
```{r}
sum(nota>=5.5)
```
**d)** *¿Cuantos alumnos del grupo B han aprobado?*
```{r}
df <- data.frame(grupo,nota)
df02<- as_tibble(df)
nrow (filter(df02, grupo == "B", nota >= 5.5))
```

**e)** *¿Que porcentaje de alumnos del grupo C han aprobado?*
```{r}
notasB <- nota [which( grupo == "B" )] # notas de los alumnos del grupo B
aprobadosC <- nrow (filter(df02, grupo == "C", nota >= 5.5))

which( grupo == "C" )
notasC <- nota [which( grupo == "C" )] # notas de los alumnos del grupo C
which(notasB >= 5.5)
NaprobadosC <- length(which(notasB >= 5.5)) # N° alumnos C aprobados
TotalC <- length(which( grupo == "C") ) # N° total alumnos C
porc <- (aprobadosC/TotalC)*100
print(porc)
```


**f)** *¿De que grupos son la maxima y minima notas de toda la muestra?.*
```{r}
Y<- nota
max(Y) -> Z
Z
min(Y) -> R
R
A <- grupo [which( nota == Z )]
A
B <- grupo [which( nota == R )]
B
```


**g) Nota media de los alumnos de grupo A y B, juntos, considerando sólo a los que han aprobado.**

```{r}
notasA <- nota [which(grupo == "A")] # notas de los alumnos del grupo A
AprobadasA <- notasA[which(notasA >= 5.5)] # Notas aprobadas de A

notasB <- nota [which(grupo == "B")] # notas de los alumnos del grupo B
AprobadasB <- notasB[which(notasB >= 5.5)] # Notas aprobadas de B

c <- c(AprobadasA, AprobadasB)

mean(c)
```

**6. Calcula el percentil 66 de las notas de todos los alumnos, y también de los alumnos del grupo C.**

**a)** *Todos los alumnos*

```{r}
perc <- df$nota 
quantile(perc, c(.66))
```

**b)** *Alumnos del grupo C*

```{r}
GC <- filter(df02, grupo == "C")
GC
perc2 <- GC$nota
quantile(perc2, c(.66))
```

**7. ¿Que porcentaje, del total de alumnos, tiene una nota menor o igual que la suya? ¿Y qué porcentaje tiene una nota mayor o igual que la suya?**

```{r}
x<-c(nota)
A <- which(x <= 4.9)
C <- length(A)
A
B <- length(nota)
(C*100/B)

x<-c(nota)
A <- which(x >= 4.9)
C <- length(A)
A
B <- length(nota)
(C*100/B)
```
**8. Realiza el grafico de diagramas de caja de las notas de cada grupo, para poder comparar el nivel de cada uno de ellos.**

```{r}
N <- c(nota)
N
A <- nota [which( grupo == "A" )]
A
B <- nota [which( grupo == "B" )]
B
C <- nota [which( grupo == "C" )]
C
D <- nota [which( grupo == "D" )]
D
E <- nota [which( grupo == "E" )]
E
boxplot (A, B, C, D, E, main = "NOTAS DE CADA GRUPO",
         xlab = "GRUPOS", ylab = "NOTAS",
         col = c("orange3", "yellow3", "green3", "grey", "yellow3"))
```

**9. Si la variable conc recoge la concentración de plomo (en ppm) en el aire de cierta zona durante un dia completo.**

**a)**  *¿Cuál ha sido la concentraci?n máxima?*

```{r}
max(conc)
```

**b)** *¿En cuántos de los muestreos se ha superado la concentración de 40.0 ppm?*

```{r}
sum(conc > 40.0)
```

**c)** *¿Cuál ha sido la concentración media del día?*

```{r}
mean(conc)
```

**d)** *¿Cuáles fueron las 10 mediciones más bajas del día?*

```{r}
m<-sort(conc)
head(m<-sort(conc), 10)
```

**e)** *Si la primera medida fue a las 00:00. ¿A qué hora del día se alcanzó la concentración máxima?*

```{r}
hora<-seq(as.POSIXct("00:00", format="%H:%M"),
          as.POSIXct("23:59", format="%H:%M"), by="5 min")
h_conc<-data.frame(hora=hora,Medida=conc)
h_conc[max(h_conc$Medida),]

```


## Parte 2

**1. 1.Graficar los puntos $(1,1),(2,4),(3,6),(4,8),(5,25),(6,36),(7,49),(8,61),(9,81),(10,100)$ en un plano utilizando RStudio.**

Como primer paso tenemos que crear un vector que contengan todos los elementos del eje de las abscisas.

Para crear dicho vector, se pueden realizar de diversas maneras:

1) Digitalizar cada elemento del eje de las abscisas.

```{r}
x1 <- c(1,2,3,4,5,6,7,8,9,10)
x1
```

2) Establecer una secuencia de números consecutivos utilizando ":"

```{r}
x2 <- c(1:10)
x2
```


Luego crearemos otro vector que contengan los elementos del eje de las ordenadas.
Para crear dicho vector, al no haber una correspondencia entre cada término,se tendrá que digitalizar cada elemento del eje de las ordenadas.

```{r}
y2 <- c(1,4,6,8,25,36,49,61,81,100)
y2
```
Por último, se procedera a plotearlo, utilizando la funcion "plot"

```{r}
plot(x2,y2)
```

Para borrar el plot actual es de la siguiente manera:

```{r}
dev.off()
```

Esta función presenta una serie de argumentos, por lo que se pueden colocar los siguientes:

```{r}
#Para establecer el color del fondo de la caja:

plot.new()

rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "#ebebeb") 

par(new = TRUE)


#Para establecer los argumentos de la función "Plot"

plot(x2,y2,
     main="Gráfico de puntos",# Título
     sub = "Ejercicio 1",
     las = 1,                 # Rotar las etiquetas de los ejes de manera horizontal
     xlab = "Eje X",          # Etiqueta del eje x
     ylab = "Eje Y",          # Etiqueta del eje y
     cex.main = 1.5,          # Tamaño del título
     cex.sub = 1.2,           # Tamaño del subtítulo
     cex.lab = 1.2,           # Tamaño de las etiquetas de los ejes X e Y
     cex.axis = 1,            # Tamaño de las etiquetas de los ticks de los ejes
     font.sub = 1,            # Estilo de fuente del título sin formato
     font.main  = 2,          # Estilo de fuente del subtítulo en negrita
     font.axis = 3,           # Estilo de fuente de los ejes X e Y en cursiva
     font.lab  = 4,           # Estilo de fuente de los ticks de los ejes en negrita y cursiva
     pch  = 21,               # Estilo del símbolo de los puntos del grafico
     bg = "red",              # Color de fondo del simbolo
     col = "black",           # Color del borde del simbolo
     cex = 1.2,               # Tamaño del símbolo
     lwd = 1.2,               # Ancho del borde
     col.main = "Black",      # Color del título
     col.sub = "blue",        # Color del subtítulo
     col.lab = "orange",      # Color de las etiqetas de los ejes
     col.axis = "maroon4",    # Color de las etiquetas de los ticks
     fg = "green")            # Color de la caja


#Para agregar una grilla es con la función "Grid"

grid(nx = NULL, ny = NULL,    # Agrega una grilla a los ejes X e Y
     lty = 2,                 # Tipo de línea
     col = "gray",            # Color
     lwd = 1)                 # Ancho de línea
par(new = TRUE)

#Para establecer que las grillas se encuentren por debajo del grafico, lo que haremos es plotear denuevo:

plot(x2,y2,
     main="Gráfico de puntos",# Título
     sub = "Ejercicio 1",
     las = 1,                 # Rotar las etiquetas de los ejes de manera horizontal
     xlab = "Eje X",          # Etiqueta del eje x
     ylab = "Eje Y",          # Etiqueta del eje y
     cex.main = 1.5,          # Tamaño del título
     cex.sub = 1.2,           # Tamaño del subtítulo
     cex.lab = 1.2,           # Tamaño de las etiquetas de los ejes X e Y
     cex.axis = 1,            # Tamaño de las etiquetas de los ticks de los ejes
     font.sub = 1,            # Estilo de fuente del título sin formato
     font.main  = 2,          # Estilo de fuente del subtítulo en negrita
     font.axis = 3,           # Estilo de fuente de los ejes X e Y en cursiva
     font.lab  = 4,           # Estilo de fuente de los ticks de los ejes en negrita y cursiva
     pch  = 21,               # Estilo del símbolo de los puntos del grafico
     bg = "red",              # Color de fondo del simbolo
     col = "black",           # Color del borde del simbolo
     cex = 1.2,               # Tamaño del símbolo
     lwd = 1.2,               # Ancho del borde
     col.main = "Black",      # Color del título
     col.sub = "blue",        # Color del subtítulo
     col.lab = "orange",      # Color de las etiqetas de los ejes
     col.axis = "maroon4",    # Color de las etiquetas de los ticks
     fg = "green")            # Color de la caja


#Para establecer las etiquetas de cada punto se realiza de la siguiente manera:

names <- c("(1,1)","(2,4)","(3,6)","(4,8)","(5,25)","(6,36)","(7,49)","(8,61)","(9,81)","(10,100)")

text(x2, y2,
     labels = names,
     cex = 0.6,               # Tamaño del rotulo de los puntos
     pos = 4,                 # Agrega la posición, en este caso la posición "4" es a la derecha
     col = "blue")            # Agrega el color de los rótulos



```

**2. Ingresar la matriz A en RStudio**
$A = \begin{pmatrix} 1 & 2 & 3 \\ 2 & 4 & 6 \\ 3 & 6 & 9 \\ 4 & 8 & 12  \end{pmatrix}$

Hay 3 formas de crear una matriz:

**2.1. Usando la funcion DIM:**

Primero creamos el vector que contiene a todos los elementos de la matriz:

```{r}
A1<-c(1,2,3,4,2,4,6,8,3,6,9,12)
A1
```
Luego declaramos las dimensiones de la matriz, en este caso es de 4 filas x 3 columnas:

```{r}
dim(A1) <- c(4,3)
A1
```
Si nos damos cuenta, esto funcionaría si el orden de los elementos de la matríz es en el sentido de las columnas, ahora realizaremos la prueba con un vector que contenga los elementos en sentido de las filas:

```{r}
A2 <-c(1,2,3,2,4,6,3,6,9,4,8,12)
dim(A2) <- c(4,3)
A2
```
Si nos damos cuenta, no resulta la matriz deseada, por lo que esta función tiene sus limitaciones.

**2.2. Usando la función MATRIZ:**

Para crear una matriz mediante la función matrix y los números del vector A1 con 4 filas y 3 columnas, se usa la siguiente instrucción en R:

```{r}
A3 <- matrix(A1,nrow = 4,ncol = 3)
A3
```
En este caso, se ordena por defecto según las columnas. En cambio, si se desea a ingresar los números según las filas, se puede usar:

```{r}

A4 <- matrix(A1,nrow = 4,ncol = 3,byrow=TRUE)
A4
```
Como nos damos cuenta, la matriz no es el deseado, por lo que tenemos que usar el vector:

```{r}
A5 <- matrix(A2,nrow = 4,ncol = 3,byrow=TRUE)
A5
```

Para observar la clase en mediante la función "CLASS":

```{r}
class(A5)
```
Para observar los atributos de la matriz, es mediante la función "ATTRIBUTES"

```{r}
attributes(A5)
```
**2.3. Usando RBIND y CBIND:**

**RBIND:** Esta función crea una matriz en función a las filas,para eso primero creamos 4 vectores que tengan los elementos de cada fila:

```{r}
(p<-c(1,2,3))
(x<-c(2,4,6))
(y<-c(3,6,9))
(z<-c(4,8,12))
```
Por consiguiente, se crea la matriz A6 a partir de los vectores p,x,y,z utilizando la función RBIND. En R:

```{r}
A6 <- rbind(p,x,y,z)
A6
```
**CBIND:** Esta función crear una matriz en función a las columnas,para eso primero creamos 3 vectores que tengan los elementos de cada columna:

```{r}
(a <- c(1,2,3,4))
(b <- c(2,4,6,8))
(c <- c(3,6,9,12))
```
Por consiguiente, se crea la matriz A7 a partir de los vectores a,b,c utilizando la función CBIND. En R:

```{r}
A7 <- cbind(a,b,c)
A7
```



**3. Ingresar la matriz identidad de tamaño 3**
$I = \begin{pmatrix} 1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 1  \end{pmatrix}$

Una forma de crear una matriz identidad de tamaño 3 es creando un vector que contengan los elementos y usando cualquiera de las 3 formas de crear una matriz mencionadas anteriormente, se obtendrá el resultado:

```{r}
I1 <- matrix(c(1,0,0,0,1,0,0,0,1),nrow = 3,ncol = 3)
I1
```
Sin embargo, existe una función que te crea una matriz identidad de manera rápida, esta es la función diag():

```{r}
I2 <- diag(3)
I2
```
Si se desea colocar que en la diagonal aparezca un número específico, se realiza la siguiente estructura:

```{r}
I3 <- diag(1,3,3) #Primero se especifica el elemento que irá en la diagonal de la matriz, luego se especifica el número de filas y por último, el número de columnas.

I3
```



**4. Crea una función que cree una matriz nula ingresando las dimensiones**

Primero crearemos la matriz nula a partir de un número determinado de filas y columnas:

```{r}
I0 <-diag(0,5,5)
I0
```
Luego, crearemos la función que tenga como parámetros al número de filas y columnas:

```{r}
I0 <-function(a,b){
        I <-diag(0,a,b)
        return(I)
}
```

Por ejemplo, se pide crear una matriz nula que contegan 3 filas y 5 columnas:

```{r}
I0(3,5)
```
Otros ejemplos:

```{r}
I0(10,2)
```
```{r}
I0(2,5)
```
```{r}
I0(5,6)
```



**5. Modificar la matriz `diag(4)`, para que se parezca a la matriz B**
$B = \begin{pmatrix} 0 & 0 & 0 & 0 \\ 0 & 2 & 0 & 0 \\ 0 & 0 & 3 & 0 \\ 0 & 0 & 0 & 4  \end{pmatrix}$

Como primer paso, crearemos la matriz diag(4):

```{r}
B <- diag(4)
B
```
Luego realizaremos la manipulación de los elementos de dicha matriz:

```{r}
B[1,1]=0
B[2,2]=2
B[3,3]=3
B[4,4]=4
B
```



**6. Obtener la matriz transpuesta de A (ejercicio 2)**

Primero creamos la matriz de ejercicio por cualquiera de los métodos mencionados:

```{r}
A <- matrix(c(1,2,3,4,2,4,6,8,3,6,9,12),nrow = 4,ncol = 3)
A
```
Para calcular la matriz transpuesta es con la función t():

```{r}
t(A)
```



**7. Realizar las siguientes operaciones $A + B$, $A - B$, $3B$ y $AB$**

Sean las matrices A y B:
```{r}
A
```
```{r}
B
```
a) Suma o diferencia de matrices:

Es importante que ambas tengan las mismas dimensiones (mismo número de filas y mismo número de columnas):

```{r}
dim(A)==dim(B)
```
Como observamos, no se podria realizar una suma ni resta, ya que sus dimensiones no son iguales.

b) Multiplicación de una matriz por un escalar:

Para multiplicar una matriz por un número escalar es de la siguiente manera:

```{r}
3*B
```


c) Multiplicacion de matrices:

Para el producto o multiplicación matricial, se debe considerar que la primera matriz debe ser de dimensión 𝑚 × 𝑛 y la segunda, de 𝑛 × 𝑝. Es decir, la primera matriz debe tener el número de columnas igual al número de filas de la segunda matriz.

```{r}
dim(A)==dim(B)
```
Como observamos, no se podría realizar una multiplicación de matrices, dado que sus dimensiones no cumplen con el requisito del ítem anterior.



**8. Crea una función para calcular $P^6$**
$P = \begin{pmatrix} 1 & 2 & 3 \\ -2 & 4 & -2 \\ 1 & 0 & 1  \end{pmatrix}$

Primero crearemos la matriz P:

```{r}
P0 <-matrix(c(1,2,3,-2,4,-2,1,0,1),nrow = 3,ncol = 3,byrow = TRUE)
P0
```

**Potencia de una matriz:** 

En R base se puede realizar el cálculo de la potencia de una matriz usando el operador %*% :

Para calcular la matriz elevada al cuadrado es de la siguiente manera:

```{r}
P0 %*% P0
```
Pero resultaría tedioso realizar este procedimientos para potencias elevadas a números grandes. No existe una función en R base que calcule la potencia de una matriz, pero se puede realizar de dos maneras usando paquetes específicos:

**8.1. Usando el operador %^% del paquete EXPM:**

Primero instalamos el paquete *EXPM*:

```{r}
#install.packages("expm")
library(expm)
```
Luego se realiza la siguiente estructura:

```{r}
#Para calcular la matriz elevada al cuadrado:
P0 %^% 2
```
**8.2. Usando la función MATRIX.POWER del paquete MATRIXCALC:**

Primero instalamos el paquete *MATRIXCALC*:

```{r}
#install.packages("matrixcalc")
library(matrixcalc)
```

Luego se realiza la siguiente estructura:

```{r}
#Para calcular la matriz elevada al cuadrado:
matrix.power(P0,2)
```
*NOTA:La matriz debe ser cuadrada para calcular la potencia, ya que el número de columnas debe ser igual al número de filas para realizar los cálculos.*

Como último paso, crearemos una función que tenga como parámetro la potencia, utilizando cualquiera de los dos métodos mencionados anteriormente:

```{r}
P <- function(a){
        P0 <-matrix(c(1,2,3,-2,4,-2,1,0,1),nrow = 3,ncol = 3,byrow = TRUE)
        P1<-matrix.power(P0,a)
        return(P1)
}

P(6)
```



**9. Resolver el sistema de ecuaciones**

$3x - y + z  = -1\\9x - 2y + z = -9\\3x + y - 2z = -9$

El siguiente sistema se resolverá mediante operaciones fila. Al ordenar de forma matricial, se obtiene el siguiente resultado:

$$

\begin{pmatrix} A \end{pmatrix}

\begin{pmatrix} X \end{pmatrix}

=

\begin{pmatrix} B \end{pmatrix}

$$

$$

\begin{pmatrix} 
3 & -1 & 1  \\
9 & -2 & 1  \\
3 & 1 & -2  \\
\end{pmatrix}

\begin{pmatrix} 
x \\
y \\
z  \\
\end{pmatrix}
=
\begin{pmatrix} 
-1 \\
-9 \\
-9  \\
\end{pmatrix}
$$
Entonces como primer paso crearemos las matrices “A” y “B”:

Creación de la matriz A:

```{r}
A <- matrix(c(3,-1,1,9,-2,1,3,1,-2),3,3,byrow=T)
A
```
Creación de la matriz B:

```{r}
B <- matrix(c(-1,-9,-9),3,1)
B
```
Por consiguiente, concatenaremos las matrices “A” y “B” para obtener una nueva matriz denominada “AB”:

```{r}
AB <- cbind(A,B)
AB

```
Para hallar 𝑥, 𝑦, z es necesario que las tres primeras columnas formen una matriz identidad de 3 x 3. La última columna representará el vector de soluciones. Para llegar a dicha solución, se realizarán las operaciones en fila:

Como primera operación, se debe obtener la unidad en el primer elemento de la fila 1, para eso dividimos toda la primera fila entre 3:

```{r}
AB[1,] <- AB[1,]/3
AB
```
Aplicando la misma lógica, para obtener que toda la primera columna tenga la unidad, se debe dividir dichos elementos entre su misma cantidad:

```{r}
AB[2,] <- AB[2,]/9
AB

```
```{r}
AB[3,] <- AB[3,]/3
AB
```
El siguiente paso es obtener un valor “cero” en la posición [2,1], para esto restamos la segunda fila con la primera fila:

```{r}
AB[2,] <- AB[2,]-AB[1,]
AB
```
De igual forma, para obtener un valor “cero” en la posición [3,1], se tiene que restar tercera fila con la primera fila:

```{r}
AB[3,] <- AB[3,]-AB[1,]
AB
```
El siguiente procedimiento será determinar la unidad en la posición de la diagonal principal de la matriz para formar la matriz identidad, para eso dividimos la fila 2 entre el valor de la posición [2,2]:

```{r}
AB[2,] <- AB[2,]/AB[2,2]
AB
```
Aprovechando el valor de la posición [2,2], se puede utilizar para determinar el valor cero de la posición [3,2], ya que a la segunda fila se le puede multiplicar por dicha posición, con el objetivo de aplicarle una resta y obtener el valor 0 para la posición [3,2]:

```{r}
AB[3,] <- AB[3,]-(AB[2,]*AB[3,2])
AB
```
Luego dividimos la fila 3 entre el valor de la posición [3,3] para hallar la unidad de dicha posición:

```{r}
AB[3,] <- AB[3,]/AB[3,3]
AB
```
Luego para completar el valor “cero” de la posición [2,3], se aprovechará el valor de la unidad de la columna 2, sumando la fila 2 con el producto de la fila 3 con el número escalar 2, ya que se nota que la posición [2,3] es el doble de la posición [3,3]:

```{r}
AB[2,] <- AB[2,]+(AB[3,]*2)
AB

```
Aplicando el mismo procedimiento del paso anterior, se puede obtener el valor “cero” en la posición [1,2], aprovechando la unidad de la posición [2,2]:

```{r}
AB[1,] <- AB[1,]-(AB[2,]*AB[1,2])
AB
```
De la misma manera, se realiza el mismo procedimiento para calcular el valor "cero" de la posición [1,3]:

```{r}
AB[1,] <- AB[1,]-(AB[3,]*AB[1,3])
AB
```
*Se observa que las tres primeras columnas forman una matriz identidad 3 × 3. Esto quiere decir que la solución es 𝑥 = -1, 𝑦 = 2, 𝑧 = 4.* 

Para extraer la matriz de la solución del sistema, se creará una variable “X” que representará dicha matriz:

```{r}
X <- AB[,4]
X
```

**FUNCIÓN SOLVE**: Esta función genérica resuelve la ecuación a% *% x = b para x, donde b puede ser un vector o una matriz.

Para demostrar que la matriz 3 x 3 obtenida es invertible, se le debe aplicar la función “Solve”:

```{r}
XX <- solve(A,B)
XX

```

Como se puede observar, una manera fácil de resolver el sistema es mediante la función "Solve". Así mismo, ambos resultados son los mismos, por lo que se comprueba que las operaciones anteriores son correctas.



**10. Utilizando la ayuda de R, investigue para qué sirven las funciones `eigen()` y `det()`**

**Función eigen()**: Calcula autovalores y autovectores de matrices numéricas (dobles, enteras, lógicas) o complejas, para entenderlo, tenemos que saber sobre los Análisis de Comoponentes Principales (PCA)

*Análisis de Componentes Principales (PCA)*:es un método estadístico que permite simplificar la complejidad de espacios muestrales con muchas dimensiones a la vez que conserva su información.

El método de PCA permite por lo tanto “condensar” la información aportada por múltiples variables en solo unas pocas componentes. 

En el Álgebra Lineal se establecen 2 conceptos matemáticos que se aplican en la PCA:

**a) Eigenvector (Autovectores):** Los eigenvectors son un caso particular de multiplicación entre una matriz y un vector. El vector resultante de la multiplicación es un múltiplo entero del vector original. Los eigenvectors de una matriz son todos aquellos vectores que, al multiplicarlos por dicha matriz, resultan en el mismo vector o en un múltiplo entero del mismo.

**b) Eigenvalues (Autovalores):** Cuando se multiplica una matriz por alguno de sus eigenvectors se obtiene un múltiplo del vector original, es decir, el resultado es ese mismo vector multiplicado por un número. Al valor por el que se multiplica el eigenvector resultante se le conoce como eigenvalue. A todo eigenvector le corresponde un eigenvalue y viceversa.

Entonces la función eigen() calcula ambos y los almacena en una lista bajo el nombre de vectors y values. Los eigenvalues se devuelven en orden decreciente y los eigenvectors (estandarizados) se ordenan de izquierda a derecha acorde a sus eigenvalues asociados.

Por ejemplo tenemos la siguiente matriz:

```{r}
x<-cbind(c(6,9), c(4,2))
x
```
Para determinar los autovalores y autovectores, se aplica la función eigen():

```{r}
eigen(x)
```

[Enlace](https://rpubs.com/Joaquin_AR/287787#:~:text=La%20funci%C3%B3n%20eigen()%20calcula,acorde%20a%20sus%20eigenvalues%20asociados.)

**Función det():** Calcula el determinante de una matriz. El determinante es una función genérica que devuelve por separado el módulo del determinante, opcionalmente en la escala logarítmica, y el signo del determinante.

Para calcular el determinante de una matriz, estás deben ser cuadradas, es decir, deben tener el mismo número de filas y el mismo número de columnas.

Por ejemplo, tenemos la matriz siguiente:

```{r}
D<-matrix(c(1,5,3,2),2,2)
D
```
Calculando el determinante de la matriz cuadrada:

```{r}
det(D)
```



**11. Considerando las matrices**


$$

B= \begin{pmatrix} 
1 & 2 & 3 & 4 & 5 \\
2 & 4 & 6 & 8 & 10 \\
3 & 6 & 9 & 12 & 15 \\
4 & 8 & 12 & 16 & 20 \\
5 & 10 & 15 & 20 & 25 \\
6 & 12 & 18 & 24 & 30 \\
7 & 14 & 21 & 28 & 35 \\
8 & 16 & 24 & 32 & 40 \\
9 & 18 & 27 & 36 & 45 \\
10 & 20 & 30 & 40 & 50
\end{pmatrix}

A = \begin{pmatrix}
  0 & 1 & 0 & 1 & 0 \\
  1 & 0 & 1 & 0 & 1 \\
  0 & 1 & 0 & 1 & 0 \\
  0 & 1 & 0 & 0 & 1 \\
  1 & 0 & 1 & 1 & 0 \\
\end{pmatrix}

$$

Calcular $A \cdot B - A B^t$

**Creamos la matriz A:**

En esta ocasión se creará vectores de la misma longitud que contengan 0 y 1:

```{r}
(a <- c(0,0,0))
(b <- c(1,1,1))

```
El objetivo es combinar dichos vectores para crear 2 vectores que contengan los elementos de los originales intercalados.

```{r}
v1 <- as.numeric(rbind(a,b))
v1
```
```{r}
v2 <- as.numeric(rbind(b,a))
v2
```
Luego eliminaremos la posición "x" de los vectores, con el objetivo de obtener las filas de la matriz A:

Para obtener la primera y tercera fila de la matriz A:

```{r}
v3 <- v1[-6]
v3
```
Para obtener la segunda fila de la matriz A:

```{r}
v4 <- v2[-6]
v4

```

Para obtener la cuarta fila de la matriz A:

```{r}
v5 <- v1[-4]
v5
```
Para obtener la quinta fila de la matriz A:

```{r}
v6 <- v2[-4]
v6
```
Por último, unimos los vectores formados en una matriz:

```{r}
A <- matrix(c(v3,v4,v3,v5,v6),5,5,byrow=T)
A
```
**Creamos la matriz B:**

Se creará a partir de secuencias:

```{r}
B <- matrix(c(1:10,seq(2,20,by=2),seq(3,30,by=3),seq(4,40,by=4),seq(5,50,by=5)),10,5)
B

```


**Calculando: $A \cdot B - A B^t$**

Primero veremos si las dimensiones de cada matriz:

```{r}
dim(A)
dim(B)

```
*Para el producto o multiplicación matricial, se debe considerar que la primera matriz debe ser de dimensión 𝑚 × 𝑛 y la segunda, de 𝑛 × 𝑝. Es decir, la primera matriz debe tener el número de columnas igual al número de filas de la segunda matriz.En este caso no cumple con lo anterior, por lo que no se podrá realizar la operación pedida.*



**12. Considere**
$\hat\beta = (X^t \cdot X)^{-1} \cdot X^t \cdot Y$

Determine la matriz $\hat\beta$

$$

x=\begin{pmatrix}1 & 1\\ 1 & -1\\ 1 & 0\\ 1 & 1\\ 1 & 2\\ \end{pmatrix}

y = \begin{pmatrix}0\\0\\1\\1\\3\\\end{pmatrix}

$$

**Creamos la matriz x:**

```{r}
x <- matrix(c(rep(1,5),1,-1,0,1,2),5,2)
x
```
**Creamos la matriz y:**
```{r}
y <- matrix(c(0,0,1,1,3),5,1)
y

```
Calculando: $\hat\beta = (X^t \cdot X)^{-1} \cdot X^t \cdot Y$


```{r}
b <- ((solve(t(x)%*%x)%*%t(x))%*%y)
b

```


