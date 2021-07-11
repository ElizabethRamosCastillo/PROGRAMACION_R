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









