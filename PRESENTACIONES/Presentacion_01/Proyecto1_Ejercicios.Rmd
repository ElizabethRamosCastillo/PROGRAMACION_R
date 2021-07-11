---
title: "Programando con R"

author:
 - name:  "Ceras Robles, Jose Alexander (código : 17160034)"
 - name:  "Condor Melo,Deysi (código : 17160180)"
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
