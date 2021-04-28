#Julian Ortiz Garcia
#r Studio
#https://bookdown.org/jboscomendoza/r-principiantes4/if-else.html

# Creamos una muestra de 20 observaciones del 1 al 100 en el que se pueden repetir hasta 2 observaciones
(muestra <- sample(1:100, 20, 2))


## Creamos una variable indicando la medida de tendencia central que queremos calcular
centralizacion <- "mediana"

#Creamos un algoritmo para calcular la tendencia central que deseemos
#decimos si la variable es media calcule la media
if (centralizacion == "media") {
   #aca de sacamos la media a la muestra con mean y la guardamos
    media = mean(muestra)
    #ya aca mostramos el mensaje con la media como caracter
    message("La media es ", as.character(media))
    #y aca decimos que si no es el anterio entonces sino la variable es igua a mediana
} else if (centralizacion == "mediana") {
   #ya aca hacemos la mediana con la median y de sacamos la mediana a muestra
    mediana = median(muestra)
    #ya aca mostramos el mensaje con la mediana como caracter
    message("La mediana es ", as.character(mediana))
    #ya si la variable no es ninguna de la anterio entonces podemos decir que es moda
} else if (centralizacion == "moda") {
    #aca ya hacemos la moda de la muestra con el metodo de mfv
    moda = mlv(muestra, method = "mfv")
    #ya aca mostramos el mensaje con la moda como caracter
    message("La moda es ", as.character(moda))
    #pero si la variable no es ninguno de los anterios entonces podemos un else y decimos algo
} else {
  ~#ya aca mostramos un mensaje que diga que no cumple con nada 
    message("Este algoritmo sola calcula la media,
          mediana, moda")
}

# Calculamos la media de la muestra
#Esta función permite ejecutar una de entre varias acciones en función del valor de una expresión. Es una alternativa a los if-else anidados cuando se compara la misma expresión con diferentes valores.

#El caso más común toma como primer argumento una expresión que devuelve un string, seguido por varios argumentos con nombre que proporcionan el resultado cuando el nombre coincida con el primer argumento. Cuando se encuentra la primera coincidencia, se ejecuta el bloque de instrucciones y se sale de función. Si no se encuentra ninguna coincidencia con ningún valor, se ejecuta el bloque de sentencias del argumento por defecto.
modo_switch<-(switch("media", media = mean(muestra), mediana = median(muestra), moda = mlv(muestra, 
    method = "mfv", "Solo se puede calcular la media, mediana y moda")))
 #mostramos el la media con el Selección Múltiple con switch
print(modo_switch)



