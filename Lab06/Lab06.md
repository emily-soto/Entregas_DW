Laboratorio 06
================
Emily Soto
20/09/2021

### Preguntas

2.  Genere una expresión regular que valide si un archivo es de tipo
    .pdf o jpg.

<!-- -->

1.  Ejemplo1.pdf, prueba2.PDF, respuestas\_del\_examen.jpg, amor.JPG

``` r
prueba_formato=c("Ejemplo1.pdf", "prueba2.PDF", "respuestas_del_examen.jpg", "amor.JPG", ".JPGHola", "hola.r", "PJPG")
str_detect(prueba_formato, "[.](pdf|jpg|PDF|JPG)$")
```

    ## [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE

``` r
prueba_formato[which(str_detect(prueba_formato, "[.](pdf|jpg|PDF|JPG)$")==TRUE)]
```

    ## [1] "Ejemplo1.pdf"              "prueba2.PDF"              
    ## [3] "respuestas_del_examen.jpg" "amor.JPG"

3.  Genere una expresión regular para validar contraseñas de correo. Una
    contraseña de correo debe contener por lo menos 8 caracteres, una
    letra mayúscula y un carácter especial.

``` r
prueba_contra=c("Hio12356$", "ghE123&4", "dfh1")
str_detect(prueba_contra,"^(?=.*[#?!@$%^&*-])(?=.*[A-Z]).{8,}$")
```

    ## [1]  TRUE  TRUE FALSE

``` r
prueba_contra[which(str_detect(prueba_contra,"^(?=.*[#?!@$%^&*-])(?=.*[A-Z]).{8,}$")==TRUE)]
```

    ## [1] "Hio12356$" "ghE123&4"

5.  Cree una expresión regular que encuentre todas las palabras de la
    primera línea, pero ninguna de la segunda.

<!-- -->

1.  pit, spot, spate, slap two, respite P\[a-i\]t//
2.  pt,Pot,peat,part

``` r
# ((.)?p[a-o]t)|((.)?p t)
palabras=c("pit", "spot", "spate", "slap two", "respite", "pt","Pot","peat","part")
str_detect(palabras, "((.)?p[a-o]t)|((.)?p t)")
```

    ## [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE

``` r
palabras[which(str_detect(palabras, "((.)?p[a-o]t)|((.)?p t)")==TRUE)]
```

    ## [1] "pit"      "spot"     "spate"    "slap two" "respite"

6.  Cree una expresión regular para obtener los números telefónicos de
    Guatemala. Estos pueden contener al inicio +502 o 502, pueden estar
    separados por un espacio en blanco o un guión o juntos. Notar que
    los números telefónicos pueden empezar únicamente con 4,5,6 o 2.

``` r
numeros_cel=c("5024212-4345", "52123232","+5022456-8787", "3567-8989", "4567-8989", "6567 8989")
str_detect(numeros_cel,"^(\\+502|502)?(4|5|6|2)(\\d{3})\\-?\\s?(\\d{4})$")
```

    ## [1]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE

``` r
numeros_cel[which(str_detect(numeros_cel,"^(\\+502|502)?(4|5|6|2)(\\d{3})\\-?\\s?(\\d{4})$")==TRUE)]
```

    ## [1] "5024212-4345"  "52123232"      "+5022456-8787" "4567-8989"    
    ## [5] "6567 8989"

7.  Genere una expresión regular que sea capaz de obtener correos de la
    UFM

``` r
correos=c("esovel@ufm.edu","rob@ufm.edu","j@ufm.edu","emily34@ufm.edu","Emilysoto@ufm.edu")
str_detect(correos,"[a-zA-Z0-9]+(@ufm\\.edu)$")
```

    ## [1] TRUE TRUE TRUE TRUE TRUE

``` r
correos[which(str_detect(correos,"[a-zA-Z0-9]+(@ufm\\.edu)$")==TRUE)]
```

    ## [1] "esovel@ufm.edu"    "rob@ufm.edu"       "j@ufm.edu"        
    ## [4] "emily34@ufm.edu"   "Emilysoto@ufm.edu"

4.  Cree una expresión regular para validar un numero de carnet de la
    Universidad Galileo, por ejemplo 19002324 donde los primeros dos
    dígitos representan el año en el que el alumno se inscribió los
    cuales pueden variar desde el 01 (año 2001) hasta el 30 (año 2030).
    Los siguientes dos dígitos son cero (00) los cuales van por default
    y los últimos cuatro dígitos son un número que va desde el 1110
    hasta el 8970. Para dar su respuesta utilice la notación de
    expresiones regulares.

``` r
carne=c("19002324","01008970","30008969", "29008971","00008970","02001110","03001109")
str_detect(carne,"^((0+[1-9])|[12][0-9]|30)+00+([1-8][1-9][1-6][0-9]|8970)$")
```

    ## [1]  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE

``` r
carne[which(str_detect(carne,"^((0+[1-9])|[12][0-9]|30)+00+([1-8][1-9][1-6][0-9]|8970)$")==TRUE)]
```

    ## [1] "19002324" "01008970" "30008969" "02001110"
