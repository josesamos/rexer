
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rexer

<!-- badges: start -->
<!-- badges: end -->

The goal of `rexer` is to …

Evaluación continua en clase, los estudiantes están próximos. Generar
exámenes similares pero distintos de manera que cada estudiante deba
centrarse en la resolución de su examen / que el examen de alguien
próximo no le sirva.

Preguntas similares con componentes cambiantes.

Ayuda para la corrección.

Selección de preguntas aleatorias.

Poder tener una base de datos de preguntas y seleccionar

Podrían considerarse como *string exercises* del paquete `exams` pero
con el componente aleatorio.

Si se indican varias respuestas, se selecciona una de las respuestas y
el resto de opciones que se eligen se corresponden a la respuesta
seleccionada -\> debería haber el mismo número de opciones o un divisor
del él para que sea más fácil de entender.

## Installation

You can install the development version of rexer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("josesamos/rexer")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rexer)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
# summary(cars)
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
