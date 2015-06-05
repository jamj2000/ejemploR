#' @title Ejemplo
#'
#' @description
#' Un paquete de ejemplo en R.
#'
#' @details
#' Analiza una matriz y muestra información descriptiva de sus columnas.
#' No tiene ninguna finalidad práctica, mas que servir de recurso
#' didáctico para los programadores de R en lengua castellana.
#' Esta función permite obtener una vista general de una matriz
#' con información descriptiva tal como número de valores nulos,
#' máximo, mínimo, media, desviación estándar y varianza.
#'
#' @param datos  Una matriz o data frame a ser analizado
#' @param na.rm  Valor lógico que indica si los valores nulos deben
#' ser eliminados al obtener la información estadística descriptiva
#' @return Un objeto de clase \code{"analizado"}, básicamente una lista con
#' información estadística descriptiva
#'
#' @author José Antonio Muñoz Jiménez
#' @seealso \code{\link{summary}}
#' @export
#' @examples
#' #creamos una matriz 10x5
#' m <- matrix(runif(50), 10, 5)
#' m                    # objeto de clase 'matrix'
#' class(m)
#'
#' #analizamos m
#' a <- analiza(m)
#' a                    # objeto de clase 'analizado'
#' class(a)
#'
#' is.analizado(m)      # FALSE # m NO es un objeto de clase 'analizado'
#' is.analizado(a)      # TRUE  # a SÍ es un objeto de clase 'analizado'
#'
analiza <- function(datos, na.rm = FALSE) {
  UseMethod("analiza", datos)
}


#' @S3method analiza default
analiza.default <- function(datos, na.rm = FALSE) {
  if (is.null(dim(datos)))
    stop("Los datos no son de clase matrix ni data.frame")
}


#' @S3method analiza matrix
analiza.matrix <- function(datos, na.rm = FALSE) {
  # obtenemos nombres de columnas
  nombre.columnas <- if (is.null(colnames(datos)))  1:ncol(datos)  else  colnames(datos)

  # Información estadística descriptiva
  resultado           <- .info(datos, na.rm)
  resultado$columnas  <- nombre.columnas

  # Salida
  class(resultado) = "analizado"
  resultado
}


#' @S3method analiza data.frame
analiza.data.frame <- function(datos, na.rm = FALSE) {
  # ¿Son todas las columnas numéricas?
  if (any(sapply(datos, class) != "numeric"))
    stop("Los datos contienen columnas no numéricas")

  # obtenemos nombres de columnas
  nombre.columnas <- if (is.null(colnames(datos)))  1:ncol(datos)  else  colnames(datos)

  # Información estadística descriptiva
  resultado           <- .info(datos, na.rm)
  resultado$columnas  <- nombre.columnas

  # Salida
  class(resultado) = "analizado"
  resultado
}


#' @S3method print analizado
print.analizado <- function(x, ...)
{
  # Mostramos análisis en formato data frame
  informacion  <- data.frame(
    nulos       = x$nulos,
    maximo      = x$maximo,
    minimo      = x$minimo,
    media       = x$media,
    desviacion  = x$desviacion,
    varianza    = x$varianza,
    row.names   = x$columnas)

  print(round(informacion, 4))
  invisible(x)
}


#' @rdname analiza
#' @param x un objeto a comprobar
#' @export
is.analizado <- function(x) is(x, "analizado")


# Información estadística descriptiva
.info <- function(datos, na.rm = FALSE) {
  list(nulos      = apply(datos, 2, function(x) sum (is.na(x))),
       maximo     = apply(datos, 2, function(x) max (x, na.rm=na.rm)),
       minimo     = apply(datos, 2, function(x) min (x, na.rm=na.rm)),
       media      = apply(datos, 2, function(x) mean(x, na.rm=na.rm)),
       desviacion = apply(datos, 2, function(x) sd  (x, na.rm=na.rm)),
       varianza   = apply(datos, 2, function(x) var (x, na.rm=na.rm))
  )
}

