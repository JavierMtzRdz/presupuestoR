u <- Sys.setlocale("LC_ALL", "es_ES.UTF-8")

#' Desagrupar por fila
#'
#' Esta función sirve para desagrupar un dataframe agrupado por
#' fila (dplyr::rowwise()).
#'
#' @param data dataframe agrupado por fila
#'
#' @return dataframe sin agrupación
#' @export
unrowwise_df <- function(data) {

  class(data) <- c("tbl_df", "data.frame")

  return(data)

}

#' Guardar base de datos en nuevo objeto
#'
#' Esta función sirve para guardar un objeto  tal como está en cierto punto
#' de una secuencia de pipas.
#'
#' @param data cualquier objeto
#' @param nombre string que se le asigna como nombre al objeto. Si no se
#' asigna ninguno, se guardará como obj_guardado .
#'
#' @return el mismo objeto y guarda un nuevo objeto con el nombre de bd_guardada
#' @export
guardar_obj <- function(data, nombre = "obj_guardado") {

  assign(nombre,
         data,
         envir = .GlobalEnv)

  return(data)

}

