u <- Sys.setlocale("LC_ALL", "es_ES.UTF-8")

#' Indexar o reindexar
#'
#' Esa función genera un índice a partir de cierto periodo base.
#'
#' @param .x Base de datos tidy
#' @param col_from Columna que se va a indexar
#' @param col_to Colúmna que se va a generar con el resultado
#' @param ... Componentes a filtrar
#' @param n_base Base del índice
#'
#' @importFrom magrittr %>%
#' @return Regresa la base de datos con la colúmna con el índice
#' @export
indexing <- function(.x,
                     col_from,
                     col_to = index,
                     ...,
                     n_base = 100){

  cf <- rlang::enquo(col_from)

  ct <- rlang::enquo(col_to)

  group_vars <- rlang::enquos(...)


  rows <- .x %>%
    dplyr::mutate(num = sum(ifelse(!!!group_vars,
                                   1, 0))) %>%
    dplyr::filter(num > 1) %>%
    nrow()

  if (rows > 1) {warning("Se está usando como referencia más de una fila")}

  .x <- .x %>%
    dplyr::mutate(!!ct := (!!cf*n_base)/ifelse(sum(ifelse(!!!group_vars ,
                                                         1, 0)) == 1,
                                              sum(ifelse(!!!group_vars ,
                                                         !!cf, 0)),
                                              sum(ifelse(!!!group_vars ,
                                                         !!cf, 0))/sum(ifelse(!!!group_vars ,
                                                                             1, 0))))



  return(.x)

}

#' Generar índice combaso en cambio entre periodos
#'
#' Esa función genera un índice a partir de cambios de un periodo (ejemplo,
#' de un mes en estadisticas mensuales o de un trimestre en estadisticas
#' trimestrales)
#'
#' @param .x Base de datos tidy
#' @param col_from Columna que contiene la tasa periodica de cambio (1 = 100%)
#' @param col_to Colúmna que se va a generar con el índice
#' @param ... Componentes para seleccionar la base 100. Tiene que ser una obs.
#' @param n_base Base del índice
#'
#' @importFrom magrittr %>%
#' @return Regresa la base de datos con la colúmna con el índice
#' @export
gen_index <- function(.x,
                      col_from,
                      col_to = index,
                      ...,
                      n_base = 100){

  cf <- rlang::enquo(col_from)

  ct <- rlang::enquo(col_to)

  group_vars <- rlang::enquos(...)

  .x <- .x %>%
    dplyr::mutate(!!ct := ifelse(!!!group_vars,
                                 n_base, NA))

  while (.x %>%
         dplyr::filter(!is.na(dplyr::lag(!!cf)) |
                !is.na(dplyr::lead(!!cf))) %>%
         dplyr::pull(!!ct) %>%
         is.na() %>%
         any()) {


    .x <- .x %>%
      dplyr::mutate(!!ct := ifelse(!is.na(!!ct),
                            !!ct, (dplyr::lag(!!ct)/1)*(1+!!cf)),
             !!ct := ifelse(!is.na(!!ct),
                            !!ct, (dplyr::lead(!!ct)*1)/(1+dplyr::lead(!!cf))))
  }
  return(.x)
}

#' Abrevia una entidad
#'
#' Transforma un nombre propio de entidad Mexicana en abreviaciones no ambiguas.
#' Remueve todo caracter especial o acentuado y luego intenta. Si la entidad no
#' se encuentra regresa NA. Toda referencia a la Republica Federal o Nacion o
#' Nacional se transformará en "NAC". Oaxaca es un caso especial: cualquier
#' mencion de "oaxaca" como match de regex en el nombre se identificará con
#' "OAX".
#'
#' @param entidad Nombre de una entidad
#' @return un vector de caracteres de tamaño 3 de entidades de México no
#' ambiguas
#' @export
entidad_to_abr2 <- function(entidad) {
  y <- stringi::stri_trans_general(entidad,"latin-ascii" )
  y <- stringi::stri_replace_all(y, regex = "[:punct:]", "")
  y <- stringr::str_to_title(y)

  case_when(
    y == "Tabasco" ~ "Tab.",
    y == "Nayarit" ~ "Nay.",
    y == "Durango" ~ "Dgo.",
    stringi::stri_detect(y, fixed ="oaxaca", case_insensitive = T) ~ "Oax.",
    y == "Oaxaca" ~ "Oax.",
    y == "Mexico" ~ "Mex.",
    y == "Edomex" ~ "Mex.",
    y == "Estado De Mexico" ~ "Mex.",
    y == "Campeche" ~ "Camp.",
    y == "Zacatecas" ~ "Zac.",
    y == "Quintana Roo" ~ "Q. Roo",
    y == "Sonora" ~ "Son.",
    y == "Cdmx" ~ "CdMx",
    y == "Distrito Federal" ~ "CdMx",
    y == "Ciudad De Mexico" ~ "CdMx",
    y == "Veracruz De Ignacio De La Llave" ~ "Ver.",
    y == "Veracruz" ~ "Ver.",
    y == "Baja California Sur" ~ "BCS",
    y == "Morelos" ~ "Mor.",
    y == "Guanajuato" ~ "Gto.",
    y == "Jalisco" ~ "Jal.",
    y == "Tamaulipas" ~ "Tamps.",
    y == "Guerrero" ~ "Gro.",
    y == "Baja California" ~ "BC",
    y == "Nuevo Leon" ~ "NL",
    y == "Chihuahua" ~ "Chih.",
    y == "San Luis Potosi" ~ "SLP",
    y == "Tlaxcala" ~ "Tlax.",
    y == "Yucatan" ~ "Yuc.",
    y == "Puebla" ~ "Pue.",
    y == "Coahuila De Zaragoza" ~ "Coah.",
    y == "Coahuila" ~ "Coah.",
    y == "Colima" ~ "Col.",
    y == "Hidalgo" ~ "Hgo.",
    y == "Queretaro" ~ "Qro.",
    y == "Sinaloa" ~ "Sin.",
    y == "Chiapas" ~ "Chis.",
    y == "Michoacan De Ocampo" ~ "Mich.",
    y == "Michoacan" ~ "Mich.",
    y == "Aguascalientes" ~ "Ags.",
    y == "Nacional" ~ "Nac.",
    y == "Nacion" ~ "Nac.",
    y == "Republica" ~ "Nac.",
    y == "Republica Federal" ~ "Nac.",
    y == "Estados Unidos Mexicanos" ~ "Nac.",
    T ~ y
  )
}


#' Generar generar valor anterior para conectar líneas
#'
#' Esa función duplica el último valor de una serie integrando la categoría
#' de la siguiente serie
#'
#' @param .x Base de datos tidy
#' @param variable Variable en la que se basa el cambio
#' @param date Variable de fechas
#'
#' @importFrom magrittr %>%
#' @return Regresa la base de datos con la colúmna con el índice
#' @export
conect_value <- function(.x,
                         variable,
                         date = NA) {

  variable_e <- enquo(variable)

  date_e <- enquo(date)

  .x <- .x %>%
    arrange(!!date_e, .by_group = T) %>%
    mutate(n = ifelse(!!variable_e != lag(!!variable_e),
                      2, 1),
           n = lead(n),
           n = replace_na(n, 1)) %>%
    uncount(n, .id = "id") %>%
    mutate(!!variable_e := ifelse(id == 2,
                                  lead(!!variable_e),
                                  !!variable_e)) %>%
    select(-id)

  return(.x)
}
