#' Generar índice con variación entre periodos
#'
#' Esa función genera un índice a partir de cambios de un periodo
#' (ejemplo, de un mes en estadisticas mensuales o de un trimestre
#' en estadisticas trimestrales)
#'
#' @param .x Base de datos tidy
#' @param col_from Columna que contiene la tasa periodica de cambio (1 = 100)
#' @param col_to Colúmna que se va a generar con el índice
#' @param ... Componentes para seleccionar la base 100. Tiene que ser una obs.
#' @param n_base Base del índice
#'
#' @importFrom rlang :=
#' @importFrom rlang !!
#' @return Regresa la base de datos con la colúmna con el índice
#' @export
variacion_to_index <- function(.x,
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
#' @importFrom rlang !!
#' @importFrom rlang :=
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

  if (rows > 1) {warning("Se est\u00e1 usando como referencia m\u00e1s de una fila")}

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

  dplyr::case_when(
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


#' Clave INEGI a entidad
#'
#' Transforma las claves de INEGI correspondientes a cada entidad Mexicana en
#' nus nombres cortos. Si la clave no se encuentra regresa NA. Toda referencia a 
#' la Republica Federal o Nacion o Nacional se transformará en "Nacional" y
#' corresponderá a la clave 00..
#'
#' @param entidad Nombre de una entidad
#' @return un vector de caracteres de tamaño 3 de entidades de México no
#' ambiguas
#' @export
entidad_to_abr2 <- function(entidad) {
  y <- as.numeric(entidad,"latin-ascii" )
  y <- sprintf("%02d", y)
  
  dplyr::case_when(
    y == "00" ~ "Nacional",
    y == "01" ~ "Aguascalientes",
    y == "02" ~ "Baja California",
    y == "03" ~ "Baja California Sur",
    y == "04" ~ "Campeche",
    y == "05" ~ "Coahuila",
    y == "06" ~ "Colima",
    y == "07" ~ "Chiapas",
    y == "08" ~ "Chihuahua",
    y == "09" ~ "Ciudad de M\u00e9xico",
    y == "10" ~ "Durango",
    y == "11" ~ "Guanajuato",
    y == "12" ~ "Guerrero",
    y == "13" ~ "Hidalgo",
    y == "14" ~ "Jalisco",
    y == "15" ~ "M\u00e9xico",
    y == "16" ~ "Michoac\u00e1n",
    y == "17" ~ "Morelos",
    y == "18" ~ "Nayarit",
    y == "19" ~ "Nuevo Le\u00f3n",
    y == "20" ~ "Oaxaca",
    y == "21" ~ "Puebla",
    y == "22" ~ "Quer\u00e9taro",
    y == "23" ~ "Quintana Roo",
    y == "24" ~ "San Luis Potos\u00ed",
    y == "25" ~ "Sinaloa",
    y == "26" ~ "Sonora",
    y == "27" ~ "Tabasco",
    y == "28" ~ "Tamaulipas",
    y == "29" ~ "Tlaxcala",
    y == "30" ~ "Veracruz",
    y == "31" ~ "Yucat\u00e1n",
    y == "32" ~ "Zacatecas",
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
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @return Regresa la base de datos con la colúmna con el índice
#' @export
conect_value <- function(.x,
                         variable,
                         date = NA) {

  variable_e <- rlang::enquo(variable)

  date_e <- rlang::enquo(date)

  .x <- .x %>%
    dplyr::arrange(!!date_e, .by_group = T) %>%
    dplyr::mutate(n = ifelse(!!variable_e != dplyr::lag(!!variable_e),
                      2, 1),
           n = dplyr::lead(n),
           n = tidyr::replace_na(n, 1)) %>%
    tidyr::uncount(n, .id = "id") %>%
    dplyr::mutate(!!variable_e := ifelse(id == 2,
                                         dplyr::lead(!!variable_e),
                                  !!variable_e)) %>%
    dplyr::select(-id)

  return(.x)
}
