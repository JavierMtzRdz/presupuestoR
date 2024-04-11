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

#' Indexing a Series
#'
#' The `indexing()` function enables the creation of an index from a numeric variable,
#' using specified observations as reference points.
#'
#' @param .x Tidy dataset.
#' @param col_from Column containing the numeric variable to be indexed.
#' @param col_to Column to store the result (default is 'index').
#' @param ... Components to select the base observation(s).
#' @param n_base Base value for the index (default is 100).
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @importFrom rlang :=
#' @return Returns the tidy dataset with the indexed column
#' @examples
#' # Example 1: ---------------------------------------------------------------------
#' # In this example, the function generated a time series and established an
#' # index using the `values` column. The base observation is set at
#' # `2023-01-01`.
#'
#' ## Load packages
#' library(dplyr)
#' library(ggplot2)
#'
#' ## Generating toy dataset
#' set.seed(545)
#'
#' df <- data.frame(date = seq(as.Date("2023-01-01"),
#'                         as.Date("2025-12-01"),
#'                         "month"),
#'              values = runif(12*3, 50, 150)*
#'                (2 + cumsum(runif(12*3, 0, 0.15))))
#'
#' head(df)
#'
#' #> # A tibble: 6 × 2
#' #>   date       values
#' #>   <date>      <dbl>
#' #> 1 2023-01-01   283.
#' #> 2 2023-02-01   129.
#' #> 3 2023-03-01   235.
#' #> 4 2023-04-01   298.
#' #> 5 2023-05-01   189.
#' #> 6 2023-06-01   354.
#'
#' ## Index generation
#'
#' df2 <- indexing(df,
#'                 values,
#'                 date == as.Date("2023-01-01"))
#'
#' head(df2)
#'
#' #> # A tibble: 6 × 3
#' #>   date       values index
#' #>   <date>      <dbl> <dbl>
#' #> 1 2023-01-01   283. 100
#' #> 2 2023-02-01   129.  45.6
#' #> 3 2023-03-01   235.  83.2
#' #> 4 2023-04-01   298. 105.
#' #> 5 2023-05-01   189.  66.8
#' #> 6 2023-06-01   354. 125.
#'
#' # Comparing original variable vs. indexed variable.
#'
#' df2 %>%
#'   ggplot(aes(x = date)) +
#'   geom_hline(yintercept = 100,
#'              linetype = "dashed") +
#'   geom_line(aes(y = values,
#'                 color = "values")) +
#'   geom_line(aes(y = index,
#'                 color = "index")) +
#'   theme_minimal()
#'
#' # Example 3: ---------------------------------------------------------------------
#' # Using the toy dataset from earlier, this code chunk generates an indexed
#' # variable taking the average values of multiple observations as reference.
#' # Specifically, it computes the average for the year 2023. A warning is issued
#' # in case this operation was not intended.
#'
#' ## Index generation
#'
#' df2 <- indexing(df,
#'                 values,
#'                 lubridate::year(date) == 2023)
#'
#' #> Warning: More than one row is being used as a reference.
#'
#' head(df2)
#'
#' ## Comparing original variable vs. indexed variable.
#'
#' #> # A tibble: 6 × 3
#' #>   date       values index
#' #>   <date>      <dbl> <dbl>
#' #> 1 2023-01-01   283. 116.
#' #> 2 2023-02-01   129.  52.7
#' #> 3 2023-03-01   235.  96.3
#' #> 4 2023-04-01   298. 122.
#' #> 5 2023-05-01   189.  77.3
#' #> 6 2023-06-01   354. 145.
#'
#' # Comparing original variable vs. indexed variable.
#' df2 %>%
#'   ggplot(aes(x = date)) +
#'   geom_hline(yintercept = 100,
#'              linetype = "dashed") +
#'   geom_line(aes(y = values,
#'                 color = "values")) +
#'   geom_line(aes(y = index,
#'                 color = "index")) +
#'   theme_minimal()
#'
#' # Example 3: ---------------------------------------------------------------------
#' # In this section, we indexed the GDP per capita for Oceania countries taking
#' # the year 1952 as the base.
#'
#'
#' ## Grouped index generation
#'
#' library(gapminder)
#'
#' idx_gap <- gapminder::gapminder %>%
#'   filter(continent == "Oceania") %>%
#'   group_by(country) %>%
#'   indexing(gdpPercap,
#'            year == 1952)
#'
#' head(idx_gap)
#'
#' #> # A tibble: 6 × 7
#' #> # Groups:   country [1]
#' #>   country   continent  year lifeExp      pop gdpPercap index
#' #>   <fct>     <fct>     <int>   <dbl>    <int>     <dbl> <dbl>
#' #> 1 Australia Oceania    1952    69.1  8691212    10040.  100
#' #> 2 Australia Oceania    1957    70.3  9712569    10950.  109.
#' #> 3 Australia Oceania    1962    70.9 10794968    12217.  122.
#' #> 4 Australia Oceania    1967    71.1 11872264    14526.  145.
#' #> 5 Australia Oceania    1972    71.9 13177000    16789.  167.
#' #> 6 Australia Oceania    1977    73.5 14074100    18334.  183.
#'
#' ## Ploting generated index
#' idx_gap %>%
#'   ggplot2::ggplot(aes(x = year,
#'              y = index,
#'              color = country)) +
#'   geom_line() +
#'   theme_minimal()
#'
#' @export
indexing <- function (.x, col_from,
                      ...,
                      col_to = index,
                      n_base = 100)
{
  cf <- rlang::enquo(col_from)
  ct <- rlang::enquo(col_to)
  group_vars <- rlang::enquos(...)
  
  # Check if the specified column contains numeric data
  if (!is.numeric(.x %>% dplyr::pull(!!cf))) {
    stop(paste0("'", dplyr::as_label(cf),
                "' is not a numeric variable."))
  }
  
  # Determine the number of rows in the base dataset
  rows <- .x %>%
    dplyr::filter(!!!group_vars) %>%
    dplyr::mutate(rows = dplyr::n()) %>%
    dplyr::pull(rows) %>%
    ifelse(identical(., integer(0)),
           ., max(.))
  
  # Check if a reference row is found
  if (is.na(rows)) stop("Reference not found.")
  
  # Warn if more than one row is being used as a reference
  if (rows > 1) warning("More than one row is being used as a reference.")
  
  # Calculate the index using the specified formula
  .x <- .x %>%
    dplyr::mutate(!!ct := (!!cf * n_base) /
                    ifelse(
                      sum(ifelse(!!!group_vars, 1, 0)) == 1,
                      sum(ifelse(!!!group_vars, !!cf, 0)),
                      sum(ifelse(!!!group_vars, !!cf, 0)) /
                        sum(ifelse(!!!group_vars, 1, 0))
                    ))
  
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
#' @param cve_ent Clave de entidad
#' @return un vector de caracteres de tamaño 3 de entidades de México no
#' ambiguas
#' @export
cve_to_ent <- function(cve_ent) {
  y <- as.numeric(cve_ent)
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
