#' Deflactar montos INEGI
#'
#' Esa función deflacta montos a partir del INPC.
#'
#' @param monto monto que se quiere deflactar
#' @param mes mes a comparar. En caso de ser acumulado sin que comience en enero,
#' tiene que ponerse una lista con el mes de inicio y fin.
#' @param year_monto año de origen del monto
#' @param year_out año del precio al que se quiere deflactar
#' @param acumulado variable lógica para determinar si se quiere acumulado
#'
#' @importFrom magrittr %>%
#' @return regresa un vector de los montos deflactados
#' @export
deflactar_inpc <- function(monto, year_monto, year_out,
                           mes = 12, acumulado = T) {

  if (!exists("inpc_bd") ||
      !(c("fecha") %in% colnames(inpc_bd)) ||
      !(c("mes") %in% colnames(inpc_bd)) ||
      !(c("year") %in% colnames(inpc_bd)) ||
      !(c("inpc") %in% colnames(inpc_bd))) {

    temp <- tempfile(fileext = ".xls")

    inpc_ur <- "http://www.banxico.org.mx/SieInternet/consultasieiqy?series=SP1&locale=en"

    utils::download.file(inpc_ur, destfile = temp, mode = 'wb')

    assign("inpc_bd",
           XML::htmlParse(temp) %>%
             XML::getNodeSet('//table') %>%
             .[[6]] %>%
             XML::readHTMLTable() %>%
             dplyr::tibble() %>%
             janitor::clean_names() %>%
             tidyr::separate(date, c("mes", "year")) %>%
             dplyr::transmute(fecha = paste0(mes, "/", year),
                              mes = as.numeric(mes),
                              year = as.numeric(year),
                              inpc = as.numeric(sp1)),
           envir = as.environment("package:presupuestoR"))

  }

  if(length(mes) == 1) {
    mes_dato <- as.numeric(mes)

    fecha_monto <- paste0(mes_dato, "/", year_monto)

    fecha_out <- paste0(mes_dato, "/", year_out)
  } else {
    mes_dato1 <- as.numeric(mes[1])
    mes_dato2 <- as.numeric(mes[2])

    fecha_monto1 <- paste0(mes_dato1, "/", year_monto)
    fecha_monto2 <- paste0(mes_dato2, "/", year_monto)
    fecha_monto <- max(c(fecha_monto1, fecha_monto2))

    fecha_out1 <- paste0(mes_dato1, "/", year_out)
    fecha_out2 <- paste0(mes_dato2, "/", year_out)
    fecha_out <- max(c(fecha_out1, fecha_out2))
  }

  if (!all(c(fecha_monto, fecha_out) %in% inpc_bd$fecha)) {
    warning("Fechas no disponibles")

    return(monto)
  }


  if(acumulado) {

    inpc <- inpc_bd %>%
      {if(length(mes) == 1) {
        dplyr::filter(.,
                      mes <= mes_dato)
      } else {
        dplyr::filter(.,
                      mes <= max(c(mes_dato1, mes_dato2)),
                      mes >= min(c(mes_dato1, mes_dato2)))
      }} %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(inpc = mean(inpc)) %>%
      dplyr::rename(fecha = year)

    fecha_out <- year_out

    fecha_monto <- year_monto

  } else {
    inpc <- inpc_bd
  }

  deflactor_out <- try(purrr::map_dbl(fecha_out,
                                      function(x) as.numeric(inpc[x == inpc$fecha,]$inpc)),
                       silent = T)

  if (class(deflactor_out) == "try-error") {

    warning("Error en el a\\u00f1o del monto deflactado. La cifra no fue deflactada.")

    return(monto)
  }

  deflactor_monto <- try(purrr::map_dbl(fecha_monto,
                                        function(x) as.numeric(inpc[x == inpc$fecha,]$inpc)),
                         silent = T)

  if (class(deflactor_monto) == "try-error") {
    warning("Error en el a\\u00f1o al que se va a deflactar. La cifra no fue deflactada.")

    return(monto)
  }

  monto_deflactado <- (monto * deflactor_out) / deflactor_monto

  return(monto_deflactado)

}

#' Obtener datos de Banxico
#'
#' Esa función ayuda a extraer los datos de banxico. Está basada enteramente
#' en la librería de flores89/banxicoR, pero corrige un error que no permitía
#' descargar la información.
#'
#' @param series clave de la serio
#' @param metadata indicador lógico sobre la inclusión metadara
#' @param verbose indicador lógico para señalar si muestra los pasos
#' @param mask comando lógico si usar el nombre de la serie o values como columna
#'
#' @return regresa un vector de los montos deflactados
#' @export

banxico_series2 <- function (series, metadata = FALSE, verbose = FALSE, mask = FALSE) {
  # series = "SF331451"
  # metadata = FALSE
  # verbose = FALSE
  # mask = FALSE

  s <- as.character(series)
  s <- paste0("http://www.banxico.org.mx/SieInternet/consultasieiqy?series=",
              series, "&locale=en")
  h <- xml2::read_html(x = s)
  d <- rvest::html_nodes(x = h, css = "table")
  if (verbose) {
    print(paste0("Data series: ", series, " downloaded"))
  }
  mtd <- stringr::str_trim(rvest::html_text(rvest::html_nodes(rvest::html_nodes(rvest::html_nodes(d,
                                                                                                  "table"), "table"), "td")))
  mtd_head <- stringr::str_trim(rvest::html_text(rvest::html_nodes(rvest::html_nodes(rvest::html_nodes(rvest::html_nodes(d,
                                                                                                                         "table"), "table"), "td"), "a")))
  frequency <- banxicoR::banxico_parsemeta(mtd, "frequency")
  if (verbose) {
    print(paste0("Data series in ", frequency, " frequency"))
  }
  n <- length(d)
  e <- rvest::html_table(x = d[n], fill = TRUE, header = TRUE)
  e <- e[[1]]
  names(e) <- gsub(pattern = "FECHA", replacement = "Dates",
                   x = names(e))
  names(e) <- gsub(pattern = "DATE", replacement = "Dates",
                   x = names(e))
  if (mask) {
    names(e)[names(e) != "Dates"] <- "Values"
  }
  if (verbose) {
    print(paste0("Parsing data with ", nrow(e), " rows"))
  }
  e <- dplyr::mutate_at(e, dplyr::vars(dplyr::starts_with("S")), ~stringr::str_remove_all(., ","))
  e <- dplyr::mutate_at(e, dplyr::vars(dplyr::starts_with("S")), ~ifelse(stringr::str_detect(.,"N"),NA,.))
  e <- dplyr::mutate_at(e, dplyr::vars(dplyr::starts_with("S")), ~as.numeric(.))
  if (frequency == "monthly") {
    e$Dates <- base::as.Date(x = paste0("1/", e$Dates), format = "%d/%m/%Y")
  } else {
    if (frequency == "daily") {
      e$Dates <- base::as.Date(x = e$Dates, format = "%m/%d/%Y")
    } else {
      if (frequency == "annual") {
        e$Dates <- base::as.Date(x = paste0("01/01/",
                                            e$Dates), format = "%d/%m/%Y")
      }
      else {
        if (frequency == "quarterly") {
          e$Dates <- base::as.Date(unlist(lapply(X = e[,
                                                       1], FUN = function(x) {
                                                         as.character(banxicoR::banxico_parsetrim(string = x,
                                                                                                  trim_begin = TRUE))
                                                       })))
        } else {
          e$Dates <- as.character(e$Dates)
          warning("Frequency not supported. Saving as character.")
        }
      }
    }
  }
  if (metadata) {
    units <- banxicoR::banxico_parsemeta(mtd, "unit", exclude = FALSE)
    datatype <- banxicoR::banxico_parsemeta(mtd, "data type",
                                            exclude = FALSE)
    period <- banxicoR::banxico_parsemeta(mtd, "period",
                                          exclude = FALSE)
    names <- banxicoR::banxico_parsemeta(mtd_head, series,
                                         exclude = TRUE)
    names <- stringr::str_to_title(paste0(names, collapse = " - "))
    l <- list(MetaData = list(IndicatorName = names, IndicatorId = series,
                              Units = units, DataType = datatype, Period = period,
                              Frequency = frequency), Data = as.data.frame(e))
    return(l)
  }
  else {
    return(as.data.frame(e))
  }
}

