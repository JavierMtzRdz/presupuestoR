u <- Sys.setlocale("LC_ALL", "es_ES.UTF-8")

#' Deflactar montos INEGI
#'
#' Esa función deflacta montos a partir del INPC.
#'
#' @param monto monto que se quiere deflactar
#' @param mes mes a comparar
#' @param year_monto año de origen del monto
#' @param year_out año del precio al que se quiere deflactar
#'
#' @importFrom magrittr %>%
#' @return regresa un vector de los montos deflactados
#' @export
deflactar_inpc <- function(monto, year_monto, year_out, mes = 12, acumulado = T) {

  if (!exists("inpc_bd") ||
      !(c("fecha") %in% colnames(inpc_bd)) ||
      !(c("mes") %in% colnames(inpc_bd)) ||
      !(c("year") %in% colnames(inpc_bd)) ||
      !(c("inpc") %in% colnames(inpc_bd))) {

    temp <- tempfile(fileext = ".xls")

    inpc_ur <- "http://www.banxico.org.mx/SieInternet/consultasieiqy?series=SP1&locale=en"

    download.file(inpc_ur, destfile = temp, mode = 'wb')

    inpc_bd <<- XML::htmlParse(temp) %>%
      XML::getNodeSet('//table') %>%
      .[[6]] %>%
      XML::readHTMLTable() %>%
      tibble() %>%
      janitor::clean_names() %>%
      tidyr::separate(date, c("mes", "year")) %>%
      dplyr::transmute(fecha = paste0(mes, "/", year),
                       mes = as.numeric(mes),
                       year = as.numeric(year),
                       inpc = as.numeric(sp1))

  }

  mes_dato <- as.numeric(mes)

  fecha_monto <- paste0(mes_dato, "/", year_monto)

  fecha_out <- paste0(mes_dato, "/", year_out)

  if (!all(c(fecha_monto, fecha_out) %in% inpc_bd$fecha)) {
    warning("Fechas no disponibles")

    return(monto)
  }

  if(acumulado) {

    inpc <- inpc_bd %>%
      dplyr::filter(mes <= mes_dato) %>%
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

    warning("Error en el año del monto deflactado. La cifra no fue deflactada.")

    return(monto)
  }

  deflactor_monto <- try(purrr::map_dbl(fecha_monto,
                                        function(x) as.numeric(inpc[x == inpc$fecha,]$inpc)),
                         silent = T)

  if (class(deflactor_monto) == "try-error") {
    warning("Error en el año al que se va a deflactar. La cifra no fue deflactada.")

    return(monto)
  }

  monto_deflactado <- (monto * deflactor_out) / deflactor_monto

  return(monto_deflactado)

}
