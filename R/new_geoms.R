u <- Sys.setlocale("LC_ALL", "es_ES.UTF-8")

#' Título
#'
#' Descripción
#'
#' @param data input1
#'
#' @return output
#' @export
mostrar <- function(data) {

  return(data)

}

#' Valores absolutos abarcados
#'
#' Esta función te muestra en valores absolutos los montos recorridos por la colúmna
#' o vector evaluados.
#'
#' @param variable Vector o colúmna de datos a partir del cuál se calculan los datoss
#'
#' @return Valor absoluto de los valores absolutos (positivos y negativos) que recorre
#' el vector o colúmna.
#' @export
width_bar <- function(variable){
  ancho_barras <- ifelse(max(variable) <= 0,
                         abs(min(variable)),
                         max(variable)) +
    ifelse(min(variable) < 0,
           ifelse(max(variable) <= 0,
                  0,
                  abs(min(variable))), 0)

  ancho_barras <- ifelse(ancho_barras == 0, 1, ancho_barras)

  return(ancho_barras)
}

#' Geom text para barras horizontales
#'
#' Esta función sirve para añadir fácilmente los valores para los barras de variación
#' en valores negativos y positivos. Sólo se esplican los parámetros que difieren de
#' geom_text() normal
#'
#' @param data input1
#'
#' @return Una nueva capa de texto para una gráfica
#' @export
geom_text_bilateral <- function(lab_position,
                                 lab_hor = NULL,
                                 percent_change = 0.25,
                                 hdist = 0.05,
                                 vdist = NULL,
                                 vjust = 0.5,
                                 mapping = NULL, data = NULL,
                                 stat = "identity", position = "identity",
                                 color_black = "grey15", color_light = "grey95",
                                 ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE,
                                 na.rm = TRUE, show.legend = NA, inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }
    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  if (!missing(lab_hor)) {
    lab_hor <- rlang::enquo(lab_hor)

    variable <- rlang::enquo(lab_position)

    change <- rlang::expr(ifelse(!!variable >= 0 &
                            abs(!!variable)/width_bar(!!variable) >= percent_change,
                          !!variable, NA_integer_))

    mapping <- ggplot2::aes(y = !!change,
                   label = !!lab_hor)

    change_2 <- rlang::expr(ifelse(!!variable >= 0 &
                              abs(!!variable)/width_bar(!!variable) < percent_change,
                            !!variable, NA_integer_))

    mapping_2 <- ggplot2::aes(y = !!change_2,
                     label = !!lab_hor)

    change_3 <- rlang::expr(ifelse(!!variable < 0 &
                              abs(!!variable)/width_bar(!!variable) >= percent_change,
                            !!variable, NA_integer_))

    mapping_3 <- ggplot2::aes(y = !!change_3,
                     label = !!lab_hor)

    change_4 <- rlang::expr(ifelse(!!variable < 0 &
                              abs(!!variable)/width_bar(!!variable) < percent_change,
                            !!variable, NA_integer_))

    mapping_4 <- ggplot2::aes(y = !!change_4,
                     label = !!lab_hor)
  } else {
    variable <- rlang::enquo(lab_position)

    change <- rlang::expr(ifelse(!!variable >= 0 &
                            abs(!!variable)/width_bar(!!variable) >= percent_change,
                          !!variable, NA_integer_))

    mapping <- ggplot2::aes(y = !!change)

    change_2 <- rlang::expr(ifelse(!!variable >= 0 &
                              abs(!!variable)/width_bar(!!variable) < percent_change,
                            !!variable, NA_integer_))

    mapping_2 <- ggplot2::aes(y = !!change_2)

    change_3 <- rlang::expr(ifelse(!!variable < 0 &
                              abs(!!variable)/width_bar(!!variable) >= percent_change,
                            !!variable, NA_integer_))

    mapping_3 <- ggplot2::aes(y = !!change_3)

    change_4 <- rlang::expr(ifelse(!!variable < 0 &
                              abs(!!variable)/width_bar(!!variable) < percent_change,
                            !!variable, NA_integer_))

    mapping_4 <- ggplot2::aes(y = !!change_4)
  }

  list(ggplot2::layer(data = data, mapping = mapping,
             stat = stat, geom = GeomText,
             position = position, show.legend = show.legend, inherit.aes = inherit.aes,
             params = list(parse = parse, check_overlap = check_overlap,
                           na.rm = na.rm,
                           color = color_light,
                           hjust = 1 + hdist, # Positivo abajo
                           vjust = ifelse(is.null(vdist),
                                          vjust,
                                          1.15 + vdist),
                           ...)),
       ggplot2::layer(data = data, mapping = mapping_2,
             stat = stat, geom = GeomText,
             position = position, show.legend = show.legend, inherit.aes = inherit.aes,
             params = list(parse = parse, check_overlap = check_overlap,
                           na.rm = na.rm,
                           color = color_black,
                           hjust = -hdist, # Positivo arriba
                           vjust = ifelse(is.null(vdist),
                                          vjust,
                                          -0.1 - vdist),
                           ...)),
       ggplot2::layer(data = data, mapping = mapping_3,
             stat = stat, geom = GeomText,
             position = position, show.legend = show.legend, inherit.aes = inherit.aes,
             params = list(parse = parse, check_overlap = check_overlap,
                           na.rm = na.rm,
                           color = color_light,
                           vjust = ifelse(is.null(vdist),
                                          vjust,
                                          -0.15 + vdist),
                           hjust = -hdist,
                           ...)),
       ggplot2::layer(data = data, mapping = mapping_4,
             stat = stat, geom = GeomText,
             position = position, show.legend = show.legend, inherit.aes = inherit.aes,
             params = list(parse = parse, check_overlap = check_overlap,
                           na.rm = na.rm,
                           color = color_black,
                           vjust = ifelse(is.null(vdist),
                                          vjust,
                                          1.1 + vdist),
                           hjust = 1+hdist,
                           ...))
  )

}

#' Geom para generar segmentos sobre cada valor.
#'
#' Esta función genera segmentos para cada valor de x y así mostrar cierto nivel.
#'
#' @param data input1
#'
#' @return Una nueva capa de segmentos para una gráfica.
#' @export
geom_segment_point <- function(point_var_x,
                               point_var_y,
                               color_point = NULL,
                               mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               point_dist = 0.4,
                               ..., arrow = NULL, arrow.fill = NULL,
                               lineend = "butt", linejoin = "round",
                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){

  if (!missing(color_point)) {
    var_color <- rlang::enquo(color_point)

    variable_x <- rlang::enquo(point_var_x)

    variable_y <- rlang::enquo(point_var_y)

    mapping_seg <- ggplot2::aes(x = !!variable_x - point_dist,
                       xend = !!variable_x + point_dist,
                       y = !!variable_y,
                       yend = !!variable_y,
                       color = !!var_color)

    mapping_point_1 <- ggplot2::aes(x = !!variable_x + point_dist,
                           y = !!variable_y,
                           color = !!var_color)

    mapping_point_2 <- ggplot2::aes(x = !!variable_x - point_dist,
                           y = !!variable_y,
                           color = !!var_color)
  } else {

    variable_x <- rlang::enquo(point_var_x)

    variable_y <- rlang::enquo(point_var_y)

    mapping_seg <- ggplot2::aes(x = !!variable_x - point_dist,
                       xend = !!variable_x + point_dist,
                       y = !!variable_y,
                       yend = !!variable_y)

    mapping_point_1 <- ggplot2::aes(x = !!variable_x + point_dist,
                           y = !!variable_y)

    mapping_point_2 <- ggplot2::aes(x = !!variable_x - point_dist,
                           y = !!variable_y)
  }

  list(
    ggplot2::layer(data = data, mapping = mapping_seg, stat = stat, geom = GeomSegment,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, arrow.fill = arrow.fill,
                        lineend = lineend, linejoin = linejoin, na.rm = na.rm,
                        ...)),
    ggplot2::layer(data = data, mapping = mapping_point_1, stat = stat, geom = GeomPoint,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)),
    ggplot2::layer(data = data, mapping = mapping_point_2, stat = stat, geom = GeomPoint,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...))
  )
}
