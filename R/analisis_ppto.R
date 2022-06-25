#' Deflactar montos
#'
#' Esa funci\\u00f3n deflacta montos descargando la \\u00faltima versi\\u00f3n de deflactores publicada por Transparencia presupuestaria.
#'
#' @param monto monto que se quiere deflactar
#' @param year_monto año de origen del monto (para que funciones tiene que ser
#' entre 1994 y 2030)
#' @param year_out año del precio al que se quiere deflactar (para que funciones tiene que ser
#' entre 1994 y 2030)
#' @param fuente aquí se indica si se quiere tomar el deflactor que se guardó en el
#' paquete el 24 de junio de 2022 ("local") o se descarga el último archivo
#'  ("cargar"). TP lo actualiza posterior a la presentación de la estructura programática
#'  y el PPEF.
#'
#' @importFrom magrittr %>%
#' @return regresa un vector de los montos deflactados
#' @export
deflactar_tp <- function(monto, year_monto, year_out,
                         fuente = "local") {

  year_monto <- as.numeric(year_monto)

  year_out <- as.numeric(year_out)

  monto <- as.numeric(monto)

  if (fuente == "cargar" &
      (!exists("deflactor_desc") ||
      !(c("year") %in% colnames(deflactor)) ||
      !(c("deflactor_year") %in% colnames(deflactor)))) {


    assign("deflactor_desc",
           openxlsx::read.xlsx("https://www.transparenciapresupuestaria.gob.mx/work/models/PTP/Presupuesto/Programacion/Deflactores/Deflactores_PIB.xlsx",
                               rows = 4:40) %>%
             janitor::clean_names() %>%
             dplyr::rename(deflactor_year = dplyr::starts_with("deflactor_del_pib")) %>%
             dplyr::transmute(year = as.numeric(periodo),
                              deflactor_year) %>%
             dplyr::filter(!is.na(year)),
           envir = as.environment("package:presupuestoR"))

    deflactor <- deflactor_desc

  }
  if (fuente == "local") {
    deflactor <- deflactor_local

  }



  if (!all(c(year_monto, year_out) %in% deflactor$year)) {
    warning("A\\u00f1o no incluido supera el rango de a\\u00f1os disponible. La cifra no fue deflactada.")

    return(monto)

  }

  deflactor_out <- try(purrr::map_dbl(year_out,
                                      function(x)
                                        deflactor[deflactor$year %in% as.numeric(x),]$deflactor_year),
                       silent = T)

  if (class(deflactor_out) == "try-error") {

    warning("Error en el a\\u00f1o del monto deflactado. La cifra no fue deflactada.")

    return(monto)
  }

  deflactor_monto <- try(purrr::map_dbl(year_monto,
                                        function(x)
                                          deflactor[deflactor$year %in% as.numeric(x),]$deflactor_year),
                         silent = T)

  if (class(deflactor_monto) == "try-error") {
    warning("Error en el a\\u00f1o al que se va a deflactar. La cifra no fue deflactada.")

    return(monto)
  }

  monto_deflactado <- (monto * deflactor_out) / deflactor_monto

  return(monto_deflactado)

}

#' ID de capítulo a descripción de capítulo
#'
#' Esta función regresa la descripción del capítulo poniendo el capítulo correspondiente
#'
#' @param id_capitulo la colúmna con la id del capítulo.
#'
#' @return regresa una lista con la descripción del capitulo
#' @export

id_capitulo_to_desc_capitulo <- function(id_capitulo) {

  y <- substr(id_capitulo, 0,1)
  y <- as.numeric(y)

  dplyr::case_when(
    y == 1 ~ "Servicios personales",
    y == 2 ~ "Materiales y suministros",
    y == 3 ~ "Servicios generales",
    y == 4 ~ "Transferencias, asignaciones, subsidios y otras ayudas",
    y == 5 ~ "Bienes muebles, inmuebles e intangibles",
    y == 6 ~ "Inversi\\u00f3n p\\u00fablica",
    y == 7 ~ "Inversiones financieras y otras provisiones",
    y == 8 ~ "Participaciones y aportaciones",
    y == 9 ~ "Deuda p\\u00fablica ",
    T ~ NA_character_
  )
}

#' Agrupaci\\u00f3n del presupuesto
#'
#' Esta funci\\u00f3n permite agrupar las bases de datos abiertas del presupuesto
#' de Transparencia Presupuestaria con los principales niveles de agregaci\\u00f3n.
#' De esta manera, podemos generar resumenes presupuestales de acuerdo a su
#' clasificaci\\u00f3n administrativa, funcional o econ\\u00f3mica.
#' Por ejemplo, permite generar r\\u00e1pidamente un .xframe con el presupuesto
#' en todas sus categor\\u00edas disponibles(aprobado, modificado, pagado y
#' ejercido) por ramo.
#' Nota 1: esta funci\\u00f3n siempre mantiene el periodo indicado y, en caso de no
#' tenerlo, toma el a\\u00f1o fiscal como periodo.
#' Nota 2: esta funci\\u00f3n no distingue los cambios de claves y t\\u00e9rminos a lo
#' largo del tiempo. Por ejemplo, si un programa cambi\\u00f3 de modalidad U a S,
#' e intentamos agrupar el programa con su modalidad y clave presupuestal,
#' la funci\\u00f3n la va distinguir como un programa diferente. Otro ejemplo es que
#' esta funci\\u00f3n no distingue  los cambios en el tiempo de la en la
#' clasificaci\\u00f3n por objeto del gasto. Si una misma categor\\u00eda tiene una
#' clave o un nombre distinto a lo largo del tiempo y se clasifica el
#' presupuesto por estas categor\\u00edas, la funci\\u00f3n lo clasificar\\u00e1 como
#' categor\\u00edas diferentes.
#'
#' @param .x .x frame con la estructura de los archivos sobre presupuesto
#' de transparencia Presupuestaria
#' (https://www.transparenciapresupuestaria.gob.mx/es/PTP/Datos_Abiertos).
#' @param ... nombre de las variables por las cuales se quiere colapsar
#' la informaci\\u00f3n. Estos nombre tienen que estar completamente en min\\u00fascula
#' sin caracteres especiales y separado por guiones bajos. Por ejemplo,
#' id_ramo, desc_ramo, id_modalidad, desc_modalidad, etc.
#' @param periodo_col agregar columna con el periodo correspondiente
#' @param keep_mensual indicador lógico para determinar si mantener la variables de aprobado
#' y modificado en el periodo (en caso de estar diponible)
#'
#' @importFrom magrittr %>%
#' @return .xframe colapsado por las variables indicadas. En caso de no
#' indicarse ninguna variable, s\\u00f3lo se colapsar\\u00e1 por a\\u00f1o fiscal.
#' @export
sum_pef_tp <- function(.x, ...,
                       periodo_col = NA,
                       keep_mensual = T) {

  .x %>%
    janitor::clean_names() %>%
    {
      if ("id_ramo" %in% names(.))
        dplyr::mutate(.,
                      id_ramo = as.character(id_ramo))
      else
        .
    } %>%
    {
      if (!is.na(periodo_col) &
          !("periodo" %in% names(.)))
        dplyr::mutate(.,
                      periodo = periodo_col)
      else
        .
    } %>%
    {
      if ("ciclo" %in% names(.))
        dplyr::mutate(.,
                      ciclo = as.numeric(ciclo))
      else
        .
    } %>%
    {
      if (!("periodo" %in% names(.)) &
          ("ciclo" %in% names(.)))
        dplyr::mutate(.,
                      periodo = as.numeric(ciclo))
      else
        .
    } %>%
    {
      if (!("periodo" %in% names(.)) &
          !("ciclo" %in% names(.)))
        dplyr::mutate(.,
                      periodo = NA)
      else
        .
    } %>%
    {if (("id_capitulo" %in% names(.)))
        dplyr::mutate(.,
                      id_capitulo = as.numeric(substr(id_capitulo, 0, 1)) *
                        1000)
      else . } %>%
    {
      if (!("id_capitulo" %in% names(.)) &
          ("id_objeto_del_gasto" %in% names(.)))
        dplyr::mutate(.,
                      id_capitulo = as.numeric(substr(id_objeto_del_gasto, 0, 1)) *
                        1000)
      else . } %>%
    {
    if (!("desc_capitulo" %in% names(.)) &
        ("id_capitulo" %in% names(.)))
      dplyr::mutate(.,
                    desc_capitulo = id_capitulo_to_desc_capitulo(id_capitulo))
  else . } %>%
    {
      if (("id_concepto" %in% names(.)) )
        dplyr::mutate(.,
                      id_concepto = as.numeric(substr(id_concepto, 0, 2)) *
                        100)
      else . } %>%
    {
      if (!("id_concepto" %in% names(.)) &
          ("id_objeto_del_gasto" %in% names(.)))
        dplyr::mutate(.,
                      id_concepto = as.numeric(substr(id_objeto_del_gasto, 0, 2)) *
                        100)
      else . } %>%
    {
      if (("id_partida_generica" %in% names(.)))
        dplyr::mutate(.,
                      id_partida_generica = as.numeric(substr(id_partida_generica, 0, 3)) *
                        10)
      else
        .
    } %>%
    {
      if (!("id_partida_generica" %in% names(.)) &
          ("id_objeto_del_gasto" %in% names(.)))
        dplyr::mutate(.,
                      id_partida_generica = as.numeric(substr(id_objeto_del_gasto, 0, 3)) *
                        10)
      else
        .
    } %>%
    {
      if (("id_objeto_del_gasto" %in% names(.)))
        dplyr::mutate(.,
                      id_objeto_del_gasto = as.numeric(id_objeto_del_gasto))
      else
        .
    } %>%
    {
      if (!("id_objeto_del_gasto" %in% names(.)) &
          ("id_partida_especifica" %in% names(.)))
        dplyr::mutate(.,
                      id_objeto_del_gasto = as.numeric(id_partida_especifica))
      else
        .
    } %>%
    {
      if (!("desc_objeto_del_gasto" %in% names(.)) &
          ("desc_partida_especifica" %in% names(.)))
        dplyr::mutate(.,
                      desc_objeto_del_gasto = desc_partida_especifica)
      else
        .
    } %>%
    {
      if ("id_ur" %in% names(.))
        dplyr::mutate(., id_ur = as.character(id_ur))
      else
        .
    } %>%
    {
      if (!("tipo_programable" %in% names(.)) &
          ("id_ramo" %in% names(.) &
           "id_capitulo" %in% names(.))) {
        dplyr::mutate(
          .,
          tipo_programable = dplyr::case_when(
            id_ramo %in% c(24, 28, 30, 34) ~ "No programable",
            id_ramo %in% c(52, 53) &
              id_capitulo == 9000 ~ "No programable",
            T ~ "Programable"
          )
        )
      } else
        .
    } %>%
    dplyr::group_by(..., periodo) %>%
    {
      if (keep_mensual)
        .
      else
        dplyr::select(.,
                      -dplyr::contains("mensual"))
    } %>%
    {
      if ("monto_aprobado_mensual" %in% names(.)) {
        dplyr::rename(.,
               monto_aprob_mes = monto_aprobado_mensual)
      } else
        .
    } %>%
    {
      if ("monto_modificado_mensual" %in% names(.)) {
        dplyr::rename(.,
               monto_modif_mes = monto_modificado_mensual)
      } else
        .
    } %>%
    dplyr::rename(
      proyecto = dplyr::contains(c("proyec")),
      aprobado = dplyr::contains(c("aprobado")),
      modificado = dplyr::contains("modificado"),
      pagado = dplyr::contains("pagado"),
      ejercido = dplyr::contains(c("ejercido", "ejercicio"))
    ) %>%
    dplyr::summarise_if(is.numeric, sum, na.rm = T) %>%
    dplyr::select(..., periodo, dplyr::contains(
      c(
        "proyecto",
        "aprobado",
        "modificado",
        "pagado",
        "ejercido",
        "monto_aprob_mes",
        "monto_modif_mes"
      )
    )) %>%
    dplyr::ungroup()

}



#' Agrupar lista de .xframes de presupuesto en formato long
#'
#' Esta funci\\u00f3n retoma la una funci\\u00f3n sum_pef (por lo cual le aplican las
#' mismas reglas) y la aplica a una lista de presupuesto. Esto ayuda cuando
#' se quiere generar datos hist\\u00f3ricos del presupuesto en formato long, los
#' cuales normalmente est\\u00e1n en diferentes archivos.
#' Lo ideal es cargar todos los .xframes de presupuesto en una lista de
#' .xframes y seleccionar las variables por las que se quiere acumular la
#' informaci\\u00f3n.
#' Por ejemplo. Si colocamos un vector de las cuentas p\\u00fablicas de cada a\\u00f1o
#' fiscal sin se\\u00f1alar ninguna variable, esta funci\\u00f3n regresar\\u00e1 el total
#' del presupuesto (sin neteo) de todos los ciclos fiscales inclu\\u00eddos.
#' Nota 1: si se acumulan dos .xframes del mismo a\\u00f1o fiscal y no de le
#' distingue con una variable periodo, esta funci\\u00f3n s\\u00f3lo se\\u00f1alar\\u00e1 el ciclo
#' fiscal sin distinguir si son de trimestress diferentes.
#' Nota 2: esta funci\\u00f3n no distingue los cambios de claves y t\\u00e9rminos a lo
#' largo del tiempo. Por ejemplo, si un programa cambi\\u00f3 de modalidad U a S,
#' e intentamos agrupar el programa con su modalidad y clave presupuestal,
#' la funci\\u00f3n la va distinguir como un programa diferente. Otro ejemplo es que
#' esta funci\\u00f3n no distingue  los cambios en el tiempo de la en la
#' clasificaci\\u00f3n por objeto del gasto. Si una misma categor\\u00eda tiene una
#' clave o un nombre distinto a lo largo del tiempo y se clasifica el
#' presupuesto por estas categor\\u00edas, la funci\\u00f3n lo clasificar\\u00e1 como
#' categor\\u00edas diferentes.
#'
#' @param lista_df lista de .xframes con la estructura de los datos abiertos
#' de presupuesto de Transparencia Presupuestaria.
#' @param ... nombre de las variables por las cuales se quiere colapsar
#' la informaci\\u00f3n. Estos nombre tienen que estar completamente en min\\u00fascula
#' sin caracteres especiales y separado por guiones bajos. Por ejemplo,
#' id_ramo, desc_ramo, id_modalidad, desc_modalidad, etc.
#'
#' @return .xframe colapsado por las variables indicadas. En caso de no
#' indicarse ninguna variable, s\\u00f3lo se colapsar\\u00e1 por a\\u00f1o fiscal.
#' @export
bind_pef_tp <- function(lista_df, ...) {

  purrr::map_dfr(lista_df, sum_pef_tp, ...)

}


#' Agrupar lista de .xframes de presupuesto en formato wide
#'
#' Esta funci\\u00f3n retoma la una funci\\u00f3n sum_pef_tp (por lo cual le aplican las
#' mismas reglas) y la aplica a una lista de presupuesto. Esto ayuda cuando
#' se quiere generar datos hist\\u00f3ricos del presupuesto en formato wide, los
#' cuales normalmente est\\u00e1n en diferentes archivos.
#' Lo ideal es cargar todos los .xframes de presupuesto en una lista de
#' .xframes y seleccionar las variables por las que se quiere acumular la
#' informaci\\u00f3n.
#' Por ejemplo. Si colocamos un vector de las cuentas p\\u00fablicas de cada a\\u00f1o
#' fiscal sin se\\u00f1alar ninguna variable, esta funci\\u00f3n regresar\\u00e1 el total
#' del presupuesto (sin neteo) de todos los ciclos fiscales inclu\\u00eddos.
#' Nota 1: si se acumulan dos .xframes del mismo a\\u00f1o fiscal y no de le
#' distingue con una variable periodo, esta funci\\u00f3n s\\u00f3lo se\\u00f1alar\\u00e1 el ciclo
#' fiscal sin distinguir si son de trimestress diferentes.
#' Nota 2: esta funci\\u00f3n no distingue los cambios de claves y t\\u00e9rminos a lo
#' largo del tiempo. Por ejemplo, si un programa cambi\\u00f3 de modalidad U a S,
#' e intentamos agrupar el programa con su modalidad y clave presupuestal,
#' la funci\\u00f3n la va distinguir como un programa diferente. Otro ejemplo es que
#' esta funci\\u00f3n no distingue  los cambios en el tiempo de la en la
#' clasificaci\\u00f3n por objeto del gasto. Si una misma categor\\u00eda tiene una
#' clave o un nombre distinto a lo largo del tiempo y se clasifica el
#' presupuesto por estas categor\\u00edas, la funci\\u00f3n lo clasificar\\u00e1 como
#' categor\\u00edas diferentes.
#'
#' @param lista_df lista de .xframes con la estructura de los datos abiertos
#' de presupuesto de Transparencia Presupuestaria.
#' @param ... nombre de las variables por las cuales se quiere colapsar
#' la informaci\\u00f3n. Estos nombre tienen que estar completamente en min\\u00fascula
#' sin caracteres especiales y separado por guiones bajos. Por ejemplo,
#' id_ramo, desc_ramo, id_modalidad, desc_modalidad, etc.
#'
#' @importFrom magrittr %>%
#' @return .xframe colapsado por las variables indicadas. En caso de no
#' indicarse ninguna variable, s\\u00f3lo se colapsar\\u00e1 por a\\u00f1o fiscal.
#' @export
bind_pef_tp_wide <- function(lista_df, ...) {
  purrr::map_dfr(lista_df, sum_pef_tp, ...) %>%
    tidyr::pivot_wider(
      names_from = periodo,
      values_from = dplyr::contains(
        c(
          "aprobado",
          "modificado",
          "pagado",
          "ejercido",
          "monto_aprob_mes",
          "monto_modif_mes"
        )
      ),
      names_glue = "{.value}_{periodo}"
    ) %>%
    janitor::clean_names() %>%
    # dplyr::rename(aprobado = dplyr::contains("aprobado")) %>%
    # dplyr::rename(aprobado = dplyr::contains("aprobado1")) %>%
    # dplyr::select(-dplyr::num_range("aprobado", 2:100)) %>%
    dplyr::select(function(x)
      any(!is.na(x)))
}


#' Netear
#'
#' Esta funci\\u00f3n pretende clasificar categor\\u00eda del presupuesto que deben ser
#' neteadas en caso de realizarse un an\\u00e1lisis del presupuesto neto. Esta
#' clasificaci\\u00f3n la realiza sobre archivos presupuestales con la estructura
#' de los datos de presupuesto abierto de Transparencia Presupuestaria.
#' Ojo: esta funci\\u00f3n a\\u00fan est\\u00e1 en desarrollo y, hasta ahora, el neteo s\\u00f3lo
#' es correcto aplicado al presupuesto aprobado.
#'
#' @param .x .xframe con la estructura de los datos abiertos de
#' presupuesto de Transparencia Presupuestaria.
#' @param ... modificaci\\u00f3n y creaci\\u00f3n de variables equivalente a dplyr::mutate
#' @param keep_mensual indicador lógico para determinar si mantener la variables de aprobado
#' y modificado en el periodo (en caso de estar diponible)
#'
#' @importFrom magrittr %>%
#' @return .xframe cuya descripci\\u00f3n de ramo indica los casos en que esa
#' categor\\u00eda del gasto debe ser neteada.
#' @export
netear_tp <- function(.x, ..., keep_mensual = T) {
  .x <- .x %>%
    janitor::clean_names() %>%
    {
      if (keep_mensual)
        .
      else
        dplyr::select(.,
                      -dplyr::contains("mensual"))
    } %>%
    {
      if ("monto_aprobado_mensual" %in% names(.)) {
        dplyr::rename(.,
                      monto_aprob_mes = monto_aprobado_mensual)
      } else
        .
    } %>%
    {
      if ("monto_modificado_mensual" %in% names(.)) {
        dplyr::rename(.,
                      monto_modif_mes = monto_modificado_mensual)
      } else
        .
    } %>%
    dplyr::rename(
      proyecto = dplyr::contains(c("proyec")),
      aprobado = dplyr::contains(c("aprobado")),
      modificado = dplyr::contains("modificado"),
      pagado = dplyr::contains("pagado"),
      ejercido = dplyr::contains(c("ejercido", "ejercicio"))
    )

  .x %>%
    {
      if ("id_objeto_del_gasto" %in% names(.)) {
        dplyr::mutate(
          .,
          id_capitulo = as.numeric(substr(id_objeto_del_gasto, 0, 1)) *
            1000,
          id_concepto = as.numeric(substr(id_objeto_del_gasto, 1, 2)) *
            100,
          id_partida_generica = as.numeric(substr(id_objeto_del_gasto, 1, 3)),
          id_partida_especifica = id_objeto_del_gasto,
          desc_partida_especifica = desc_objeto_del_gasto
        )
      } else
        .
    } %>%
    dplyr::bind_rows(.x %>%
                       {
                         if ("id_objeto_del_gasto" %in% names(.)) {
                           dplyr::mutate(
                             .,
                             id_capitulo = as.numeric(substr(id_objeto_del_gasto, 0, 1)) *
                               1000,
                             id_concepto = as.numeric(substr(id_objeto_del_gasto, 1, 2)) *
                               100,
                             id_partida_generica = as.numeric(substr(id_objeto_del_gasto,
                                                                     1, 3)),
                             id_partida_especifica = id_objeto_del_gasto,
                             desc_partida_especifica = desc_objeto_del_gasto
                           )
                         } else
                           .
                       } %>%
                       dplyr::mutate(
                         desc_ramo = dplyr::case_when(
                           id_partida_especifica %in% c(14101, 14105,
                                                        16104, 16107,
                                                        83102, 83110,
                                                        83113, 83116) &
                             id_ramo != 51 ~ "Neteo",
                           id_capitulo == 4000 &
                             id_ramo == 19 &
                             (
                               id_ur %in% c("GYR", "GYN") |
                                 desc_ur %in% c(
                                   "Instituto de Seguridad y Servicios Sociales de los Trabajadores del Estado",
                                   "Instituto Mexicano del Seguro Social"
                                 )
                             ) ~ "Neteo",
                           id_ramo == 23 &
                             id_modalidad == "U" &
                             id_pp == 129 ~ "Neteo",
                           # id_concepto %in% c(4700)  ~ "Neteo",

                           T ~ desc_ramo
                         ),
                         ...
                       ) %>%
                       dplyr::filter(desc_ramo == "Neteo"))
}



#' Presupuesto para neteo en negativo
#'
#' Esta funci\\u00f3n convierte aquellas categor\\u00edas del presupuesto que tienen
#' "Neteo" como descripci\\u00f3n del ramo en valores negativos. Normalmente se
#' deber\\u00eda usar despu\\u00e9s de la funci\\u00f3n de netear_tp.
#'
#' @param .x .xframe con la estructura de los datos abiertos de
#' presupuesto de Transparencia Presupuestaria.
#'
#' @importFrom magrittr %>%
#' @return .xframe cuyas categor\\u00edas del gasto con descripci\\u00f3n del ramo
#' como "Neteo" est\\u00e1n en valores negativos.
#' @export
negative_neteo_tp <- function(.x) {
  .x %>%
    {
      if ("aprobado" %in% names(.)) {
        dplyr::mutate(.,
               aprobado = ifelse(stringr::str_detect(desc_ramo, "Neteo"),
                                 -aprobado,
                                 aprobado))
      } else
        .
    } %>%
    {
      if ("modificado" %in% names(.)) {
        dplyr::mutate(.,
               modificado = ifelse(stringr::str_detect(desc_ramo, "Neteo"),
                                   -modificado,
                                   modificado))
      } else
        .
    } %>%
    {
      if ("pagado" %in% names(.)) {
        dplyr::mutate(.,
               pagado = ifelse(stringr::str_detect(desc_ramo, "Neteo"),
                               -pagado,
                               pagado))
      } else
        .
    } %>%
    {
      if ("ejercido" %in% names(.)) {
        dplyr::mutate(.,
               ejercido = ifelse(stringr::str_detect(desc_ramo, "Neteo"),
                                 -ejercido,
                                 ejercido))
      } else
        .
    } %>%
    {
      if ("proyecto" %in% names(.)) {
        dplyr::mutate(.,
                      proyecto = ifelse(stringr::str_detect(desc_ramo, "Neteo"),
                                        -proyecto,
                                        proyecto))
      } else
        .
    } %>%
    {
      if ("monto_aprob_mes" %in% names(.)) {
        dplyr::mutate(.,
               monto_aprob_mes = ifelse(
                 stringr::str_detect(desc_ramo, "Neteo"),
                 -monto_aprob_mes,
                 monto_aprob_mes
               ))
      } else
        .
    } %>%
    {
      if ("monto_aprobado_mensual" %in% names(.)) {
        dplyr::mutate(
          .,
          monto_aprobado_mensual = ifelse(
            stringr::str_detect(desc_ramo, "Neteo"),
            -monto_aprobado_mensual,
            monto_aprobado_mensual
          )
        )
      } else
        .
    }  %>%
    {
      if ("monto_modif_mes" %in% names(.)) {
        dplyr::mutate(.,
               monto_modif_mes = ifelse(
                 stringr::str_detect(desc_ramo, "Neteo"),
                 -monto_modif_mes,
                 monto_modif_mes
               ))
      } else
        .
    } %>%
    {
      if ("monto_modificado_mensual" %in% names(.)) {
        dplyr::mutate(
          .,
          monto_modificado_mensual = ifelse(
            stringr::str_detect(desc_ramo, "Neteo"),
            -monto_modificado_mensual,
            monto_modificado_mensual
          )
        )
      } else
        .
    }

}

#' Convertir clave del ramo a la clasificaci\\u00f3n administrativa de los ramos
#'
#' Nota 1: esta clasificaci\\u00f3n a la \\u00faltima actualizaci\\u00f3n del a\\u00f1o fiscal.
#' Por ejemplo, en esta clasificaci\\u00f3n ya se considera al ramo 49 (FGR)
#' como parte de los ramos aut\\u00f3nomos.
#'
#' @param id_ramo clave n\\u00famerica del ramo
#'
#' @return character con la clasificaci\\u00f3n administrativa del ramo
#' @export
id_ramo_to_tipo_ramo <- function(id_ramo) {

  y <- as.numeric(id_ramo)

  dplyr::case_when(
    y %in% c(1, 3, 22, 35,
                   41, 42, 43, 49, # TODO: cechar los casos en que el a\\u00f1o hace variar la calsificaci\\u00f3n.
                   44, 40, 32) ~ "A. Ramos aut\\u00f3nomos",
    y %in% c(2, 4, 5, 6,
                   7, 8, 9, 10,
                   11, 12, 13, 14,
                   15, 16, 17, 18,
                   20, 21, 27, 31,
                   36, 37, 38, 45,
                   46, 47, 48) ~ "B. Ramos administrativos",
    y %in% c(19, 23, 25, 33,
                   24, 28, 29, 30, 34) ~ "C. Ramos generales",
    y %in% c(50, 51) ~ "D. Entidades sujetas a control presupuestario directo",
    y %in% c(52, 53) ~ "E. Empresas productivas del estado",
    T ~ NA_character_
  )
}

#' Convertir clave del ramo a descripci\\u00f3n del ramo
#'
#' Nota 1: esta clasificaci\\u00f3n a la \\u00faltima actualizaci\\u00f3n del a\\u00f1o fiscal.
#' Por ejemplo, en esta clasificaci\\u00f3n ya se considera al ramo 49 (FGR)
#' como Fiscal\\u00eda y no Procuradur\\u00eda.
#'
#' @param id_ramo clave n\\u00famerica del ramo
#'
#' @return character con la clasificaci\\u00f3n administrativa del ramo
#' @export
id_ramo_to_desc_ramo <- function(id_ramo) {

  y <- as.numeric(id_ramo)

  dplyr::case_when(
    y == 1 ~  "Poder Legislativo",
    y == 2 ~  "Oficina de la Presidencia de la Rep\\u00fablica",
    y == 3 ~  "Poder Judicial",
    y == 4 ~  "Gobernaci\\u00f3n",
    y == 5 ~  "Relaciones Exteriores",
    y == 6 ~  "Hacienda y Cr\\u00e9dito P\\u00fablico",
    y == 7 ~  "Defensa Nacional",
    y == 8 ~  "Agricultura y Desarrollo Rural",
    y == 9 ~  "Comunicaciones y Transportes",
    y == 10 ~	"Econom\\u00eda",
    y == 11 ~	"Educaci\\u00f3n P\\u00fablica",
    y == 12 ~	"Salud",
    y == 13 ~	"Marina",
    y == 14 ~	"Trabajo y Previsi\\u00f3n Social",
    y == 15 ~	"Desarrollo Agrario, Territorial y Urbano",
    y == 16 ~	"Medio Ambiente y Recursos Naturales",
    y == 18 ~	"Energ\\u00eda",
    y == 19 ~	"Aportaciones a Seguridad Social",
    y == 20 ~	"Bienestar",
    y == 21 ~	"Turismo",
    y == 22 ~	"Instituto Nacional Electoral",
    y == 23 ~	"Provisiones Salariales y Econ\\u00f3micas",
    y == 24 ~	"Deuda P\\u00fablica",
    y == 25 ~	"Previsiones y Aportaciones para los Sistemas de Educaci\\u00f3n B\\u00e1sica, Normal, Tecnol\\u00f3gica y de Adultos",
    y == 27 ~	"Funci\\u00f3n P\\u00fablica",
    y == 28 ~	"Participaciones a Entidades Federativas y Municipios",
    y == 30 ~	"Adeudos de Ejercicios Fiscales Anteriores",
    y == 31 ~	"Tribunales Agrarios",
    y == 32 ~	"Tribunal Federal de Justicia Administrativa",
    y == 33 ~	"Aportaciones Federales para Entidades Federativas y Municipios",
    y == 34 ~	"Erogaciones para los Programas de Apoyo a Ahorradores y Deudores de la Banca",
    y == 35 ~	"Comisi\\u00f3n Nacional de los Derechos Humanos",
    y == 36 ~	"Seguridad y Protecci\\u00f3n Ciudadana",
    y == 37 ~	"Consejer\\u00eda Jur\\u00eddica del Ejecutivo Federal",
    y == 38 ~	"Consejo Nacional de Ciencia y Tecnolog\\u00eda",
    y == 40 ~	"Informaci\\u00f3n Nacional Estad\\u00edstica y Geogr\\u00e1fica",
    y == 41 ~	"Comisi\\u00f3n Federal de Competencia Econ\\u00f3mica",
    y == 43 ~	"Instituto Federal de Telecomunicaciones",
    y == 44 ~	"Instituto Nacional de Transparencia, Acceso a la Informaci\\u00f3n y Protecci\\u00f3n de Datos Personales",
    y == 45 ~	"Comisi\\u00f3n Reguladora de Energ\\u00eda",
    y == 46 ~	"Comisi\\u00f3n Nacional de Hidrocarburos",
    y == 47 ~	"Entidades no Sectorizadas",
    y == 48 ~	"Cultura",
    y == 49 ~	"Fiscal\\u00eda General de la Rep\\u00fablica",
    y == 50 ~	"Instituto Mexicano del Seguro Social",
    y == 51 ~	"Instituto de Seguridad y Servicios Sociales de los Trabajadores del Estado",
    y == 52 ~	"Petr\\u00f3leos Mexicanos",
    y == 53 ~	"Comisi\\u00f3n Federal de Electricidad",
    T ~ NA_character_
  )
}


#' Convertir clave del ramo a abreviatura del ramo
#'
#' Nota 1: esta clasificaci\\u00f3n a la \\u00faltima actualizaci\\u00f3n del a\\u00f1o fiscal.
#' Por ejemplo, en esta clasificaci\\u00f3n ya se considera al ramo 49 (FGR)
#' como Fiscal\\u00eda y no Procuradur\\u00eda.
#'
#' @param id_ramo clave n\\u00famerica del ramo
#'
#' @return character con la clasificaci\\u00f3n administrativa del ramo
#' @export
id_ramo_to_abr_ramo <- function(id_ramo) {

  y <- as.numeric(id_ramo)

  dplyr::case_when(
    y == 1	~ "P. Legislativo",
    y == 2	~ "OPR",
    y == 3	~ "P. Judicial",
    y == 4	~ "Segob",
    y == 5	~ "SRE",
    y == 6	~ "SHCP",
    y == 7	~ "Sedena",
    y == 8	~ "Sader",
    y == 9	~ "SCT",
    y == 10	~ "SE",
    y == 11	~ "SEP",
    y == 12	~ "SSA",
    y == 13	~ "Semar",
    y == 14	~ "STPS",
    y == 15	~ "Sedatu",
    y == 16	~ "Semarnat",
    y == 18	~ "Sener",
    y == 19	~ "Aport. a la SS.",
    y == 20	~ "Bienestar",
    y == 21	~ "Sectur",
    y == 22	~ "INE",
    y == 23	~ "Prov. Salariales y Econ.",
    y == 24	~ "Deuda P\\u00fablica",
    y == 25	~ "Prev. y Aport. a la Educ.",
    y == 27	~ "SFP",
    y == 28	~ "Participaciones Fed. para Ent. y Mun.",
    y == 30	~ "Adefas",
    y == 31	~ "Trib. Agra.",
    y == 32	~ "TFJA",
    y == 33	~ "Aportaciones Fed. para Ent. y Mun.",
    y == 34	~ "Erog. para Prog. de Apoyo a Ahorradores y Deudores de la Banca",
    y == 35	~ "CNDH",
    y == 36	~ "SSPC",
    y == 37	~ "CJEF",
    y == 38	~ "Conacyt",
    y == 40	~ "INEGI",
    y == 41	~ "Cofece",
    y == 43	~ "IFT",
    y == 44	~ "INAI",
    y == 45	~ "CRE",
    y == 46	~ "CNH",
    y == 47	~ "Ent. no sect.",
    y == 48	~ "Cultura",
    y == 49	~ "FGR",
    y == 50	~ "IMSS",
    y == 51	~ "ISSSTE",
    y == 52	~ "Pemex",
    y == 53	~ "CFE",
    T ~ NA_character_
  )
}


#' Convertir nombre del ramo a abreviatura del ramo
#'
#' @param desc_ramo string con el nombre del ramo
#'
#' @return un string con la abreviatura del ramo
#' @export
desc_ramo_to_abr_ramo <- function(desc_ramo) {
  y <- stringi::stri_trans_general(desc_ramo,"Latin-ASCII")
  y <- stringi::stri_replace_all(y, regex = "[:punct:]", "")
  y <- stringr::str_to_upper(y)
  y <- stringi::stri_replace_all(y, regex = "SECRETARIA |SECRETARIA", "")

  dplyr::case_when(
    stringr::str_detect(y, "LEGISLATIVO|CONGRESO") ~ "P. Legislativo",
    stringr::str_detect(y, "PRESIDENCIA") ~ "OPR",
    stringr::str_detect(y, "JUDICIAL") ~ "P. Judicial",
    stringr::str_detect(y, "GOBERNACION") ~ "Segob",
    stringr::str_detect(y, "EXTERIORES") ~ "SRE",
    stringr::str_detect(y, "HACIENDA") ~ "SHCP",
    stringr::str_detect(y, "DEFENSA") ~ "Sedena",
    stringr::str_detect(y, "AGRICULTURA Y DESARROLLO RURAL") ~ "Sader",
    stringr::str_detect(y, "COMUNICACIONES Y TRANSPORTES") ~ "SCT",
    stringr::str_detect(y, "ECONOMIA") ~ "SE",
    stringr::str_detect(y, "EDUCACION") ~ "SEP",
    stringr::str_detect(y, "SALUD") ~ "SSA",
    stringr::str_detect(y, "MARINA") ~ "Semar",
    stringr::str_detect(y, "TRABAJO Y PREVISION SOCIAL") ~ "STPS",
    stringr::str_detect(y, "DESARROLLO AGRARIO TERRITORIAL Y URBANO") ~ "Sedatu",
    stringr::str_detect(y, "MEDIO AMBIENTE Y RECURSOS NATURALES") ~ "Semarnat",
    y == "ENERGIA" ~ "Sener", #PONER DESPU\\u00e9S DE
    stringr::str_detect(y, "APORTACIONES A SEGURIDAD SOCIAL") ~ "Aport. a la SS.",
    stringr::str_detect(y, "BIENESTAR") ~ "Bienestar",
    stringr::str_detect(y, "TURISMO") ~ "Sectur",
    stringr::str_detect(y, "ELECTORAL") &
      stringr::str_detect(y, "NACIONAL") ~ "INE",
    stringr::str_detect(y, "ELECTORAL") &
      stringr::str_detect(y, "FEDERAL") ~ "IFE",
    stringr::str_detect(y, "PROVISIONES") &
      stringr::str_detect(y, "SALARIAL") ~ "Prov. Salariales y Econ.",
    stringr::str_detect(y, "DEUDA") &
      stringr::str_detect(y, "PUBLICA") ~ "Deuda P\\u00fablica",
    stringr::str_detect(y, "PREVISIONES") &
      stringr::str_detect(y, "APORTACIONES") &
      stringr::str_detect(y, "EDUCA") ~ "Prev. y Aport. a la Educ.",
    stringr::str_detect(y, "FUNCION") &
      stringr::str_detect(y, "PUB") ~ "SFP",
    stringr::str_detect(y, "PARTICIPACIONES") &
      stringr::str_detect(y, "ENTIDA|ESTADO") ~ "Participaciones Fed. para Ent. y Mun.",
    stringr::str_detect(y, "ADEUDOS") &
      stringr::str_detect(y, "ANTERIOR") ~ "Adefas",
    stringr::str_detect(y, "TRIBUNALES") &
      stringr::str_detect(y, "AGRARIOS") ~ "Trib. Agra.",
    stringr::str_detect(y, "TRIBUNAL") &
      stringr::str_detect(y, "ADMINISTRATIV") ~ "TFJA",
    stringr::str_detect(y, "APORTACIONES") &
      stringr::str_detect(y, "ENTIDA|ESTADO") ~ "Aportaciones Fed. para Ent. y Mun.",
    stringr::str_detect(y, "EROGACION") &
      stringr::str_detect(y, "AHORRA|DEUDO") &
      stringr::str_detect(y, "BANCA") ~ "Erog. para Prog. de Apoyo a Ahorradores y Deudores de la Banca",
    stringr::str_detect(y, "DERECHOS HUMANOS") ~ "CNDH",
    stringr::str_detect(y, "SEGURIDAD") &
      stringr::str_detect(y, "PUBLICA") ~ "SSP",
    stringr::str_detect(y, "SEGURIDAD") &
      stringr::str_detect(y, "CIUDADANA") ~ "SSPC",
    stringr::str_detect(y, "CONSEJERIA") &
      stringr::str_detect(y, "JURIDICA") ~ "CJEF",
    stringr::str_detect(y, "CIENCIA") &
      stringr::str_detect(y, "TECNOLOGIA") ~ "Conacyt",
    stringr::str_detect(y, "ESTADISTICA") &
      stringr::str_detect(y, "GEOGRAFIC") ~ "INEGI",
    stringr::str_detect(y, "COMPETENCIA") &
      stringr::str_detect(y, "ECONOM") ~ "Cofece",
    stringr::str_detect(y, "TELECOMUNICACIONES") ~ "IFT",
    stringr::str_detect(y, "FEDERAL") &
      stringr::str_detect(y, "INFORMACION")~ "IFAI",
    stringr::str_detect(y, "NACIONAL") &
      stringr::str_detect(y, "INFORMACION")~ "INAI",
    stringr::str_detect(y, "TRANSPARENCIA") ~ "INAI",
    stringr::str_detect(y, "REGULADORA") &
      stringr::str_detect(y, "ENERGIA") ~ "CRE",
    stringr::str_detect(y, "HIDROCARBUROS") &
      stringr::str_detect(y, "NACIONAL") ~ "CNH",
    stringr::str_detect(y, "NO SECTOR") ~ "Ent. no sect.",
    stringr::str_detect(y, "CULTURA") ~ "Cultura",
    stringr::str_detect(y, "FISCALIA") ~ "FGR",
    stringr::str_detect(y, "PROCURADURIA") ~ "PGR",
    stringr::str_detect(y, "INSTITUTO") &
      stringr::str_detect(y, "SEGURO SOCIAL") ~ "IMSS",
    stringr::str_detect(y, "SEGURIDAD") &
      stringr::str_detect(y, "TRABAJADORES") &
      stringr::str_detect(y, "ESTADO") ~ "ISSSTE",
    stringr::str_detect(y, "PETROLEOS") &
      stringr::str_detect(y, "MEXICANOS") ~ "Pemex",
    stringr::str_detect(y, "ELECTRICIDAD") ~ "CFE",
    T ~ NA_character_
  )
}


#' Generar col\\u00famna con clasificaci\\u00f3n de programable
#'
#' @param .x base de datos de presupuesto
#'
#' @return una base de datos de presupuesto con clasificaci\\u00f3n del gasto
#' programable y no programable
#' @export
gen_tipo_programable <- function(.x) {
  dplyr::mutate(.x,
    tipo_programable = dplyr::case_when(
      id_ramo %in% c(24, 28, 30, 34) ~ "No programable",
      id_ramo %in% c(52, 53) &
        id_capitulo == 9000 ~ "No programable",
      T ~ "Programable"
    )
  )
}


#' Generar clasificación económica
#'
#' @param .x base de datos de presupuesto
#' @param name nombre de la columna con la clasificación económica
#'
#' @importFrom rlang :=
#' @importFrom rlang !!
#' @return una base de datos de presupuesto con clasificación económica
#' @export
gen_clas_eco <- function (.x,
                          name = clasif_eco) {

  name_new_col <- rlang::enquo(name)

    .x %>% janitor::clean_names() %>% {
      if ("id_ramo" %in% names(.))
        dplyr::mutate(., id_ramo = as.character(id_ramo))
      else .
    } %>% {
      if ("ciclo" %in% names(.))
        dplyr::mutate(., ciclo = as.numeric(ciclo))
      else .
    } %>% {
      if (!("periodo" %in% names(.)) & ("ciclo" %in% names(.)))
        dplyr::mutate(., periodo = as.numeric(ciclo))
      else .
    } %>% {
      if (!("periodo" %in% names(.)) & !("ciclo" %in% names(.)))
        dplyr::mutate(., periodo = NA)
      else .
    } %>% {
      if (("id_capitulo" %in% names(.)))
        dplyr::mutate(., id_capitulo = as.numeric(substr(id_capitulo,
                                                         0, 1)) * 1000)
      else .
    } %>% {
      if (!("id_capitulo" %in% names(.)) & ("id_objeto_del_gasto" %in%
                                            names(.)))
        dplyr::mutate(., id_capitulo = as.numeric(substr(id_objeto_del_gasto,
                                                         0, 1)) * 1000)
      else .
    } %>% {
      if (("id_concepto" %in% names(.)))
        dplyr::mutate(., id_concepto = as.numeric(substr(id_concepto,
                                                         0, 2)) * 100)
      else .
    } %>% {
      if (!("id_concepto" %in% names(.)) & ("id_objeto_del_gasto" %in%
                                            names(.)))
        dplyr::mutate(., id_concepto = as.numeric(substr(id_objeto_del_gasto,
                                                         0, 2)) * 100)
      else .
    } %>% {
      if (("id_partida_generica" %in% names(.)))
        dplyr::mutate(., id_partida_generica = as.numeric(substr(id_partida_generica,
                                                                 0, 3)) * 10)
      else .
    } %>% {
      if (!("id_partida_generica" %in% names(.)) & ("id_objeto_del_gasto" %in%
                                                    names(.)))
        dplyr::mutate(., id_partida_generica = as.numeric(substr(id_objeto_del_gasto,
                                                                 0, 3)) * 10)
      else .
    } %>% {
      if (("id_objeto_del_gasto" %in% names(.)))
        dplyr::mutate(., id_objeto_del_gasto = as.numeric(id_objeto_del_gasto))
      else .
    } %>% { ## Cortar
      if (!("id_objeto_del_gasto" %in% names(.)) & ("id_partida_especifica" %in%
                                                    names(.)))
        dplyr::mutate(., id_objeto_del_gasto = as.numeric(id_partida_especifica))
      else .
    } %>% {
      if (!("desc_objeto_del_gasto" %in% names(.)) & ("desc_partida_especifica" %in%
                                                      names(.)))
        dplyr::mutate(., desc_objeto_del_gasto = desc_partida_especifica)
      else .
    } %>% {
      if ("id_ur" %in% names(.))
        dplyr::mutate(., id_ur = as.character(id_ur))
      else .
    } %>% {
      # if (!("clasif_eco" %in% names(.)) & ("id_ramo" %in% names(.) & "id_capitulo" %in% names(.))) {

      dplyr::mutate(

        ., !!name_new_col := dplyr::case_when(
          # GASTO CORRIENTE
          # Subsidios
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & stringr::str_starts(id_objeto_del_gasto, "43") ~ "Gasto corriente",
          # Servicios personales
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & stringr::str_starts(id_objeto_del_gasto, "1|83101|83102|83106|83107|83108|83109|83110|83111|83112|83113|83114|83115|83116|83117") ~ "Gasto corriente",
          # Gastos de operaci\\u00f3n
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & stringr::str_starts(id_objeto_del_gasto, "2|3") & !stringr::str_starts(id_objeto_del_gasto, "391|394|395|396|397|26106|32902|39908|39910") ~ "Gasto corriente",
          # Otros de corriente
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) &
            (
              (id_capitulo == "4000" & id_concepto != "4300" & id_concepto != "4500"& id_concepto != "4700") | # !str_starts(id_objeto_del_gasto, "43|45|47")) |
                stringr::str_starts(id_objeto_del_gasto, "79|85|391|394|395|396|397|834|26106|32902|39908|39910|83103|83105|83118") |
                (stringr::str_starts(id_objeto_del_gasto, "471") & (id_ramo == 19 & (id_ur == "GYR" | id_ur == "GYN")))
            ) ~ "Gasto corriente",

          # PENSIONES Y JUBILACIONES
          stringr::str_starts(id_objeto_del_gasto, "45") | (stringr::str_starts(id_objeto_del_gasto, "471") & (id_ramo == 19 & (id_ur != "GYR" & id_ur != "GYN"))) ~ "Pensiones y jubilaciones",

          # GASTOS DE INVERSI\\u00f3N
          # Inversi\\u00f3n f\\u00edsica
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "1|2|5|6|3|4|79|85|831|834") & !stringr::str_starts(id_objeto_del_gasto, "39909|43") ~ "Gastos de inversi\\u00f3n",
          # Subsidios
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "43") ~ "Gastos de inversi\\u00f3n",
          # Otros de inversi\\u00f3n
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "7|39909") & !stringr::str_starts(id_objeto_del_gasto, "79") ~ "Gastos de inversi\\u00f3n",

          T ~ NA_character_
        )

      )


    }

  }



#' Generar subclasificaci\\u00f3n econ\\u00f3mica
#'
#' @param .x base de datos de presupuesto
#' @param name_clas nombre de la columna con la clasificación económica
#' @param name_sub nombre de la columna con la sclasificación económica
#'
#' @importFrom rlang :=
#' @importFrom rlang !!
#' @return una base de datos de presupuesto con subclasificaci\\u00f3n econ\\u00f3mica
#' @export
gen_subclas_eco <- function (.x,
                             name_clas = clasif_eco,
                             name_sub = sub_clasif_eco) {

      name_new_col_clas <- rlang::enquo(name_clas)
      name_new_col_sub <- rlang::enquo(name_sub)


    .x %>%
      janitor::clean_names() %>% {
      if ("id_ramo" %in% names(.))
        dplyr::mutate(., id_ramo = as.character(id_ramo))
      else .
    } %>% {
      if (!is.na(periodo_col) & !("periodo" %in% names(.)))
        dplyr::mutate(., periodo = periodo_col)
      else .
    } %>% {
      if ("ciclo" %in% names(.))
        dplyr::mutate(., ciclo = as.numeric(ciclo))
      else .
    } %>% {
      if (!("periodo" %in% names(.)) & ("ciclo" %in% names(.)))
        dplyr::mutate(., periodo = as.numeric(ciclo))
      else .
    } %>% {
      if (!("periodo" %in% names(.)) & !("ciclo" %in% names(.)))
        dplyr::mutate(., periodo = NA)
      else .
    } %>% {
      if (("id_capitulo" %in% names(.)))
        dplyr::mutate(., id_capitulo = as.numeric(substr(id_capitulo,
                                                         0, 1)) * 1000)
      else .
    } %>% {
      if (!("id_capitulo" %in% names(.)) & ("id_objeto_del_gasto" %in%
                                            names(.)))
        dplyr::mutate(., id_capitulo = as.numeric(substr(id_objeto_del_gasto,
                                                         0, 1)) * 1000)
      else .
    } %>% {
      if (("id_concepto" %in% names(.)))
        dplyr::mutate(., id_concepto = as.numeric(substr(id_concepto,
                                                         0, 2)) * 100)
      else .
    } %>% {
      if (!("id_concepto" %in% names(.)) & ("id_objeto_del_gasto" %in%
                                            names(.)))
        dplyr::mutate(., id_concepto = as.numeric(substr(id_objeto_del_gasto,
                                                         0, 2)) * 100)
      else .
    } %>% {
      if (("id_partida_generica" %in% names(.)))
        dplyr::mutate(., id_partida_generica = as.numeric(substr(id_partida_generica,
                                                                 0, 3)) * 10)
      else .
    } %>% {
      if (!("id_partida_generica" %in% names(.)) & ("id_objeto_del_gasto" %in%
                                                    names(.)))
        dplyr::mutate(., id_partida_generica = as.numeric(substr(id_objeto_del_gasto,
                                                                 0, 3)) * 10)
      else .
    } %>% {
      if (("id_objeto_del_gasto" %in% names(.)))
        dplyr::mutate(., id_objeto_del_gasto = as.numeric(id_objeto_del_gasto))
      else .
    } %>% {
      if (!("id_objeto_del_gasto" %in% names(.)) & ("id_partida_especifica" %in%
                                                    names(.)))
        dplyr::mutate(., id_objeto_del_gasto = as.numeric(id_partida_especifica))
      else .
    } %>% {
      if (!("desc_objeto_del_gasto" %in% names(.)) & ("desc_partida_especifica" %in%
                                                      names(.)))
        dplyr::mutate(., desc_objeto_del_gasto = desc_partida_especifica)
      else .
    } %>% {
      if ("id_ur" %in% names(.))
        dplyr::mutate(., id_ur = as.character(id_ur))
      else .
    } %>% {
      # if (!("clasif_eco" %in% names(.)) & ("id_ramo" %in% names(.) & "id_capitulo" %in% names(.))) {

      dplyr::mutate(

        ., !!name_new_col_clas := dplyr::case_when(
          # GASTO CORRIENTE
          # Subsidios
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & stringr::str_starts(id_objeto_del_gasto, "43") ~ "Gasto corriente",
          # Servicios personales
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & stringr::str_starts(id_objeto_del_gasto, "1|83101|83102|83106|83107|83108|83109|83110|83111|83112|83113|83114|83115|83116|83117") ~ "Gasto corriente",
          # Gastos de operaci\\u00f3n
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & stringr::str_starts(id_objeto_del_gasto, "2|3") & !stringr::str_starts(id_objeto_del_gasto, "391|394|395|396|397|26106|32902|39908|39910") ~ "Gasto corriente",
          # Otros de corriente
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) &
            (
              (id_capitulo == "4000" & id_concepto != "4300" & id_concepto != "4500"& id_concepto != "4700") | # !str_starts(id_objeto_del_gasto, "43|45|47")) |
                stringr::str_starts(id_objeto_del_gasto, "79|85|391|394|395|396|397|834|26106|32902|39908|39910|83103|83105|83118") |
                (stringr::str_starts(id_objeto_del_gasto, "471") & (id_ramo == 19 & (id_ur == "GYR" | id_ur == "GYN")))
            ) ~ "Gasto corriente",

          # PENSIONES Y JUBILACIONES
          stringr::str_starts(id_objeto_del_gasto, "45") | (stringr::str_starts(id_objeto_del_gasto, "471") & (id_ramo == 19 & (id_ur != "GYR" & id_ur != "GYN"))) ~ "Pensiones y jubilaciones",

          # GASTOS DE INVERSI\\u00f3N
          # Inversi\\u00f3n f\\u00edsica
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "1|2|5|6|3|4|79|85|831|834") & !stringr::str_starts(id_objeto_del_gasto, "39909|43") ~ "Gastos de inversi\\u00f3n",
          # Subsidios
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "43") ~ "Gastos de inversi\\u00f3n",
          # Otros de inversi\\u00f3n
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "7|39909") & !stringr::str_starts(id_objeto_del_gasto, "79") ~ "Gastos de inversi\\u00f3n",

          T ~ NA_character_
        ),
        !!name_new_col_sub := dplyr::case_when(
          # GASTO CORRIENTE
          # Subsidios
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & id_concepto == "4300" ~ "Subsidios",
          # Servicios personales
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & stringr::str_starts(id_objeto_del_gasto, "1|83101|83102|83106|83107|83108|83109|83110|83111|83112|83113|83114|83115|83116|83117") ~ "Servicios personales",
          # Gastos de operaci\\u00f3n
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & stringr::str_starts(id_objeto_del_gasto, "2|3") & !stringr::str_starts(id_objeto_del_gasto, "391|394|395|396|397|26106|32902|39908|39910") ~ "Gastos de operaci\\u00f3n",
          clasif_eco == "Gasto corriente" ~ "Gastos de operaci\\u00f3n",

          # PENSIONES Y JUBILACIONES
          stringr::str_starts(id_objeto_del_gasto, "45") | (stringr::str_starts(id_objeto_del_gasto, "471") & (id_ramo == 19 & (id_ur != "GYR" & id_ur != "GYN"))) ~ "Pensiones y jubilaciones",

          # GASTOS DE INVERSI\\u00f3N
          # Subsidios
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "43") ~ "Subsidios",
          # Inversi\\u00f3n financiera
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & (id_capitulo == "7000" & id_concepto != "7900") ~ "Inversi\\u00f3n financiera",
          # Otros de inversi\\u00f3n
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "7|39909") & !stringr::str_starts(id_objeto_del_gasto, "79") ~ "Otros de inversi\\u00f3n",
          # Inversi\\u00f3n f\\u00edsica
          clasif_eco == "Gastos de inversi\\u00f3n" ~ "Inversi\\u00f3n f\\u00edsica",

          T ~ NA_character_
        )

      )


    }

  }

#' Datos del deflactor de TP
#'
#' Son los datos del deflactor implícito del PIB, observados y estimados
#' de la SHCP.
#'
#' @format Data Frame
#' @source Datos de TP disponibles en https://www.transparenciapresupuestaria.gob.mx/work/models/PTP/Presupuesto/Programacion/Deflactores/Deflactores_PIB.xlsx
"deflactor_local"

globalVariables(c(".", "deflactor_local", "periodo",
                  "n", "id", "inpc_bd",
                  "year", "sp1", "deflactor_year",
                  "deflactor_desc", "clasif_eco", "id_ramo",
                  "ciclo", "id_capitulo", "id_objeto_del_gasto",
                  "id_concepto", "id_partida_especifica", "id_partida_generica",
                  "desc_partida_especifica", "id_ur", "clasif_eco",
                  "sub_clasif_eco", "periodo_col", "index",
                  "num", "desc_ramo", "aprobado",
                  "modificado", "pagado", "ejercido",
                  "proyecto", "monto_aprob_mes", "monto_aprobado_mensual",
                  "monto_modif_mes", "monto_modificado_mensual", "desc_objeto_del_gasto",
                  "monto_aprobado_mensual", "desc_ramo"))
