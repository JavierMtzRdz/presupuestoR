u <- Sys.setlocale("LC_ALL", "es_ES.UTF-8")

#' Deflactar montos
#'
#' Esa función deflacta montos descargando la última versión de deflactores publicada por Transparencia presupuestaria.
#'
#' @param monto monto que se quiere deflactar
#' @param year_monto año de origen del monto (para que funciones tiene que ser
#' entre 1994 y 2030)
#' @param year_out año del precio al que se quiere deflactar (para que funciones tiene que ser
#' entre 1994 y 2030)
#'
#' @importFrom magrittr %>%
#' @return regresa un vector de los montos deflactados
#' @export
deflactar_tp <- function(monto, year_monto, year_out) {

  year_monto <- as.numeric(year_monto)

  year_out <- as.numeric(year_out)

  monto <- as.numeric(monto)


  if (!exists("deflactor") ||
      !(c("year") %in% colnames(deflactor)) ||
      !(c("deflactor_year") %in% colnames(deflactor))) {

    temp <- tempfile(fileext = ".xlsx")

    dataURL <-
      "https://www.transparenciapresupuestaria.gob.mx/work/models/PTP/Presupuesto/Programacion/Deflactores/Deflactores_PIB.xlsx"

    download.file(dataURL, destfile = temp, mode = 'wb')

    deflactor <<- readxl::read_excel(temp,
                                     range = "B4:O42") %>%
      janitor::clean_names() %>%
      dplyr::rename(deflactor_year = starts_with("deflactor_del_pib")) %>%
      dplyr::transmute(year = as.numeric(periodo),
                       deflactor_year) %>%
      dplyr::filter(!is.na(year))

  }


  if (!all(c(year_monto, year_out) %in% deflactor$year)) {
    warning("Año no incluido supera el rango de años disponible. La cifra no fue deflactada.")

    return(monto)

  }

  deflactor_out <- try(purrr::map_dbl(year_out,
                                      function(x)
                                        deflactor[deflactor$year %in% as.numeric(x),]$deflactor_year),
                       silent = T)

  if (class(deflactor_out) == "try-error") {

    warning("Error en el año del monto deflactado. La cifra no fue deflactada.")

    return(monto)
  }

  deflactor_monto <- try(purrr::map_dbl(year_monto,
                                        function(x)
                                          deflactor[deflactor$year %in% as.numeric(x),]$deflactor_year),
                         silent = T)

  if (class(deflactor_monto) == "try-error") {
    warning("Error en el año al que se va a deflactar. La cifra no fue deflactada.")

    return(monto)
  }

  monto_deflactado <- (monto * deflactor_out) / deflactor_monto

  return(monto_deflactado)

}


#' Agrupación del presupuesto
#'
#' Esta función permite agrupar las bases de datos abiertas del presupuesto
#' de Transparencia Presupuestaria con los principales niveles de agregación.
#' De esta manera, podemos generar resumenes presupuestales de acuerdo a su
#' clasificación administrativa, funcional o económica.
#' Por ejemplo, permite generar rápidamente un dataframe con el presupuesto
#' en todas sus categorías disponibles(aprobado, modificado, pagado y
#' ejercido) por ramo.
#' Nota 1: esta función siempre mantiene el periodo indicado y, en caso de no
#' tenerlo, toma el año fiscal como periodo.
#' Nota 2: esta función no distingue los cambios de claves y términos a lo
#' largo del tiempo. Por ejemplo, si un programa cambió de modalidad U a S,
#' e intentamos agrupar el programa con su modalidad y clave presupuestal,
#' la función la va distinguir como un programa diferente. Otro ejemplo es que
#' esta función no distingue  los cambios en el tiempo de la en la
#' clasificación por objeto del gasto. Si una misma categoría tiene una
#' clave o un nombre distinto a lo largo del tiempo y se clasifica el
#' presupuesto por estas categorías, la función lo clasificará como
#' categorías diferentes.
#'
#' @param data data frame con la estructura de los archivos sobre presupuesto
#' de transparencia Presupuestaria
#' (https://www.transparenciapresupuestaria.gob.mx/es/PTP/Datos_Abiertos).
#' @param ... nombre de las variables por las cuales se quiere colapsar
#' la información. Estos nombre tienen que estar completamente en minúscula
#' sin caracteres especiales y separado por guiones bajos. Por ejemplo,
#' id_ramo, desc_ramo, id_modalidad, desc_modalidad, etc.
#'
#' @importFrom magrittr %>%
#' @return dataframe colapsado por las variables indicadas. En caso de no
#' indicarse ninguna variable, sólo se colapsará por año fiscal.
#' @export
sum_pef_tp <- function(data, ...,
                       periodo_col = NA,
                       keep_mensual = T) {

  data %>%
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


#' Agrupar lista de dataframes de presupuesto en formato long
#'
#' Esta función retoma la una función sum_pef (por lo cual le aplican las
#' mismas reglas) y la aplica a una lista de presupuesto. Esto ayuda cuando
#' se quiere generar datos históricos del presupuesto en formato long, los
#' cuales normalmente están en diferentes archivos.
#' Lo ideal es cargar todos los dataframes de presupuesto en una lista de
#' dataframes y seleccionar las variables por las que se quiere acumular la
#' información.
#' Por ejemplo. Si colocamos un vector de las cuentas públicas de cada año
#' fiscal sin señalar ninguna variable, esta función regresará el total
#' del presupuesto (sin neteo) de todos los ciclos fiscales incluídos.
#' Nota 1: si se acumulan dos dataframes del mismo año fiscal y no de le
#' distingue con una variable periodo, esta función sólo señalará el ciclo
#' fiscal sin distinguir si son de trimestress diferentes.
#' Nota 2: esta función no distingue los cambios de claves y términos a lo
#' largo del tiempo. Por ejemplo, si un programa cambió de modalidad U a S,
#' e intentamos agrupar el programa con su modalidad y clave presupuestal,
#' la función la va distinguir como un programa diferente. Otro ejemplo es que
#' esta función no distingue  los cambios en el tiempo de la en la
#' clasificación por objeto del gasto. Si una misma categoría tiene una
#' clave o un nombre distinto a lo largo del tiempo y se clasifica el
#' presupuesto por estas categorías, la función lo clasificará como
#' categorías diferentes.
#'
#' @param lista_df lista de dataframes con la estructura de los datos abiertos
#' de presupuesto de Transparencia Presupuestaria.
#' @param ... nombre de las variables por las cuales se quiere colapsar
#' la información. Estos nombre tienen que estar completamente en minúscula
#' sin caracteres especiales y separado por guiones bajos. Por ejemplo,
#' id_ramo, desc_ramo, id_modalidad, desc_modalidad, etc.
#'
#' @return dataframe colapsado por las variables indicadas. En caso de no
#' indicarse ninguna variable, sólo se colapsará por año fiscal.
#' @export
bind_pef_tp <- function(lista_df, ...) {

  purrr::map_dfr(lista_df, sum_pef_tp, ...)

}


#' Agrupar lista de dataframes de presupuesto en formato wide
#'
#' Esta función retoma la una función sum_pef_tp (por lo cual le aplican las
#' mismas reglas) y la aplica a una lista de presupuesto. Esto ayuda cuando
#' se quiere generar datos históricos del presupuesto en formato wide, los
#' cuales normalmente están en diferentes archivos.
#' Lo ideal es cargar todos los dataframes de presupuesto en una lista de
#' dataframes y seleccionar las variables por las que se quiere acumular la
#' información.
#' Por ejemplo. Si colocamos un vector de las cuentas públicas de cada año
#' fiscal sin señalar ninguna variable, esta función regresará el total
#' del presupuesto (sin neteo) de todos los ciclos fiscales incluídos.
#' Nota 1: si se acumulan dos dataframes del mismo año fiscal y no de le
#' distingue con una variable periodo, esta función sólo señalará el ciclo
#' fiscal sin distinguir si son de trimestress diferentes.
#' Nota 2: esta función no distingue los cambios de claves y términos a lo
#' largo del tiempo. Por ejemplo, si un programa cambió de modalidad U a S,
#' e intentamos agrupar el programa con su modalidad y clave presupuestal,
#' la función la va distinguir como un programa diferente. Otro ejemplo es que
#' esta función no distingue  los cambios en el tiempo de la en la
#' clasificación por objeto del gasto. Si una misma categoría tiene una
#' clave o un nombre distinto a lo largo del tiempo y se clasifica el
#' presupuesto por estas categorías, la función lo clasificará como
#' categorías diferentes.
#'
#' @param lista_df lista de dataframes con la estructura de los datos abiertos
#' de presupuesto de Transparencia Presupuestaria.
#' @param ... nombre de las variables por las cuales se quiere colapsar
#' la información. Estos nombre tienen que estar completamente en minúscula
#' sin caracteres especiales y separado por guiones bajos. Por ejemplo,
#' id_ramo, desc_ramo, id_modalidad, desc_modalidad, etc.
#'
#' @importFrom magrittr %>%
#' @return dataframe colapsado por las variables indicadas. En caso de no
#' indicarse ninguna variable, sólo se colapsará por año fiscal.
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
#' Esta función pretende clasificar categoría del presupuesto que deben ser
#' neteadas en caso de realizarse un análisis del presupuesto neto. Esta
#' clasificación la realiza sobre archivos presupuestales con la estructura
#' de los datos de presupuesto abierto de Transparencia Presupuestaria.
#' Ojo: esta función aún está en desarrollo y, hasta ahora, el neteo sólo
#' es correcto aplicado al presupuesto aprobado.
#'
#' @param data dataframe con la estructura de los datos abiertos de
#' presupuesto de Transparencia Presupuestaria.
#' @param ... modificación y creación de variables equivalente a dplyr::mutate
#'
#' @importFrom magrittr %>%
#' @return dataframe cuya descripción de ramo indica los casos en que esa
#' categoría del gasto debe ser neteada.
#' @export
netear_tp <- function(data, ..., keep_mensual = T) {
  data <- data %>%
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

  data %>%
    {
      if ("id_objeto_del_gasto" %in% names(.)) {
        mutate(
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
    dplyr::bind_rows(data %>%
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
#' Esta función convierte aquellas categorías del presupuesto que tienen
#' "Neteo" como descripción del ramo en valores negativos. Normalmente se
#' debería usar después de la función de netear_tp.
#'
#' @param data dataframe con la estructura de los datos abiertos de
#' presupuesto de Transparencia Presupuestaria.
#'
#' @importFrom magrittr %>%
#' @return dataframe cuyas categorías del gasto con descripción del ramo
#' como "Neteo" están en valores negativos.
#' @export
negative_neteo_tp <- function(data) {
  data %>%
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

#' Convertir clave del ramo a la clasificación administrativa de los ramos
#'
#' Nota 1: esta clasificación a la última actualización del año fiscal.
#' Por ejemplo, en esta clasificación ya se considera al ramo 49 (FGR)
#' como parte de los ramos autónomos.
#'
#' @param id_ramo clave númerica del ramo
#'
#' @return character con la clasificación administrativa del ramo
#' @export
id_ramo_to_tipo_ramo <- function(id_ramo) {

  y <- as.numeric(id_ramo)

  dplyr::case_when(
    y %in% c(1, 3, 22, 35,
                   41, 42, 43, 49, # TODO: cechar los casos en que el año hace variar la calsificación.
                   44, 40, 32) ~ "A. Ramos autónomos",
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

#' Convertir clave del ramo a descripción del ramo
#'
#' Nota 1: esta clasificación a la última actualización del año fiscal.
#' Por ejemplo, en esta clasificación ya se considera al ramo 49 (FGR)
#' como Fiscalía y no Procuraduría.
#'
#' @param id_ramo clave númerica del ramo
#'
#' @return character con la clasificación administrativa del ramo
#' @export
id_ramo_to_desc_ramo <- function(id_ramo) {

  y <- as.numeric(id_ramo)

  dplyr::case_when(
    y == 1 ~  "Poder Legislativo",
    y == 2 ~  "Oficina de la Presidencia de la República",
    y == 3 ~  "Poder Judicial",
    y == 4 ~  "Gobernación",
    y == 5 ~  "Relaciones Exteriores",
    y == 6 ~  "Hacienda y Crédito Público",
    y == 7 ~  "Defensa Nacional",
    y == 8 ~  "Agricultura y Desarrollo Rural",
    y == 9 ~  "Comunicaciones y Transportes",
    y == 10 ~	"Economía",
    y == 11 ~	"Educación Pública",
    y == 12 ~	"Salud",
    y == 13 ~	"Marina",
    y == 14 ~	"Trabajo y Previsión Social",
    y == 15 ~	"Desarrollo Agrario, Territorial y Urbano",
    y == 16 ~	"Medio Ambiente y Recursos Naturales",
    y == 18 ~	"Energía",
    y == 19 ~	"Aportaciones a Seguridad Social",
    y == 20 ~	"Bienestar",
    y == 21 ~	"Turismo",
    y == 22 ~	"Instituto Nacional Electoral",
    y == 23 ~	"Provisiones Salariales y Económicas",
    y == 24 ~	"Deuda Pública",
    y == 25 ~	"Previsiones y Aportaciones para los Sistemas de Educación Básica, Normal, Tecnológica y de Adultos",
    y == 27 ~	"Función Pública",
    y == 28 ~	"Participaciones a Entidades Federativas y Municipios",
    y == 30 ~	"Adeudos de Ejercicios Fiscales Anteriores",
    y == 31 ~	"Tribunales Agrarios",
    y == 32 ~	"Tribunal Federal de Justicia Administrativa",
    y == 33 ~	"Aportaciones Federales para Entidades Federativas y Municipios",
    y == 34 ~	"Erogaciones para los Programas de Apoyo a Ahorradores y Deudores de la Banca",
    y == 35 ~	"Comisión Nacional de los Derechos Humanos",
    y == 36 ~	"Seguridad y Protección Ciudadana",
    y == 37 ~	"Consejería Jurídica del Ejecutivo Federal",
    y == 38 ~	"Consejo Nacional de Ciencia y Tecnología",
    y == 40 ~	"Información Nacional Estadística y Geográfica",
    y == 41 ~	"Comisión Federal de Competencia Económica",
    y == 43 ~	"Instituto Federal de Telecomunicaciones",
    y == 44 ~	"Instituto Nacional de Transparencia, Acceso a la Información y Protección de Datos Personales",
    y == 45 ~	"Comisión Reguladora de Energía",
    y == 46 ~	"Comisión Nacional de Hidrocarburos",
    y == 47 ~	"Entidades no Sectorizadas",
    y == 48 ~	"Cultura",
    y == 49 ~	"Fiscalía General de la República",
    y == 50 ~	"Instituto Mexicano del Seguro Social",
    y == 51 ~	"Instituto de Seguridad y Servicios Sociales de los Trabajadores del Estado",
    y == 52 ~	"Petróleos Mexicanos",
    y == 53 ~	"Comisión Federal de Electricidad",
    T ~ NA_character_
  )
}


#' Convertir clave del ramo a abreviatura del ramo
#'
#' Nota 1: esta clasificación a la última actualización del año fiscal.
#' Por ejemplo, en esta clasificación ya se considera al ramo 49 (FGR)
#' como Fiscalía y no Procuraduría.
#'
#' @param id_ramo clave númerica del ramo
#'
#' @return character con la clasificación administrativa del ramo
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
    y == 24	~ "Deuda Pública",
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
    y == "ENERGIA" ~ "Sener", #PONER DESPUÉS DE
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
      stringr::str_detect(y, "PUBLICA") ~ "Deuda Pública",
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


#' Generar colúmna con clasificación de programable
#'
#' @param .x base de datos de presupuesto
#'
#' @return una base de datos de presupuesto con clasificación del gasto
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
#'
#' @return una base de datos de presupuesto con clasificación económica
#' @export
gen_clas_eco <- 
  function (data, ..., periodo_col = NA, keep_mensual = T) {
    
    data %>% janitor::clean_names() %>% {
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
        
        ., clasif_eco = dplyr::case_when(
          # GASTO CORRIENTE
          # Subsidios
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & str_starts(id_objeto_del_gasto, "43") ~ "Gasto corriente",
          # Servicios personales
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & str_starts(id_objeto_del_gasto, "1|83101|83102|83106|83107|83108|83109|83110|83111|83112|83113|83114|83115|83116|83117") ~ "Gasto corriente",
          # Gastos de operación
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & str_starts(id_objeto_del_gasto, "2|3") & !str_starts(id_objeto_del_gasto, "391|394|395|396|397|26106|32902|39908|39910") ~ "Gasto corriente",
          # Otros de corriente
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & 
            (
              (id_capitulo == "4000" & id_concepto != "4300" & id_concepto != "4500"& id_concepto != "4700") | # !str_starts(id_objeto_del_gasto, "43|45|47")) | 
                str_starts(id_objeto_del_gasto, "79|85|391|394|395|396|397|834|26106|32902|39908|39910|83103|83105|83118") |
                (str_starts(id_objeto_del_gasto, "471") & (id_ramo == 19 & (id_ur == "GYR" | id_ur == "GYN")))
            ) ~ "Gasto corriente",
          
          # PENSIONES Y JUBILACIONES
          str_starts(id_objeto_del_gasto, "45") | (str_starts(id_objeto_del_gasto, "471") & (id_ramo == 19 & (id_ur != "GYR" & id_ur != "GYN"))) ~ "Pensiones y jubilaciones",
          
          # GASTOS DE INVERSIÓN
          # Inversión física
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & str_starts(id_objeto_del_gasto, "1|2|5|6|3|4|79|85|831|834") & !str_starts(id_objeto_del_gasto, "39909|43") ~ "Gastos de inversión",
          # Subsidios
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & str_starts(id_objeto_del_gasto, "43") ~ "Gastos de inversión",
          # Otros de inversión
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & str_starts(id_objeto_del_gasto, "7|39909") & !str_starts(id_objeto_del_gasto, "79") ~ "Gastos de inversión",
          
          T ~ NA_character_
        )
        
      )
      
      
    }
    
  }