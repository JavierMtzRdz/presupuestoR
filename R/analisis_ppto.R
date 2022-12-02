#' Deflactar montos
#'
#' Esa función deflacta montos descargando la última versión de deflactores
#' publicada por Transparencia presupuestaria.
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

  if (fuente == "CP2021") {
    deflactor <- CP2021
  }

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
    warning("A\u00f1o no incluido supera el rango de a\u00f1os disponible. La cifra no fue deflactada.")

    return(monto)

  }

  deflactor_out <- try(purrr::map_dbl(year_out,
                                      function(x)
                                        deflactor[deflactor$year %in% as.numeric(x),]$deflactor_year),
                       silent = T)

  if (class(deflactor_out) == "try-error") {

    warning("Error en el a\u00f1o del monto deflactado. La cifra no fue deflactada.")

    return(monto)
  }

  deflactor_monto <- try(purrr::map_dbl(year_monto,
                                        function(x)
                                          deflactor[deflactor$year %in% as.numeric(x),]$deflactor_year),
                         silent = T)

  if (class(deflactor_monto) == "try-error") {
    warning("Error en el a\u00f1o al que se va a deflactar. La cifra no fue deflactada.")

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
    y == 6 ~ "Inversi\u00f3n p\u00fablica",
    y == 7 ~ "Inversiones financieras y otras provisiones",
    y == 8 ~ "Participaciones y aportaciones",
    y == 9 ~ "Deuda p\u00fablica ",
    T ~ NA_character_
  )
}


#' ID de concepto a descripción de concepto
#'
#' Esta función regresa la descripción del concepto poniendo el concepto correspondiente
#'
#' @param id_concepto la colúmna con la id del concepto.
#'
#' @return regresa una lista con la descripción del concepto.
#' @export
id_concepto_to_desc_concepto <- function(id_concepto) {

  y <- substr(id_concepto, 0,2)
  y <- as.numeric(y)

  dplyr::case_when(
    y == 11 ~ 'Remuneraciones al personal de car\u00e1cter permanente',
    y == 12 ~ 'Remuneraciones al personal de car\u00e1cter transitorio',
    y == 13 ~ 'Remuneraciones adicionales y especiales',
    y == 14 ~ 'Seguridad social',
    y == 15 ~ 'Otras prestaciones sociales y econ\u00f3micas',
    y == 16 ~ 'Previsiones',
    y == 17 ~ 'Pago de est\u00edmulos a servidores p\u00fablicos',
    y == 21 ~ 'Materiales de administraci\u00f3n, emisi\u00f3n de documentos y art\u00edculos oficiales',
    y == 22 ~ 'Alimentos y utensilios',
    y == 23 ~ 'Materias primas y materiales de producci\u00f3n y comercializaci\u00f3n',
    y == 24 ~ 'Materiales y art\u00edculos de construcci\u00f3n y de reparaci\u00f3n',
    y == 25 ~ 'Productos qu\u00edmicos, farmac\u00e9uticos y de laboratorio',
    y == 26 ~ 'Combustibles, lubricantes y aditivos',
    y == 27 ~ 'Vestuario, blancos, prendas de protecci\u00f3n y art\u00edculos deportivos',
    y == 28 ~ 'Materiales y suministros para seguridad',
    y == 29 ~ 'Herramientas, refacciones y accesorios menores',
    y == 31 ~ 'Servicios b\u00e1sicos',
    y == 32 ~ 'Servicios de arrendamiento',
    y == 33 ~ 'Servicios profesionales, cient\u00edficos, t\u00e9cnicos y otros servicios',
    y == 34 ~ 'Servicios financieros, bancarios y comerciales',
    y == 35 ~ 'Servicios de instalaci\u00f3n, reparaci\u00f3n, mantenimiento y conservaci\u00f3n',
    y == 36 ~ 'Servicios de comunicaci\u00f3n social y publicidad',
    y == 37 ~ 'Servicios de traslado y vi\u00e1ticos',
    y == 38 ~ 'Servicios oficiales',
    y == 39 ~ 'Otros servicios generales',
    y == 41 ~ 'Transferencias internas y asignaciones al sector p\u00fablico',
    y == 43 ~ 'Subsidios y subvenciones',
    y == 44 ~ 'Ayudas sociales',
    y == 45 ~ 'Pensiones y jubilaciones',
    y == 46 ~ 'Transferencias a fideicomisos, mandatos y otros an\u00e1logos',
    y == 47 ~ 'Transferencias a la seguridad social',
    y == 48 ~ 'Donativos',
    y == 49 ~ 'Transferencias al exterior',
    y == 51 ~ 'Mobiliario y equipo de administraci\u00f3n',
    y == 52 ~ 'Mobiliario y equipo educacional y recreativo',
    y == 53 ~ 'Equipo e instrumental medico y de laboratorio',
    y == 53 ~ 'Equipo e instrumental m\u00e9dico y de laboratorio',
    y == 54 ~ 'Veh\u00edculos y equipo de transporte',
    y == 56 ~ 'Maquinaria, otros equipos y herramientas',
    y == 58 ~ 'Bienes inmuebles',
    y == 59 ~ 'Activos intangibles',
    y == 62 ~ 'Obra p\u00fablica en bienes propios',
    y == 72 ~ 'Acciones y participaciones de capital',
    y == 73 ~ 'Compra de t\u00edtulos y valores',
    y == 79 ~ 'Provisiones para contingencias y otras erogaciones especiales',
    y == 81 ~ 'Participaciones',
    y == 83 ~ 'Aportaciones',
    y == 85 ~ 'Convenios',
    y == 92 ~ 'Intereses de la deuda p\u00fablica',
    y == 93 ~ 'Comisiones de la deuda p\u00fablica',
    y == 94 ~ 'Gastos de la deuda p\u00fablica',
    y == 95 ~ 'Costo por coberturas',
    y == 96 ~ 'Apoyos financieros',
    y == 99 ~ 'Adeudos de ejercicios fiscales anteriores (ADEFAS)',
    T ~ NA_character_
  )
}


#' ID de partida genérica a descripción de partida genérica
#'
#' Esta función regresa la descripción del partida genérica poniendo el partida genérica correspondiente
#'
#' @param id_partida_generica la colúmna con la id del partida genérica.
#'
#' @return regresa una lista con la descripción del partida genérica.
#' @export
id_part_gen_to_desc_part_gen <- function(id_partida_generica) {

  y <- substr(id_partida_generica, 0,3)
  y <- as.numeric(y)

  dplyr::case_when(
    y == 111 ~ 'Dietas',
    y == 112 ~ 'Haberes',
    y == 113 ~ 'Sueldos base al personal permanente',
    y == 114 ~ 'Remuneraciones por adscripci\u00f3n laboral en el extranjero',
    y == 121 ~ 'Honorarios asimilables a salarios',
    y == 122 ~ 'Sueldos base al personal eventual',
    y == 123 ~ 'Retribuciones por servicios de car\u00e1cter social',
    y == 124 ~ 'Retribuci\u00f3n a los representantes de los trabajadores y de los patrones en la Junta de Conciliaci\u00f3n y Arbitraje',
    y == 131 ~ 'Primas por a\u00f1os de servicios efectivos prestados',
    y == 132 ~ 'Primas de vacaciones, dominical y gratificaci\u00f3n de fin de a\u00f1o',
    y == 133 ~ 'Horas extraordinarias',
    y == 134 ~ 'Compensaciones',
    y == 135 ~ 'Sobrehaberes',
    y == 136 ~ 'Asignaciones de t\u00e9cnico, de mando, por comisi\u00f3n, de vuelo y de t\u00e9cnico especial',
    y == 141 ~ 'Aportaciones de seguridad social',
    y == 142 ~ 'Aportaciones a fondos de vivienda',
    y == 143 ~ 'Aportaciones al sistema para el retiro',
    y == 144 ~ 'Aportaciones para seguros',
    y == 151 ~ 'Cuotas para el fondo de ahorro y fondo de trabajo',
    y == 152 ~ 'Indemnizaciones',
    y == 153 ~ 'Prestaciones y haberes de retiro',
    y == 154 ~ 'Prestaciones contractuales',
    y == 155 ~ 'Apoyos a la capacitaci\u00f3n de los servidores p\u00fablicos',
    y == 159 ~ 'Otras prestaciones sociales y econ\u00f3micas',
    y == 161 ~ 'Previsiones de car\u00e1cter laboral, econ\u00f3mica y de seguridad social',
    y == 171 ~ 'Est\u00edmulos',
    y == 211 ~ 'Materiales, \u00fatiles y equipos menores de oficina',
    y == 212 ~ 'Materiales y \u00fatiles de impresi\u00f3n y reproducci\u00f3n',
    y == 213 ~ 'Material estad\u00edstico y geogr\u00e1fico',
    y == 214 ~ 'Materiales, \u00fatiles y equipos menores de tecnolog\u00edas de la informaci\u00f3n y comunicaciones',
    y == 215 ~ 'Material impreso e informaci\u00f3n digital',
    y == 216 ~ 'Material de limpieza',
    y == 217 ~ 'Materiales y \u00fatiles de ense\u00f1anza',
    y == 218 ~ 'Materiales para el registro e identificaci\u00f3n de bienes y personas',
    y == 221 ~ 'Productos alimenticios para personas',
    y == 222 ~ 'Productos alimenticios para animales',
    y == 223 ~ 'Utensilios para el servicio de alimentaci\u00f3n',
    y == 231 ~ 'Productos alimenticios, agropecuarios y forestales adquiridos como materia prima',
    y == 232 ~ 'Insumos textiles adquiridos como materia prima',
    y == 233 ~ 'Productos de papel, cart\u00f3n e impresos adquiridos como materia prima',
    y == 234 ~ 'Combustibles, lubricantes, aditivos, carb\u00f3n y sus derivados adquiridos como materia prima',
    y == 235 ~ 'Productos qu\u00edmicos, farmac\u00e9uticos y de laboratorio adquiridos como materia prima',
    y == 236 ~ 'Productos met\u00e1licos y a base de minerales no met\u00e1licos adquiridos como materia prima',
    y == 237 ~ 'Productos de cuero, piel, pl\u00e1stico y hule adquiridos como materia prima',
    y == 238 ~ 'Mercanc\u00edas adquiridas para su comercializaci\u00f3n',
    y == 239 ~ 'Otros productos adquiridos como materia prima',
    y == 241 ~ 'Productos minerales no met\u00e1licos',
    y == 242 ~ 'Cemento y productos de concreto',
    y == 243 ~ 'Cal, yeso y productos de yeso',
    y == 244 ~ 'Madera y productos de madera',
    y == 245 ~ 'Vidrio y productos de vidrio',
    y == 246 ~ 'Material el\u00e9ctrico y electr\u00f3nico',
    y == 247 ~ 'Art\u00edculos met\u00e1licos para la construcci\u00f3n',
    y == 248 ~ 'Materiales complementarios',
    y == 249 ~ 'Otros materiales y art\u00edculos de construcci\u00f3n y reparaci\u00f3n',
    y == 251 ~ 'Productos qu\u00edmicos b\u00e1sicos',
    y == 252 ~ 'Fertilizantes, pesticidas y otros agroqu\u00edmicos',
    y == 253 ~ 'Medicinas y productos farmac\u00e9uticos',
    y == 254 ~ 'Materiales, accesorios y suministros m\u00e9dicos',
    y == 255 ~ 'Materiales, accesorios y suministros de laboratorio',
    y == 256 ~ 'Fibras sint\u00e9ticas, hules, pl\u00e1sticos y derivados',
    y == 259 ~ 'Otros productos qu\u00edmicos',
    y == 261 ~ 'Combustibles, lubricantes y aditivos',
    y == 271 ~ 'Vestuario y uniformes',
    y == 272 ~ 'Prendas de seguridad y protecci\u00f3n personal',
    y == 273 ~ 'Art\u00edculos deportivos',
    y == 274 ~ 'Productos textiles',
    y == 275 ~ 'Blancos y otros productos textiles, excepto prendas de vestir',
    y == 281 ~ 'Sustancias y materiales explosivos',
    y == 282 ~ 'Materiales de seguridad p\u00fablica',
    y == 283 ~ 'Prendas de protecci\u00f3n para seguridad p\u00fablica y nacional',
    y == 291 ~ 'Herramientas menores',
    y == 292 ~ 'Refacciones y accesorios menores de edificios',
    y == 293 ~ 'Refacciones y accesorios menores de mobiliario y equipo de administraci\u00f3n, educacional y recreativo',
    y == 294 ~ 'Refacciones y accesorios menores de equipo de c\u00f3mputo y tecnolog\u00edas de la informaci\u00f3n',
    y == 295 ~ 'Refacciones y accesorios menores de equipo e instrumental m\u00e9dico y de laboratorio',
    y == 296 ~ 'Refacciones y accesorios menores de equipo de transporte',
    y == 297 ~ 'Refacciones y accesorios menores de equipo de defensa y seguridad',
    y == 298 ~ 'Refacciones y accesorios menores de maquinaria y otros equipos',
    y == 299 ~ 'Refacciones y accesorios menores otros bienes muebles',
    y == 311 ~ 'Energ\u00eda el\u00e9ctrica',
    y == 312 ~ 'Gas',
    y == 313 ~ 'Agua',
    y == 314 ~ 'Telefon\u00eda tradicional',
    y == 315 ~ 'Telefon\u00eda celular',
    y == 316 ~ 'Servicios de telecomunicaciones y sat\u00e9lites',
    y == 317 ~ 'Servicios de acceso de Internet, redes y procesamiento de informaci\u00f3n',
    y == 318 ~ 'Servicios postales y telegr\u00e1ficos',
    y == 319 ~ 'Servicios integrales y otros servicios',
    y == 321 ~ 'Arrendamiento de terrenos',
    y == 322 ~ 'Arrendamiento de edificios',
    y == 323 ~ 'Arrendamiento de mobiliario y equipo de administraci\u00f3n, educacional y recreativo',
    y == 324 ~ 'Arrendamiento de equipo e instrumental m\u00e9dico y de laboratorio',
    y == 325 ~ 'Arrendamiento de equipo de transporte',
    y == 326 ~ 'Arrendamiento de maquinaria, otros equipos y herramientas',
    y == 327 ~ 'Arrendamiento de activos intangibles',
    y == 329 ~ 'Otros arrendamientos',
    y == 331 ~ 'Servicios legales, de contabilidad, auditor\u00eda y relacionados',
    y == 332 ~ 'Servicios de dise\u00f1o, arquitectura, ingenier\u00eda y actividades relacionadas',
    y == 333 ~ 'Servicios de consultor\u00eda administrativa, procesos, t\u00e9cnica y en tecnolog\u00edas de la informaci\u00f3n',
    y == 334 ~ 'Servicios de capacitaci\u00f3n',
    y == 335 ~ 'Servicios de investigaci\u00f3n cient\u00edfica y desarrollo',
    y == 336 ~ 'Servicios de apoyo administrativo, traducci\u00f3n, fotocopiado e impresi\u00f3n',
    y == 337 ~ 'Servicios de protecci\u00f3n y seguridad',
    y == 338 ~ 'Servicios de vigilancia',
    y == 339 ~ 'Servicios profesionales, cient\u00edficos y t\u00e9cnicos integrales',
    y == 341 ~ 'Servicios financieros y bancarios',
    y == 343 ~ 'Servicios de recaudaci\u00f3n, traslado y custodia de valores',
    y == 344 ~ 'Seguros de responsabilidad patrimonial y fianzas',
    y == 345 ~ 'Seguro de bienes patrimoniales',
    y == 346 ~ 'Almacenaje, envase y embalaje',
    y == 347 ~ 'Fletes y maniobras',
    y == 348 ~ 'Comisiones por ventas',
    y == 349 ~ 'Servicios financieros, bancarios y comerciales integrales',
    y == 351 ~ 'Conservaci\u00f3n y mantenimiento menor de inmuebles',
    y == 352 ~ 'Instalaci\u00f3n, reparaci\u00f3n y mantenimiento de mobiliario y equipo de administraci\u00f3n, educacional y recreativo',
    y == 353 ~ 'Instalaci\u00f3n, reparaci\u00f3n y mantenimiento de equipo de c\u00f3mputo y tecnolog\u00eda de la informaci\u00f3n',
    y == 354 ~ 'Instalaci\u00f3n, reparaci\u00f3n y mantenimiento de equipo e instrumental m\u00e9dico y de laboratorio',
    y == 355 ~ 'Reparaci\u00f3n y mantenimiento de equipo de transporte',
    y == 356 ~ 'Reparaci\u00f3n y mantenimiento de equipo de defensa y seguridad',
    y == 357 ~ 'Instalaci\u00f3n, reparaci\u00f3n y mantenimiento de maquinaria, otros equipos y herramienta',
    y == 358 ~ 'Servicios de limpieza y manejo de desechos',
    y == 359 ~ 'Servicios de jardiner\u00eda y fumigaci\u00f3n',
    y == 361 ~ 'Difusi\u00f3n por radio, televisi\u00f3n y otros medios de mensajes sobre programas y actividades gubernamentales',
    y == 362 ~ 'Difusi\u00f3n por radio, televisi\u00f3n y otros medios de mensajes comerciales para promover la venta de bienes o servicios',
    y == 363 ~ 'Servicios de creatividad, preproducci\u00f3n y producci\u00f3n de publicidad, excepto Internet',
    y == 366 ~ 'Servicio de creaci\u00f3n y difusi\u00f3n de contenido exclusivamente a trav\u00e9s de Internet',
    y == 369 ~ 'Otros servicios de informaci\u00f3n',
    y == 371 ~ 'Pasajes a\u00e9reos',
    y == 372 ~ 'Pasajes terrestres',
    y == 373 ~ 'Pasajes mar\u00edtimos, lacustres y fluviales',
    y == 375 ~ 'Vi\u00e1ticos en el pa\u00eds',
    y == 376 ~ 'Vi\u00e1ticos en el extranjero',
    y == 377 ~ 'Gastos de instalaci\u00f3n y traslado de menaje',
    y == 378 ~ 'Servicios integrales de traslado y vi\u00e1ticos',
    y == 379 ~ 'Otros servicios de traslado y hospedaje',
    y == 381 ~ 'Gastos de ceremonial',
    y == 382 ~ 'Gastos de orden social y cultural',
    y == 383 ~ 'Congresos y convenciones',
    y == 384 ~ 'Exposiciones',
    y == 385 ~ 'Gastos de representaci\u00f3n',
    y == 391 ~ 'Servicios funerarios y de cementerios',
    y == 392 ~ 'Impuestos y derechos',
    y == 393 ~ 'Impuestos y derechos de importaci\u00f3n',
    y == 394 ~ 'Sentencias y resoluciones por autoridad competente',
    y == 395 ~ 'Penas, multas, accesorios y actualizaciones',
    y == 396 ~ 'Otros gastos por responsabilidades',
    y == 398 ~ 'Impuesto sobre n\u00f3minas y otros que se deriven de una relaci\u00f3n laboral',
    y == 399 ~ 'Otros servicios generales',
    y == 415 ~ 'Transferencias internas otorgadas a entidades paraestatales no empresariales y no financieras',
    y == 431 ~ 'Subsidios a la producci\u00f3n',
    y == 433 ~ 'Subsidios a la inversi\u00f3n',
    y == 434 ~ 'Subsidios a la prestaci\u00f3n de servicios p\u00fablicos',
    y == 437 ~ 'Subvenciones al consumo',
    y == 438 ~ 'Subsidios a Entidades Federativas y Municipios',
    y == 439 ~ 'Otros Subsidios',
    y == 441 ~ 'Ayudas sociales a personas',
    y == 442 ~ 'Becas y otras ayudas para programas de capacitaci\u00f3n',
    y == 444 ~ 'Ayudas sociales a actividades cient\u00edficas o acad\u00e9micas',
    y == 445 ~ 'Ayudas sociales a instituciones sin fines de lucro',
    y == 448 ~ 'Ayudas por desastres naturales y otros siniestros',
    y == 452 ~ 'Jubilaciones',
    y == 459 ~ 'Otras pensiones y jubilaciones',
    y == 461 ~ 'Transferencias a fideicomisos del Poder Ejecutivo',
    y == 471 ~ 'Transferencias por obligaci\u00f3n de ley',
    y == 481 ~ 'Donativos a instituciones sin fines de lucro',
    y == 482 ~ 'Donativos a entidades federativas',
    y == 485 ~ 'Donativos internacionales',
    y == 491 ~ 'Transferencias para gobiernos extranjeros',
    y == 492 ~ 'Transferencias para organismos internacionales',
    y == 511 ~ 'Muebles de oficina y estanter\u00eda',
    y == 512 ~ 'Muebles, excepto de oficina y estanter\u00eda',
    y == 513 ~ 'Bienes art\u00edsticos, culturales y cient\u00edficos',
    y == 515 ~ 'Equipo de c\u00f3mputo y de tecnolog\u00edas de la informaci\u00f3n',
    y == 519 ~ 'Otros mobiliarios y equipos de administraci\u00f3n',
    y == 521 ~ 'Equipos y aparatos audiovisuales',
    y == 522 ~ 'Aparatos deportivos',
    y == 523 ~ 'C\u00e1maras fotogr\u00e1ficas y de video',
    y == 529 ~ 'Otro mobiliario y equipo educacional y recreativo',
    y == 531 ~ 'Equipo m\u00e9dico y de laboratorio',
    y == 532 ~ 'Instrumental m\u00e9dico y de laboratorio',
    y == 541 ~ 'Veh\u00edculos y Equipo Terrestre',
    y == 542 ~ 'Carrocer\u00edas y remolques',
    y == 544 ~ 'Equipo ferroviario',
    y == 545 ~ 'Embarcaciones',
    y == 561 ~ 'Maquinaria y equipo agropecuario',
    y == 562 ~ 'Maquinaria y equipo industrial',
    y == 563 ~ 'Maquinaria y equipo de construcci\u00f3n',
    y == 564 ~ 'Sistemas de aire acondicionado, calefacci\u00f3n y de refrigeraci\u00f3n industrial y comercial',
    y == 565 ~ 'Equipo de comunicaci\u00f3n y telecomunicaci\u00f3n',
    y == 566 ~ 'Equipos de generaci\u00f3n el\u00e9ctrica, aparatos y accesorios el\u00e9ctricos',
    y == 567 ~ 'Herramientas y m\u00e1quinas-herramienta',
    y == 569 ~ 'Otros equipos',
    y == 581 ~ 'Terrenos',
    y == 583 ~ 'Edificios no residenciales',
    y == 589 ~ 'Otros bienes inmuebles',
    y == 591 ~ 'Software',
    y == 597 ~ 'Licencias inform\u00e1ticas e intelectuales',
    y == 622 ~ 'Edificaci\u00f3n no habitacional',
    y == 623 ~ 'Construcci\u00f3n de obras para el abastecimiento de agua, petr\u00f3leo, gas, electricidad y telecomunicaciones',
    y == 624 ~ 'Divisi\u00f3n de terrenos y construcci\u00f3n de obras de urbanizaci\u00f3n',
    y == 625 ~ 'Construcci\u00f3n de v\u00edas de comunicaci\u00f3n',
    y == 626 ~ 'Otras construcciones de ingenier\u00eda civil u obra pesada',
    y == 627 ~ 'Instalaciones y equipamiento en construcciones',
    y == 629 ~ 'Trabajos de acabados en edificaciones y otros trabajos especializados',
    y == 725 ~ 'Acciones y participaciones de capital en organismos internacionales con fines de pol\u00edtica econ\u00f3mica',
    y == 739 ~ 'Otros valores',
    y == 799 ~ 'Otras erogaciones especiales',
    y == 811 ~ 'Fondo general de participaciones',
    y == 812 ~ 'Fondo de fomento municipal',
    y == 814 ~ 'Otros conceptos participables de la Federaci\u00f3n a entidades federativas',
    y == 831 ~ 'Aportaciones de la Federaci\u00f3n a las entidades federativas',
    y == 835 ~ 'Aportaciones previstas en leyes y decretos compensatorias a entidades federativas y municipios',
    y == 851 ~ 'Convenios de reasignaci\u00f3n',
    y == 921 ~ 'Intereses de la deuda interna con instituciones de cr\u00e9dito',
    y == 922 ~ 'Intereses derivados de la colocaci\u00f3n de t\u00edtulos y valores',
    y == 924 ~ 'Intereses de la deuda externa con instituciones de cr\u00e9dito',
    y == 925 ~ 'Intereses de la deuda con organismos financieros Internacionales',
    y == 926 ~ 'Intereses de la deuda bilateral',
    y == 927 ~ 'Intereses derivados de la colocaci\u00f3n de t\u00edtulos y valores en el exterior',
    y == 931 ~ 'Comisiones de la deuda p\u00fablica interna',
    y == 932 ~ 'Comisiones de la deuda p\u00fablica externa',
    y == 941 ~ 'Gastos de la deuda p\u00fablica interna',
    y == 942 ~ 'Gastos de la deuda p\u00fablica externa',
    y == 951 ~ 'Costos por coberturas',
    y == 962 ~ 'Apoyos a ahorradores y deudores del Sistema Financiero Nacional',
    y == 991 ~ 'ADEFAS',
    T ~ NA_character_
  )
}


#' ID de partida específica a descripción de partida específica
#'
#' Esta función regresa la descripción del partida específica poniendo el partida específica correspondiente
#'
#' @param id_partida_especifica la colúmna con la id del partida específica.
#'
#' @return regresa una lista con la descripción del partida específica.
#' @export
id_part_esp_to_desc_part_esp <- function(id_partida_especifica) {

  y <- as.numeric(id_partida_especifica)

  dplyr::case_when(
    y == 11101 ~ 'Dietas (Ramos Aut\u00f3nomos)',
    y == 11201 ~ 'Haberes',
    y == 11301 ~ 'Sueldos base',
    y == 11401 ~ 'Retribuciones por adscripci\u00f3n en el extranjero',
    y == 12101 ~ 'Honorarios',
    y == 12201 ~ 'Remuneraciones al personal eventual',
    y == 12201 ~ 'Sueldos base al personal eventual',
    y == 12202 ~ 'Compensaciones a sustitutos de profesores',
    y == 12301 ~ 'Retribuciones por servicios de car\u00e1cter social',
    y == 12301 ~ 'Retribuciones por servicios en per\u00edodo de formaci\u00f3n profesional',
    y == 12401 ~ 'Retribuci\u00f3n a los representantes de los trabajadores y de los patrones en la Junta Federal de Conciliaci\u00f3n y Arbitraje',


    y == 13101 ~ 'Prima quinquenal por a\u00f1os de servicios efectivos prestados',
    y == 13102 ~ 'Acreditaci\u00f3n por a\u00f1os de servicio en la docencia y al personal administrativo de las instituciones de educaci\u00f3n superior',
    y == 13103 ~ 'Prima de perseverancia por a\u00f1os de servicio activo en el Ej\u00e9rcito, Fuerza A\u00e9rea y Armada Mexicanos',
    y == 13104 ~ 'Antig\u00fcedad',
    y == 13201 ~ 'Primas de vacaciones y dominical',
    y == 13202 ~ 'Aguinaldo o gratificaci\u00f3n de fin de a\u00f1o',
    y == 13204 ~ 'Primas de vacaciones y dominical de \u00e1reas administrativas (Ramos Aut\u00f3nomos)',
    y == 13301 ~ 'Remuneraciones por horas extraordinarias',
    y == 13401 ~ 'Acreditaci\u00f3n por titulaci\u00f3n en la docencia',
    y == 13402 ~ 'Acreditaci\u00f3n al personal docente por a\u00f1os de estudio de licenciatura',
    y == 13402 ~ 'Compensaciones adicionales por servicios especiales',
    y == 13403 ~ 'Compensaciones por servicios especiales',
    y == 13404 ~ 'Compensaciones por servicios eventuales',
    y == 13405 ~ 'Compensaciones de retiro',
    y == 13406 ~ 'Compensaciones de servicios',
    y == 13407 ~ 'Compensaciones adicionales por servicios especiales',
    y == 13408 ~ 'Asignaciones docentes, pedag\u00f3gicas gen\u00e9ricas y espec\u00edficas',
    y == 13409 ~ 'Compensaci\u00f3n por adquisici\u00f3n de material did\u00e1ctico',
    y == 13410 ~ 'Compensaci\u00f3n por actualizaci\u00f3n y formaci\u00f3n acad\u00e9mica',
    y == 13411 ~ 'Compensaciones a m\u00e9dicos residentes',
    y == 13412 ~ 'Gastos contingentes para el personal radicado en el extranjero',
    y == 13413 ~ 'Asignaciones para la conclusi\u00f3n de servicios en la Administraci\u00f3n P\u00fablica Federal',
    y == 13414 ~ 'Asignaciones conforme al r\u00e9gimen laboral',
    y == 13501 ~ 'Sobrehaberes',
    y == 13601 ~ 'Asignaciones de t\u00e9cnico',
    y == 13602 ~ 'Asignaciones de mando',
    y == 13604 ~ 'Asignaciones de vuelo',
    y == 13605 ~ 'Asignaciones de t\u00e9cnico especial',


    y == 14101 ~ 'Aportaciones al ISSSTE',
    y == 14102 ~ 'Aportaciones al ISSFAM',
    y == 14103 ~ 'Aportaciones al IMSS',
    y == 14104 ~ 'Aportaciones de seguridad social contractuales',
    y == 14105 ~ 'Aportaciones al seguro de cesant\u00eda en edad avanzada y vejez',
    y == 14201 ~ 'Aportaciones al FOVISSSTE',
    y == 14202 ~ 'Aportaciones al INFONAVIT',
    y == 14301 ~ 'Aportaciones al Sistema de Ahorro para el Retiro',
    y == 14302 ~ 'Dep\u00f3sitos para el ahorro solidario',
    y == 14401 ~ 'Cuotas para el seguro de vida del personal civil',
    y == 14402 ~ 'Cuotas para el seguro de vida del personal militar',
    y == 14403 ~ 'Cuotas para el seguro de gastos m\u00e9dicos del personal civil',
    y == 14404 ~ 'Cuotas para el seguro de separaci\u00f3n individualizado',
    y == 14405 ~ 'Cuotas para el seguro colectivo de retiro',
    y == 14406 ~ 'Seguro de responsabilidad civil, asistencia legal y otros seguros',
    y == 15101 ~ 'Cuotas para el fondo de ahorro del personal civil',
    y == 15102 ~ 'Cuotas para el fondo de ahorro de generales, almirantes, jefes y oficiales',
    y == 15103 ~ 'Cuotas para el fondo de trabajo del personal del Ej\u00e9rcito, Fuerza A\u00e9rea y Armada Mexicanos',
    y == 15201 ~ 'Indemnizaciones por accidentes en el trabajo',
    y == 15202 ~ 'Pago de liquidaciones',
    y == 15301 ~ 'Prestaciones de retiro',
    y == 15401 ~ 'Prestaciones establecidas por condiciones generales de trabajo o contratos colectivos de trabajo',
    y == 15402 ~ 'Compensaci\u00f3n garantizada',
    y == 15403 ~ 'Asignaciones adicionales al sueldo',
    y == 15405 ~ 'Compensaci\u00f3n de Apoyo (Ramos Aut\u00f3nomos)',
    y == 15501 ~ 'Apoyos a la capacitaci\u00f3n de los servidores p\u00fablicos',
    y == 15901 ~ 'Otras prestaciones',
    y == 15902 ~ 'Pago extraordinario por riesgo',


    y == 16101 ~ 'Incrementos a las percepciones',
    y == 16102 ~ 'Creaci\u00f3n de plazas',
    y == 16103 ~ 'Otras medidas de car\u00e1cter laboral y econ\u00f3mico',
    y == 16104 ~ 'Previsiones para aportaciones al ISSSTE',
    y == 16105 ~ 'Previsiones para aportaciones al FOVISSSTE',
    y == 16106 ~ 'Previsiones para aportaciones al Sistema de Ahorro para el Retiro',
    y == 16107 ~ 'Previsiones para aportaciones al seguro de cesant\u00eda en edad avanzada y vejez',
    y == 16108 ~ 'Previsiones para los dep\u00f3sitos al ahorro solidario',
    y == 17101 ~ 'Est\u00edmulos por productividad y eficiencia',
    y == 17102 ~ 'Est\u00edmulos al personal operativo',
    y == 21101 ~ 'Materiales y \u00fatiles de oficina',
    y == 21102 ~ 'Material Electoral',
    y == 21199 ~ 'Materiales de administraci\u00f3n, emisi\u00f3n de documentos y art\u00edculos oficiales',
    y == 21201 ~ 'Materiales y \u00fatiles de impresi\u00f3n y reproducci\u00f3n',
    y == 21301 ~ 'Material estad\u00edstico y geogr\u00e1fico',
    y == 21401 ~ 'Materiales y \u00fatiles consumibles para el procesamiento en equipos y bienes inform\u00e1ticos',
    y == 21401 ~ 'Materiales y \u00fatiles para el procesamiento en equipos y bienes inform\u00e1ticos',
    y == 21501 ~ 'Material de apoyo informativo',
    y == 21502 ~ 'Material para informaci\u00f3n en actividades de investigaci\u00f3n cient\u00edfica y tecnol\u00f3gica',
    y == 21601 ~ 'Material de limpieza',
    y == 21701 ~ 'Materiales y suministros para planteles educativos',
    y == 21801 ~ 'Materiales para el registro e identificaci\u00f3n de bienes y personas (Ramos Aut\u00f3nomos)',
    y == 22101 ~ 'Productos alimenticios para el Ej\u00e9rcito, Fuerza A\u00e9rea y Armada Mexicanos, y para los efectivos que participen en programas de seguridad p\u00fablica',
    y == 22102 ~ 'Productos alimenticios para personas derivado de la prestaci\u00f3n de servicios p\u00fablicos en unidades de salud, educativas, de readaptaci\u00f3n social y otras',
    y == 22103 ~ 'Productos alimenticios para el personal que realiza labores en campo o de supervisi\u00f3n',
    y == 22104 ~ 'Productos alimenticios para el personal en las instalaciones de las dependencias y entidades',
    y == 22106 ~ 'Productos alimenticios para el personal derivado de actividades extraordinarias',
    y == 22199 ~ 'Alimentos y utensilios',
    y == 22201 ~ 'Productos alimenticios para animales',
    y == 22301 ~ 'Utensilios para el servicio de alimentaci\u00f3n',
    y == 23101 ~ 'Productos alimenticios, agropecuarios y forestales adquiridos como materia prima',
    y == 23201 ~ 'Insumos textiles adquiridos como materia prima',
    y == 23301 ~ 'Productos de papel, cart\u00f3n e impresos adquiridos como materia prima',
    y == 23401 ~ 'Combustibles, lubricantes, aditivos, carb\u00f3n y sus derivados adquiridos como materia prima',
    y == 23501 ~ 'Productos qu\u00edmicos, farmac\u00e9uticos y de laboratorio adquiridos como materia prima',
    y == 23601 ~ 'Productos met\u00e1licos y a base de minerales no met\u00e1licos adquiridos como materia prima',
    y == 23701 ~ 'Productos de cuero, piel, pl\u00e1stico y hule adquiridos como materia prima',
    y == 23801 ~ 'Mercanc\u00edas para su comercializaci\u00f3n en tiendas del sector p\u00fablico',
    y == 23901 ~ 'Otros productos adquiridos como materia prima',
    y == 23902 ~ 'Petr\u00f3leo, gas y sus derivados adquiridos como materia prima',
    y == 24101 ~ 'Productos minerales no met\u00e1licos',
    y == 24199 ~ 'Materiales y art\u00edculos de construcci\u00f3n y de reparaci\u00f3n',
    y == 24201 ~ 'Cemento y productos de concreto',
    y == 24301 ~ 'Cal, yeso y productos de yeso',
    y == 24401 ~ 'Madera y productos de madera',
    y == 24501 ~ 'Vidrio y productos de vidrio',
    y == 24601 ~ 'Material el\u00e9ctrico y electr\u00f3nico',
    y == 24701 ~ 'Art\u00edculos met\u00e1licos para la construcci\u00f3n',
    y == 24801 ~ 'Materiales complementarios',
    y == 24901 ~ 'Otros materiales y art\u00edculos de construcci\u00f3n y reparaci\u00f3n',
    y == 25101 ~ 'Productos qu\u00edmicos b\u00e1sicos',
    y == 25199 ~ 'Productos qu\u00edmicos, farmac\u00e9uticos y de laboratorio',
    y == 25201 ~ 'Plaguicidas, abonos y fertilizantes',
    y == 25301 ~ 'Medicinas y productos farmac\u00e9uticos',
    y == 25401 ~ 'Materiales, accesorios y suministros m\u00e9dicos',
    y == 25501 ~ 'Materiales, accesorios y suministros de laboratorio',
    y == 25601 ~ 'Fibras sint\u00e9ticas, hules, pl\u00e1sticos y derivados',
    y == 25601 ~ 'Fibras sint\u00e9ticas, hules, pl\u00e1sticos y derivados (Ramos Aut\u00f3nomos)',
    y == 25901 ~ 'Otros productos qu\u00edmicos',
    y == 26101 ~ 'Combustibles, lubricantes y aditivos para veh\u00edculos terrestres, a\u00e9reos, mar\u00edtimos, lacustres y fluviales destinados a la ejecuci\u00f3n de programas de seguridad p\u00fablica y nacional',
    y == 26102 ~ 'Combustibles, lubricantes y aditivos para veh\u00edculos terrestres, a\u00e9reos, mar\u00edtimos, lacustres y fluviales destinados a servicios p\u00fablicos y la operaci\u00f3n de programas p\u00fablicos',
    y == 26103 ~ 'Combustibles, lubricantes y aditivos para veh\u00edculos terrestres, a\u00e9reos, mar\u00edtimos, lacustres y fluviales destinados a servicios administrativos',
    y == 26104 ~ 'Combustibles, lubricantes y aditivos para veh\u00edculos terrestres, a\u00e9reos, mar\u00edtimos, lacustres y fluviales asignados a servidores p\u00fablicos',
    y == 26105 ~ 'Combustibles, lubricantes y aditivos para maquinaria, equipo de producci\u00f3n y servicios administrativos',
    y == 26106 ~ 'PIDIREGAS cargos variables',
    y == 26107 ~ 'Combustibles nacionales para plantas productivas',
    y == 26108 ~ 'Combustibles de importaci\u00f3n para plantas productivas',
    y == 26199 ~ 'Combustibles, lubricantes y aditivos',
    y == 27101 ~ 'Vestuario y uniformes',
    y == 27199 ~ 'Vestuario, blancos, prendas de protecci\u00f3n y art\u00edculos deportivos',
    y == 27201 ~ 'Prendas de protecci\u00f3n personal',
    y == 27301 ~ 'Art\u00edculos deportivos',
    y == 27401 ~ 'Productos textiles',
    y == 27501 ~ 'Blancos y otros productos textiles, excepto prendas de vestir',
    y == 28101 ~ 'Sustancias y materiales explosivos',
    y == 28201 ~ 'Materiales de seguridad p\u00fablica',
    y == 28301 ~ 'Prendas de protecci\u00f3n para seguridad p\u00fablica y nacional',
    y == 29101 ~ 'Herramientas menores',
    y == 29199 ~ 'Herramientas, refacciones y accesorios menores',
    y == 29201 ~ 'Refacciones y accesorios menores de edificios',
    y == 29301 ~ 'Refacciones y accesorios menores de mobiliario y equipo de administraci\u00f3n, educacional y recreativo',
    y == 29401 ~ 'Refacciones y accesorios para equipo de c\u00f3mputo',
    y == 29401 ~ 'Refacciones y accesorios para equipo de c\u00f3mputo y telecomunicaciones',
    y == 29501 ~ 'Refacciones y accesorios menores de equipo e instrumental m\u00e9dico y de laboratorio',
    y == 29601 ~ 'Refacciones y accesorios menores de equipo de transporte',
    y == 29701 ~ 'Refacciones y accesorios menores de equipo de defensa y seguridad',
    y == 29801 ~ 'Refacciones y accesorios menores de maquinaria y otros equipos',
    y == 29901 ~ 'Refacciones y accesorios menores otros bienes muebles',
    y == 31101 ~ 'Servicio de energ\u00eda el\u00e9ctrica',
    y == 31199 ~ 'Servicios b\u00e1sicos',
    y == 31201 ~ 'Servicio de gas',
    y == 31301 ~ 'Servicio de agua',
    y == 31401 ~ 'Servicio telef\u00f3nico convencional',
    y == 31501 ~ 'Servicio de telefon\u00eda celular',
    y == 31601 ~ 'Servicio de radiolocalizaci\u00f3n',
    y == 31602 ~ 'Servicios de telecomunicaciones',
    y == 31603 ~ 'Asignaciones destinadas a cubrir el pago de servicios de internet, requeridos en el desempe\u00f1o de funciones oficiales',
    y == 31603 ~ 'Servicios de internet',
    y == 31701 ~ 'Servicios de conducci\u00f3n de se\u00f1ales anal\u00f3gicas y digitales',
    y == 31801 ~ 'Servicio postal',
    y == 31802 ~ 'Servicio telegr\u00e1fico',
    y == 31901 ~ 'Servicios integrales de telecomunicaci\u00f3n',
    y == 31902 ~ 'Contrataci\u00f3n de otros servicios',
    y == 31903 ~ 'Servicios generales para planteles educativos',
    y == 31904 ~ 'Servicios integrales de infraestructura de c\u00f3mputo',
    y == 32101 ~ 'Arrendamiento de terrenos',
    y == 32199 ~ 'Servicios de arrendamiento',
    y == 32201 ~ 'Arrendamiento de edificios y locales',
    y == 32301 ~ 'Arrendamiento de equipo y bienes inform\u00e1ticos',
    y == 32302 ~ 'Arrendamiento de mobiliario',
    y == 32303 ~ 'Arrendamiento de equipo de telecomunicaciones',
    y == 32401 ~ 'Arrendamiento de equipo e instrumental m\u00e9dico y de laboratorio',
    y == 32501 ~ 'Arrendamiento de veh\u00edculos terrestres, a\u00e9reos, mar\u00edtimos, lacustres y fluviales para la ejecuci\u00f3n de programas de seguridad p\u00fablica y nacional',
    y == 32502 ~ 'Arrendamiento de veh\u00edculos terrestres, a\u00e9reos, mar\u00edtimos, lacustres y fluviales para servicios p\u00fablicos y la operaci\u00f3n de programas p\u00fablicos',
    y == 32503 ~ 'Arrendamiento de veh\u00edculos terrestres, a\u00e9reos, mar\u00edtimos, lacustres y fluviales para servicios administrativos',
    y == 32505 ~ 'Arrendamiento de veh\u00edculos terrestres, a\u00e9reos, mar\u00edtimos, lacustres y fluviales para servidores p\u00fablicos',
    y == 32601 ~ 'Arrendamiento de maquinaria y equipo',
    y == 32701 ~ 'Patentes, derechos de autor, regal\u00edas y otros',
    y == 32701 ~ 'Patentes, regal\u00edas y otros',
    y == 32901 ~ 'Arrendamiento de sustancias y productos qu\u00edmicos',
    y == 32902 ~ 'PIDIREGAS cargos fijos',
    y == 32903 ~ 'Otros Arrendamientos',
    y == 33101 ~ 'Asesor\u00edas asociadas a convenios, tratados o acuerdos',
    y == 33102 ~ 'Asesor\u00edas por controversias en el marco de los tratados internacionales',
    y == 33103 ~ 'Consultor\u00edas para programas o proyectos financiados por organismos internacionales',
    y == 33104 ~ 'Otras asesor\u00edas para la operaci\u00f3n de programas',
    y == 33105 ~ 'Servicios relacionados con procedimientos jurisdiccionales',
    y == 33106 ~ 'Servicios legales, de contabilidad, auditor\u00eda y relacionados (Ramos Aut\u00f3nomos)',
    y == 33199 ~ 'Servicios profesionales, cient\u00edficos, t\u00e9cnicos y otros servicios',
    y == 33201 ~ 'Servicios de dise\u00f1o, arquitectura, ingenier\u00eda y actividades relacionadas',
    y == 33201 ~ 'Servicios de dise\u00f1o, arquitectura, ingenier\u00eda y actividades relacionadas (Ramos Aut\u00f3nomos)',
    y == 33301 ~ 'Servicios de desarrollo de aplicaciones inform\u00e1ticas',
    y == 33301 ~ 'Servicios de inform\u00e1tica',
    y == 33302 ~ 'Servicios estad\u00edsticos y geogr\u00e1ficos',
    y == 33303 ~ 'Servicios relacionados con certificaci\u00f3n de procesos',
    y == 33304 ~ 'Servicios de consultor\u00eda administrativa, procesos, t\u00e9cnica y en tecnolog\u00edas de la informaci\u00f3n',
    y == 33304 ~ 'Servicios de mantenimiento de aplicaciones inform\u00e1ticas',
    y == 33401 ~ 'Servicios para capacitaci\u00f3n a servidores p\u00fablicos',
    y == 33501 ~ 'Estudios e investigaciones',
    y == 33601 ~ 'Servicios relacionados con traducciones',
    y == 33602 ~ 'Otros servicios comerciales',
    y == 33603 ~ 'Impresiones de documentos oficiales para la prestaci\u00f3n de servicios p\u00fablicos, identificaci\u00f3n, formatos administrativos y fiscales, formas valoradas, certificados y t\u00edtulos',
    y == 33604 ~ 'Impresi\u00f3n y elaboraci\u00f3n de material informativo derivado de la operaci\u00f3n y administraci\u00f3n de las dependencias y entidades',
    y == 33605 ~ 'Informaci\u00f3n en medios masivos derivada de la operaci\u00f3n y administraci\u00f3n de las dependencias y entidades',
    y == 33605 ~ 'Informaci\u00f3n en medios masivos derivada de la operaci\u00f3n y administraci\u00f3n de las Unidades Responsables',
    y == 33606 ~ 'Servicios de digitalizaci\u00f3n',
    y == 33701 ~ 'Gastos de seguridad p\u00fablica y nacional',
    y == 33801 ~ 'Servicios de vigilancia',
    y == 33901 ~ 'Subcontrataci\u00f3n de servicios con terceros',
    y == 33902 ~ 'Proyectos para prestaci\u00f3n de servicios',
    y == 33903 ~ 'Servicios integrales',
    y == 33904 ~ 'Asignaciones derivadas de proyectos de asociaci\u00f3n p\u00fablico privada',
    y == 33905 ~ 'Servicios integrales en materia de seguridad p\u00fablica y nacional',
    y == 34101 ~ 'Servicios bancarios y financieros',
    y == 34199 ~ 'Servicios financieros, bancarios y comerciales',
    y == 34301 ~ 'Gastos inherentes a la recaudaci\u00f3n',
    y == 34401 ~ 'Seguro de responsabilidad patrimonial del Estado',
    y == 34501 ~ 'Seguros de bienes patrimoniales',
    y == 34601 ~ 'Almacenaje, embalaje y envase',
    y == 34701 ~ 'Fletes y maniobras',
    y == 34801 ~ 'Comisiones por ventas',
    y == 34901 ~ 'Otros servicios financieros, bancarios y comerciales (Ramos Aut\u00f3nomos)',
    y == 35101 ~ 'Mantenimiento y conservaci\u00f3n de inmuebles para la prestaci\u00f3n de servicios administrativos',
    y == 35102 ~ 'Mantenimiento y conservaci\u00f3n de inmuebles para la prestaci\u00f3n de servicios p\u00fablicos',
    y == 35199 ~ 'Servicios de instalaci\u00f3n, reparaci\u00f3n, mantenimiento y conservaci\u00f3n',
    y == 35201 ~ 'Mantenimiento y conservaci\u00f3n de mobiliario y equipo de administraci\u00f3n',
    y == 35301 ~ 'Mantenimiento y conservaci\u00f3n de bienes inform\u00e1ticos',
    y == 35401 ~ 'Instalaci\u00f3n, reparaci\u00f3n y mantenimiento de equipo e instrumental m\u00e9dico y de laboratorio',
    y == 35501 ~ 'Mantenimiento y conservaci\u00f3n de veh\u00edculos terrestres, a\u00e9reos, mar\u00edtimos, lacustres y fluviales',
    y == 35601 ~ 'Reparaci\u00f3n y mantenimiento de equipo de defensa y seguridad',
    y == 35701 ~ 'Mantenimiento y conservaci\u00f3n de maquinaria y equipo',
    y == 35702 ~ 'Mantenimiento y conservaci\u00f3n de plantas e instalaciones productivas',
    y == 35801 ~ 'Servicios de lavander\u00eda, limpieza e higiene',
    y == 35901 ~ 'Servicios de jardiner\u00eda y fumigaci\u00f3n',
    y == 36101 ~ 'Difusi\u00f3n de mensajes sobre programas y actividades gubernamentales',
    y == 36101 ~ 'Difusi\u00f3n de mensajes sobre programas y actividades Institucionales',
    y == 36199 ~ 'Servicios de comunicaci\u00f3n social y publicidad',
    y == 36201 ~ 'Difusi\u00f3n de mensajes comerciales para promover la venta de productos o servicios',
    y == 36301 ~ 'Servicios de creatividad, preproducci\u00f3n y producci\u00f3n de publicidad, excepto internet (Ramos Aut\u00f3nomos)',
    y == 36601 ~ 'Servicio de creaci\u00f3n y difusi\u00f3n de contenido exclusivamente a trav\u00e9s de Internet',
    y == 36601 ~ 'Servicio de creaci\u00f3n y difusi\u00f3n de contenido exclusivamente a trav\u00e9s de Internet (Ramos Aut\u00f3nomos)',
    y == 36901 ~ 'Servicios relacionados con monitoreo de informaci\u00f3n en medios masivos',
    y == 37101 ~ 'Pasajes a\u00e9reos nacionales para labores en campo y de supervisi\u00f3n',
    y == 37102 ~ 'Pasajes a\u00e9reos nacionales asociados a los programas de seguridad p\u00fablica y nacional',
    y == 37103 ~ 'Pasajes a\u00e9reos nacionales asociados a desastres naturales',
    y == 37104 ~ 'Pasajes a\u00e9reos nacionales para servidores p\u00fablicos de mando en el desempe\u00f1o de comisiones y funciones oficiales',
    y == 37105 ~ 'Pasajes a\u00e9reos internacionales asociados a los programas de seguridad p\u00fablica y nacional',
    y == 37106 ~ 'Pasajes a\u00e9reos internacionales para servidores p\u00fablicos en el desempe\u00f1o de comisiones y funciones oficiales',
    y == 37199 ~ 'Servicios de traslado y vi\u00e1ticos',
    y == 37201 ~ 'Pasajes terrestres nacionales para labores en campo y de supervisi\u00f3n',
    y == 37202 ~ 'Pasajes terrestres nacionales asociados a los programas de seguridad p\u00fablica y nacional',
    y == 37203 ~ 'Pasajes terrestres nacionales asociados a desastres naturales',
    y == 37204 ~ 'Pasajes terrestres nacionales para servidores p\u00fablicos de mando en el desempe\u00f1o de comisiones y funciones oficiales',
    y == 37206 ~ 'Pasajes terrestres internacionales para servidores p\u00fablicos en el desempe\u00f1o de comisiones y funciones oficiales',
    y == 37207 ~ 'Pasajes terrestres nacionales por medio electr\u00f3nico',
    y == 37301 ~ 'Pasajes mar\u00edtimos, lacustres y fluviales para labores en campo y de supervisi\u00f3n',
    y == 37302 ~ 'Pasajes mar\u00edtimos, lacustres y fluviales asociados a los programas de seguridad p\u00fablica y nacional',
    y == 37304 ~ 'Pasajes mar\u00edtimos, lacustres y fluviales para servidores p\u00fablicos de mando en el desempe\u00f1o de comisiones y funciones oficiales',
    y == 37501 ~ 'Vi\u00e1ticos nacionales para labores en campo y de supervisi\u00f3n',
    y == 37502 ~ 'Vi\u00e1ticos nacionales asociados a los programas de seguridad p\u00fablica y nacional',
    y == 37503 ~ 'Vi\u00e1ticos nacionales asociados a desastres naturales',
    y == 37504 ~ 'Vi\u00e1ticos nacionales para servidores p\u00fablicos en el desempe\u00f1o de funciones oficiales',
    y == 37601 ~ 'Vi\u00e1ticos en el extranjero asociados a los programas de seguridad p\u00fablica y nacional',
    y == 37602 ~ 'Vi\u00e1ticos en el extranjero para servidores p\u00fablicos en el desempe\u00f1o de comisiones y funciones oficiales',
    y == 37701 ~ 'Instalaci\u00f3n del personal federal',
    y == 37801 ~ 'Servicios integrales nacionales para servidores p\u00fablicos en el desempe\u00f1o de comisiones y funciones oficiales',
    y == 37802 ~ 'Servicios integrales en el extranjero para servidores p\u00fablicos en el desempe\u00f1o de comisiones y funciones oficiales',
    y == 37901 ~ 'Gastos para operativos y trabajos de campo en \u00e1reas rurales',
    y == 38101 ~ 'Gastos de ceremonial del titular del Ejecutivo Federal',
    y == 38102 ~ 'Gastos de ceremonial de los titulares de las dependencias y entidades',
    y == 38199 ~ 'Servicios oficiales',
    y == 38201 ~ 'Gastos de orden social',
    y == 38301 ~ 'Congresos y convenciones',
    y == 38401 ~ 'Exposiciones',
    y == 38501 ~ 'Gastos para alimentaci\u00f3n de servidores p\u00fablicos de mando',
    y == 39101 ~ 'Funerales y pagas de defunci\u00f3n',
    y == 39199 ~ 'Otros servicios generales',
    y == 39201 ~ 'Impuestos y derechos de exportaci\u00f3n',
    y == 39202 ~ 'Enteros de los seguros de cesant\u00eda y vej\u00e9z, invalidez y vida y riesgos de trabajo',
    y == 39202 ~ 'Otros impuestos y derechos',
    y == 39301 ~ 'Impuestos y derechos de importaci\u00f3n',
    y == 39401 ~ 'Erogaciones por resoluciones emitidas por autoridad competente',
    y == 39401 ~ 'Erogaciones por resoluciones por autoridad competente',
    y == 39402 ~ 'Indemnizaciones por expropiaci\u00f3n de predios',
    y == 39501 ~ 'Penas, multas, accesorios y actualizaciones',
    y == 39601 ~ 'P\u00e9rdidas del erario federal',
    y == 39602 ~ 'Otros gastos por responsabilidades',
    y == 39801 ~ 'Impuesto sobre n\u00f3minas',
    y == 39901 ~ 'Gastos de las Comisiones Internacionales de L\u00edmites y Aguas',
    y == 39902 ~ 'Gastos de las oficinas del Servicio Exterior Mexicano',
    y == 39902 ~ 'Otros servicios generales',
    y == 39903 ~ 'Asignaciones a los grupos parlamentarios (Ramos Aut\u00f3nomos)',
    y == 39904 ~ 'Participaciones en \u00f3rganos de gobierno',
    y == 39908 ~ 'Erogaciones por cuenta de terceros',
    y == 39909 ~ 'Erogaciones recuperables',
    y == 39910 ~ 'Apertura de Fondo Rotatorio',
    y == 41501 ~ 'Transferencias para cubrir el d\u00e9ficit de operaci\u00f3n y los gastos de administraci\u00f3n asociados al otorgamiento de subsidios',
    y == 43101 ~ 'Subsidios a la producci\u00f3n',
    y == 43301 ~ 'Subsidios para inversi\u00f3n',
    y == 43401 ~ 'Subsidios a la prestaci\u00f3n de servicios p\u00fablicos',
    y == 43701 ~ 'Subsidios al consumo',
    y == 43801 ~ 'Subsidios a Entidades Federativas y Municipios',
    y == 43901 ~ 'Otros subsidios y subvenciones',
    y == 43901 ~ 'Subsidios para capacitaci\u00f3n y becas',
    y == 43902 ~ 'Subsidios a fideicomisos privados y estatales',
    y == 44101 ~ 'Gastos relacionados con actividades culturales, deportivas y de ayuda extraordinaria',
    y == 44102 ~ 'Gastos por servicios de traslado de personas',
    y == 44103 ~ 'Premios, recompensas, pensiones de gracia y pensi\u00f3n recreativa estudiantil',
    y == 44104 ~ 'Premios, est\u00edmulos, recompensas, becas y seguros a deportistas',
    y == 44105 ~ 'Apoyo a voluntarios que participan en diversos programas federales',
    y == 44106 ~ 'Compensaciones por servicios de car\u00e1cter social',
    y == 44107 ~ 'Apoyo a representantes del Poder Legislativo y partidos pol\u00edticos ante el Consejo General del IFE (Ramos Aut\u00f3nomos)',
    y == 44108 ~ 'Dietas a consejeros electorales locales y distritales en el a\u00f1o electoral federal (Ramos Aut\u00f3nomos)',
    y == 44109 ~ 'Apoyo para alimentos a funcionarios de casilla el d\u00eda de la jornada electoral federal (Ramos Aut\u00f3nomos)',
    y == 44110 ~ 'Apoyo financiero a consejeros electorales locales y distritales en a\u00f1o electoral federal (Ramos Aut\u00f3nomos)',
    y == 44199 ~ 'Ayudas sociales',
    y == 44201 ~ 'Otras ayudas para programas de capacitaci\u00f3n (Solo para el Ramo 03 Poder Judicial) (Ramos Aut\u00f3nomos)',
    y == 44401 ~ 'Apoyos a la investigaci\u00f3n cient\u00edfica y tecnol\u00f3gica de instituciones acad\u00e9micas y sector p\u00fablico',
    y == 44402 ~ 'Apoyos a la investigaci\u00f3n cient\u00edfica y tecnol\u00f3gica en instituciones sin fines de lucro',
    y == 44501 ~ 'Apoyo financiero al Comit\u00e9 Nacional de Supervisi\u00f3n y Evaluaci\u00f3n y a la Comisi\u00f3n Nacional de Vigilancia locales y distritales del Registro Federal de Electores (Ramos Aut\u00f3nomos)',
    y == 44502 ~ 'Financiamiento p\u00fablico a partidos pol\u00edticos y agrupaciones pol\u00edticas con registro autorizado (Ramos Aut\u00f3nomos)',
    y == 44801 ~ 'Mercanc\u00edas para su distribuci\u00f3n a la poblaci\u00f3n',
    y == 45201 ~ 'Pago de pensiones y jubilaciones',
    y == 45202 ~ 'Pago de pensiones y jubilaciones contractuales',
    y == 45203 ~ 'Transferencias para el pago de pensiones y jubilaciones',
    y == 45901 ~ 'Pago de sumas aseguradas',
    y == 45902 ~ 'Prestaciones econ\u00f3micas distintas de pensiones y jubilaciones',
    y == 46101 ~ 'Aportaciones a fideicomisos p\u00fablicos',
    y == 46102 ~ 'Aportaciones a mandatos p\u00fablicos',
    y == 47101 ~ 'Trasferencias para cuotas y aportaciones de seguridad social para el IMSS, ISSSTE e ISSFAM por obligaci\u00f3n del Estado',
    y == 47102 ~ 'Transferencias para cuotas y aportaciones a los seguros de retiro, cesant\u00eda en edad avanzada y vejez',
    y == 48101 ~ 'Donativos a instituciones sin fines de lucro',
    y == 48201 ~ 'Donativos a entidades federativas o municipios',
    y == 48501 ~ 'Donativos internacionales',
    y == 49199 ~ 'Transferencias al exterior',
    y == 49201 ~ 'Cuotas y aportaciones a organismos internacionales',
    y == 49202 ~ 'Otras aportaciones internacionales',
    y == 51101 ~ 'Mobiliario',
    y == 51199 ~ 'Mobiliario y equipo de administraci\u00f3n',
    y == 51201 ~ 'Muebles, excepto de oficina y estanter\u00eda (Ramos Aut\u00f3nomos)',
    y == 51301 ~ 'Bienes art\u00edsticos y culturales',
    y == 51501 ~ 'Bienes inform\u00e1ticos',
    y == 51901 ~ 'Equipo de administraci\u00f3n',
    y == 52101 ~ 'Equipos y aparatos audiovisuales',
    y == 52201 ~ 'Aparatos deportivos',
    y == 52301 ~ 'C\u00e1maras fotogr\u00e1ficas y de video',
    y == 52901 ~ 'Otro mobiliario y equipo educacional y recreativo',
    y == 53101 ~ 'Equipo m\u00e9dico y de laboratorio',
    y == 53201 ~ 'Instrumental m\u00e9dico y de laboratorio',
    y == 54103 ~ 'Veh\u00edculos y equipo terrestres, destinados a servicios p\u00fablicos y la operaci\u00f3n de programas p\u00fablicos',
    y == 54104 ~ 'Veh\u00edculos y equipo terrestres, destinados a servicios administrativos',
    y == 54201 ~ 'Carrocer\u00edas y remolques',
    y == 54401 ~ 'Equipo ferroviario',
    y == 54502 ~ 'Veh\u00edculos y equipo mar\u00edtimo, destinados a servicios p\u00fablicos y la operaci\u00f3n de programas p\u00fablicos',
    y == 56101 ~ 'Maquinaria y equipo agropecuario',
    y == 56201 ~ 'Maquinaria y equipo industrial',
    y == 56301 ~ 'Maquinaria y equipo de construcci\u00f3n',
    y == 56401 ~ 'Sistemas de aire acondicionado, calefacci\u00f3n y de refrigeraci\u00f3n industrial y comercial',
    y == 56401 ~ 'Sistemas de aire acondicionado, calefacci\u00f3n y de refrigeraci\u00f3n industrial y comercial (Ramos Aut\u00f3nomos)',
    y == 56501 ~ 'Equipos y aparatos de comunicaciones y telecomunicaciones',
    y == 56601 ~ 'Maquinaria y equipo el\u00e9ctrico y electr\u00f3nico',
    y == 56701 ~ 'Herramientas y m\u00e1quinas herramienta',
    y == 56901 ~ 'Bienes muebles por arrendamiento financiero',
    y == 56902 ~ 'Otros bienes muebles',
    y == 58101 ~ 'Terrenos',
    y == 58301 ~ 'Edificios y locales',
    y == 58901 ~ 'Adjudicaciones, expropiaciones e indemnizaciones de inmuebles',
    y == 58902 ~ 'Bienes inmuebles en la modalidad de proyectos de infraestructura productiva de largo plazo',
    y == 58903 ~ 'Bienes inmuebles por arrendamiento financiero',
    y == 59101 ~ 'Software',
    y == 59701 ~ 'Licencias inform\u00e1ticas e intelectuales (Ramos Aut\u00f3nomos)',
    y == 62201 ~ 'Obras de construcci\u00f3n para edificios no habitacionales',
    y == 62202 ~ 'Mantenimiento y rehabilitaci\u00f3n de edificaciones no habitacionales',
    y == 62301 ~ 'Construcci\u00f3n de obras para el abastecimiento de agua, petr\u00f3leo, gas, electricidad y telecomunicaciones',
    y == 62302 ~ 'Mantenimiento y rehabilitaci\u00f3n de obras para el abastecimiento de agua, petr\u00f3leo, gas, electricidad y telecomunicaciones',
    y == 62403 ~ 'Mantenimiento y rehabilitaci\u00f3n de obras de urbanizaci\u00f3n',
    y == 62501 ~ 'Construcci\u00f3n de v\u00edas de comunicaci\u00f3n',
    y == 62502 ~ 'Mantenimiento y rehabilitaci\u00f3n de las v\u00edas de comunicaci\u00f3n',
    y == 62601 ~ 'Otras construcciones de ingenier\u00eda civil u obra pesada',
    y == 62602 ~ 'Mantenimiento y rehabilitaci\u00f3n de otras obras de ingenier\u00eda civil u obras pesadas',
    y == 62701 ~ 'Instalaciones y obras de construcci\u00f3n especializada',
    y == 62903 ~ 'Servicios de supervisi\u00f3n de obras',
    y == 62904 ~ 'Servicios para la liberaci\u00f3n de derechos de v\u00eda',
    y == 62905 ~ 'Otros servicios relacionados con obras p\u00fablicas',
    y == 72501 ~ 'Adquisici\u00f3n de acciones de organismos internacionales',
    y == 73902 ~ 'Adquisici\u00f3n de acciones',
    y == 73903 ~ 'Adquisici\u00f3n de otros valores',
    y == 79902 ~ 'Provisiones para erogaciones especiales',
    y == 81101 ~ 'Fondo General de Participaciones',
    y == 81201 ~ 'Fondo de Fomento Municipal',
    y == 81401 ~ 'Otros conceptos participables de la Federaci\u00f3n a entidades federativas',
    y == 83101 ~ 'Aportaciones federales a las entidades federativas y municipios para servicios personales',
    y == 83102 ~ 'Aportaciones federales a las entidades federativas y municipios para aportaciones al ISSSTE',
    y == 83103 ~ 'Aportaciones federales a las entidades federativas y municipios para gastos de operaci\u00f3n',
    y == 83104 ~ 'Aportaciones federales a las entidades federativas y municipios para gastos de inversi\u00f3n',
    y == 83105 ~ 'Aportaciones federales a las entidades federativas y municipios',
    y == 83106 ~ 'Aportaciones federales a las entidades federativas y municipios para incrementos a las percepciones',
    y == 83107 ~ 'Aportaciones federales a las entidades federativas y municipios para creaci\u00f3n de plazas',
    y == 83108 ~ 'Aportaciones federales a las entidades federativas y municipios para otras medidas de car\u00e1cter laboral y econ\u00f3micas',
    y == 83109 ~ 'Aportaciones federales a las entidades federativas y municipios para aportaciones al FOVISSSTE',
    y == 83110 ~ 'Aportaciones federales a las entidades federativas y municipios por previsiones para aportaciones al ISSSTE',
    y == 83111 ~ 'Aportaciones federales a las entidades federativas y municipios por previsiones para aportaciones al FOVISSSTE',
    y == 83112 ~ 'Aportaciones federales a las entidades federativas y municipios para aportaciones al sistema de ahorro para el retiro',
    y == 83113 ~ 'Aportaciones federales a las entidades federativas y municipios para aportaciones al seguro de cesant\u00eda en edad avanzada y vejez',
    y == 83114 ~ 'Aportaciones federales a las entidades federativas y municipios para los dep\u00f3sitos al ahorro solidario',
    y == 83115 ~ 'Aportaciones federales a las entidades federativas y municipios por previsiones para aportaciones al sistema de ahorro para el retiro',
    y == 83116 ~ 'Aportaciones federales a las entidades federativas y municipios por previsiones para aportaciones al seguro de cesant\u00eda en edad avanzada y vejez',
    y == 83117 ~ 'Aportaciones federales a las entidades federativas y municipios por previsiones para los dep\u00f3sitos al ahorro solidario',
    y == 83118 ~ 'Aportaciones de la Federaci\u00f3n a los organismos del Sistema Nacional de Coordinaci\u00f3n Fiscal',
    y == 83501 ~ 'Asignaciones compensatorias a entidades federativas',
    y == 85101 ~ 'Convenios de reasignaci\u00f3n',
    y == 92101 ~ 'Intereses de la deuda interna con instituciones de cr\u00e9dito',
    y == 92102 ~ 'Intereses de la deuda interna derivada de proyectos de infraestructura productiva de largo plazo',
    y == 92201 ~ 'Intereses derivados de la colocaci\u00f3n de valores gubernamentales',
    y == 92401 ~ 'Intereses de la deuda externa con instituciones de cr\u00e9dito',
    y == 92402 ~ 'Intereses de la deuda externa derivada de proyectos de infraestructura productiva de largo plazo',
    y == 92501 ~ 'Intereses de la deuda con organismos financieros internacionales',
    y == 92601 ~ 'Intereses de la deuda bilateral',
    y == 92701 ~ 'Intereses derivados de la colocaci\u00f3n externa de bonos',
    y == 93101 ~ 'Comisiones de la deuda interna',
    y == 93201 ~ 'Comisiones de la deuda externa',
    y == 94101 ~ 'Gastos de la deuda interna',
    y == 94201 ~ 'Gastos de la deuda externa',
    y == 95101 ~ 'Costo por coberturas',
    y == 96201 ~ 'Apoyos a ahorradores y deudores de la banca',
    y == 99101 ~ 'Adeudos de ejercicios fiscales anteriores',
    T ~ NA_character_
  )
}

#' Agrupación del presupuesto
#'
#' Esta función permite agrupar las bases de datos abiertas del presupuesto
#' de Transparencia Presupuestaria con los principales niveles de agregación.
#' De esta manera, podemos generar resumenes presupuestales de acuerdo a su
#' clasificación administrativa, funcional o económica.
#' Por ejemplo, permite generar rápidamente un .xframe con el presupuesto
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
#' @param .x data frame con la estructura de los archivos sobre presupuesto
#' de transparencia Presupuestaria
#' (https://www.transparenciapresupuestaria.gob.mx/es/PTP/Datos_Abiertos).
#' @param ... nombre de las variables por las cuales se quiere colapsar
#' la información. Estos nombre tienen que estar completamente en minúscula
#' sin caracteres especiales y separado por guiones bajos. Por ejemplo,
#' id_ramo, desc_ramo, id_modalidad, desc_modalidad, etc.
#' @param periodo_col agregar columna con el periodo correspondiente
#' @param keep_mensual indicador lógico para determinar si mantener la variables de aprobado
#' y modificado en el periodo (en caso de estar diponible)
#'
#' @importFrom magrittr %>%
#' @return data frame colapsado por las variables indicadas. En caso de no
#' indicarse ninguna variable, sólo se colapsará por año fiscal.
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



#' Agrupar lista de data frames de presupuesto en formato long
#'
#' Esta función retoma la una función sum_pef (por lo cual le aplican las
#' mismas reglas) y la aplica a una lista de presupuesto. Esto ayuda cuando
#' se quiere generar datos históricos del presupuesto en formato long, los
#' cuales normalmente están en diferentes archivos.
#' Lo ideal es cargar todos los .xframes de presupuesto en una lista de
#' .xframes y seleccionar las variables por las que se quiere acumular la
#' información.
#' Por ejemplo. Si colocamos un vector de las cuentas públicas de cada año
#' fiscal sin señalar ninguna variable, esta función regresará el total
#' del presupuesto (sin neteo) de todos los ciclos fiscales incluídos.
#' Nota 1: si se acumulan dos .xframes del mismo año fiscal y no de le
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
#' @param lista_df lista de .xframes con la estructura de los datos abiertos
#' de presupuesto de Transparencia Presupuestaria.
#' @param ... nombre de las variables por las cuales se quiere colapsar
#' la información. Estos nombre tienen que estar completamente en minúscula
#' sin caracteres especiales y separado por guiones bajos. Por ejemplo,
#' id_ramo, desc_ramo, id_modalidad, desc_modalidad, etc.
#'
#' @return .xframe colapsado por las variables indicadas. En caso de no
#' indicarse ninguna variable, sólo se colapsará por año fiscal.
#' @export
bind_pef_tp <- function(lista_df, ...) {

  purrr::map_dfr(lista_df, sum_pef_tp, ...)

}


#' Agrupar lista de .xframes de presupuesto en formato wide
#'
#' Esta función retoma la una función sum_pef_tp (por lo cual le aplican las
#' mismas reglas) y la aplica a una lista de presupuesto. Esto ayuda cuando
#' se quiere generar datos históricos del presupuesto en formato wide, los
#' cuales normalmente están en diferentes archivos.
#' Lo ideal es cargar todos los .xframes de presupuesto en una lista de
#' .xframes y seleccionar las variables por las que se quiere acumular la
#' información.
#' Por ejemplo. Si colocamos un vector de las cuentas públicas de cada año
#' fiscal sin señalar ninguna variable, esta función regresará el total
#' del presupuesto (sin neteo) de todos los ciclos fiscales incluídos.
#' Nota 1: si se acumulan dos .xframes del mismo año fiscal y no de le
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
#' @param lista_df lista de .xframes con la estructura de los datos abiertos
#' de presupuesto de Transparencia Presupuestaria.
#' @param ... nombre de las variables por las cuales se quiere colapsar
#' la información. Estos nombre tienen que estar completamente en minúscula
#' sin caracteres especiales y separado por guiones bajos. Por ejemplo,
#' id_ramo, desc_ramo, id_modalidad, desc_modalidad, etc.
#'
#' @importFrom magrittr %>%
#' @return .xframe colapsado por las variables indicadas. En caso de no
#' indicarse ninguna variable, sólo se colapsará por año fiscal.
#' @export
bind_pef_tp_wide <- function(lista_df, ...) {
  purrr::map_dfr(lista_df, sum_pef_tp, ...) %>%
    tidyr::pivot_wider(
      names_from = periodo,
      values_from = dplyr::contains(
        c("proyecto", 
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
#' @param .x .xframe con la estructura de los datos abiertos de
#' presupuesto de Transparencia Presupuestaria.
#' @param ... modificación y creación de variables equivalente a dplyr::mutate
#' @param keep_mensual indicador lógico para determinar si mantener la variables de aprobado
#' y modificado en el periodo (en caso de estar diponible)
#'
#' @importFrom magrittr %>%
#' @return .xframe cuya descripción de ramo indica los casos en que esa
#' categoría del gasto debe ser neteada.
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
#' Esta función convierte aquellas categorías del presupuesto que tienen
#' "Neteo" como descripción del ramo en valores negativos. Normalmente se
#' debería usar después de la función de netear_tp.
#'
#' @param .x .xframe con la estructura de los datos abiertos de
#' presupuesto de Transparencia Presupuestaria.
#'
#' @importFrom magrittr %>%
#' @return .xframe cuyas categorías del gasto con descripción del ramo
#' como "Neteo" están en valores negativos.
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
                   41, 42, 43, 49, # TODO: cechar los casos en que el año hace variar la calsificacin.
                   44, 40, 32) ~ "A. Ramos aut\u00f3nomos",
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
    y == 2 ~  "Oficina de la Presidencia de la Rep\u00fablica",
    y == 3 ~  "Poder Judicial",
    y == 4 ~  "Gobernaci\u00f3n",
    y == 5 ~  "Relaciones Exteriores",
    y == 6 ~  "Hacienda y Cr\u00e9dito P\u00fablico",
    y == 7 ~  "Defensa Nacional",
    y == 8 ~  "Agricultura y Desarrollo Rural",
    y == 9 ~  "Comunicaciones y Transportes",
    y == 10 ~	"Econom\u00eda",
    y == 11 ~	"Educaci\u00f3n P\u00fablica",
    y == 12 ~	"Salud",
    y == 13 ~	"Marina",
    y == 14 ~	"Trabajo y Previsi\u00f3n Social",
    y == 15 ~	"Desarrollo Agrario, Territorial y Urbano",
    y == 16 ~	"Medio Ambiente y Recursos Naturales",
    y == 18 ~	"Energ\u00eda",
    y == 19 ~	"Aportaciones a Seguridad Social",
    y == 20 ~	"Bienestar",
    y == 21 ~	"Turismo",
    y == 22 ~	"Instituto Nacional Electoral",
    y == 23 ~	"Provisiones Salariales y Econ\u00f3micas",
    y == 24 ~	"Deuda P\u00fablica",
    y == 25 ~	"Previsiones y Aportaciones para los Sistemas de Educaci\u00f3n B\u00e1sica, Normal, Tecnol\u00f3gica y de Adultos",
    y == 27 ~	"Funci\u00f3n P\u00fablica",
    y == 28 ~	"Participaciones a Entidades Federativas y Municipios",
    y == 30 ~	"Adeudos de Ejercicios Fiscales Anteriores",
    y == 31 ~	"Tribunales Agrarios",
    y == 32 ~	"Tribunal Federal de Justicia Administrativa",
    y == 33 ~	"Aportaciones Federales para Entidades Federativas y Municipios",
    y == 34 ~	"Erogaciones para los Programas de Apoyo a Ahorradores y Deudores de la Banca",
    y == 35 ~	"Comisi\u00f3n Nacional de los Derechos Humanos",
    y == 36 ~	"Seguridad y Protecci\u00f3n Ciudadana",
    y == 37 ~	"Consejer\u00eda Jur\u00eddica del Ejecutivo Federal",
    y == 38 ~	"Consejo Nacional de Ciencia y Tecnolog\u00eda",
    y == 40 ~	"Informaci\u00f3n Nacional Estad\u00edstica y Geogr\u00e1fica",
    y == 41 ~	"Comisi\u00f3n Federal de Competencia Econ\u00f3mica",
    y == 43 ~	"Instituto Federal de Telecomunicaciones",
    y == 44 ~	"Instituto Nacional de Transparencia, Acceso a la Informaci\u00f3n y Protecci\u00f3n de Datos Personales",
    y == 45 ~	"Comisi\u00f3n Reguladora de Energ\u00eda",
    y == 46 ~	"Comisi\u00f3n Nacional de Hidrocarburos",
    y == 47 ~	"Entidades no Sectorizadas",
    y == 48 ~	"Cultura",
    y == 49 ~	"Fiscal\u00eda General de la Rep\u00fablica",
    y == 50 ~	"Instituto Mexicano del Seguro Social",
    y == 51 ~	"Instituto de Seguridad y Servicios Sociales de los Trabajadores del Estado",
    y == 52 ~	"Petr\u00f3leos Mexicanos",
    y == 53 ~	"Comisi\u00f3n Federal de Electricidad",
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
    y == 24	~ "Deuda P\u00fablica",
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
    y == "ENERGIA" ~ "Sener", #PONER DESPU\u00e9S DE
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
      stringr::str_detect(y, "PUBLICA") ~ "Deuda P\u00fablica",
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
          # Gastos de operaci\u00f3n
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

          # GASTOS DE INVERSI\u00f3N
          # Inversi\u00f3n f\u00edsica
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "1|2|5|6|3|4|79|85|831|834") & !stringr::str_starts(id_objeto_del_gasto, "39909|43") ~ "Gastos de inversi\u00f3n",
          # Subsidios
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "43") ~ "Gastos de inversi\u00f3n",
          # Otros de inversi\u00f3n
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "7|39909") & !stringr::str_starts(id_objeto_del_gasto, "79") ~ "Gastos de inversi\u00f3n",

          T ~ NA_character_
        )

      )


    }

  }



#' Generar subclasificación económica
#'
#' @param .x base de datos de presupuesto
#' @param name_clas nombre de la columna con la clasificación económica
#' @param name_sub nombre de la columna con la sclasificación económica
#'
#' @importFrom rlang :=
#' @importFrom rlang !!
#' @return una base de datos de presupuesto con subclasificación económica
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
          # Gastos de operaci\u00f3n
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

          # GASTOS DE INVERSI\u00f3N
          # Inversi\u00f3n f\u00edsica
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "1|2|5|6|3|4|79|85|831|834") & !stringr::str_starts(id_objeto_del_gasto, "39909|43") ~ "Gastos de inversi\u00f3n",
          # Subsidios
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "43") ~ "Gastos de inversi\u00f3n",
          # Otros de inversi\u00f3n
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "7|39909") & !stringr::str_starts(id_objeto_del_gasto, "79") ~ "Gastos de inversi\u00f3n",

          T ~ NA_character_
        ),
        !!name_new_col_sub := dplyr::case_when(
          # GASTO CORRIENTE
          # Subsidios
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & id_concepto == "4300" ~ "Subsidios",
          # Servicios personales
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & stringr::str_starts(id_objeto_del_gasto, "1|83101|83102|83106|83107|83108|83109|83110|83111|83112|83113|83114|83115|83116|83117") ~ "Servicios personales",
          # Gastos de operaci\u00f3n
          (id_tipogasto == 0 | id_tipogasto == 1 | id_tipogasto == 7) & stringr::str_starts(id_objeto_del_gasto, "2|3") & !stringr::str_starts(id_objeto_del_gasto, "391|394|395|396|397|26106|32902|39908|39910") ~ "Gastos de operaci\u00f3n",
          clasif_eco == "Gasto corriente" ~ "Gastos de operaci\u00f3n",

          # PENSIONES Y JUBILACIONES
          stringr::str_starts(id_objeto_del_gasto, "45") | (stringr::str_starts(id_objeto_del_gasto, "471") & (id_ramo == 19 & (id_ur != "GYR" & id_ur != "GYN"))) ~ "Pensiones y jubilaciones",

          # GASTOS DE INVERSI\u00f3N
          # Subsidios
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "43") ~ "Subsidios",
          # Inversi\u00f3n financiera
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & (id_capitulo == "7000" & id_concepto != "7900") ~ "Inversi\u00f3n financiera",
          # Otros de inversi\u00f3n
          (id_tipogasto == 2| id_tipogasto == 3 | id_tipogasto == 9) & stringr::str_starts(id_objeto_del_gasto, "7|39909") & !stringr::str_starts(id_objeto_del_gasto, "79") ~ "Otros de inversi\u00f3n",
          # Inversi\u00f3n f\u00edsica
          clasif_eco == "Gastos de inversi\u00f3n" ~ "Inversi\u00f3n f\u00edsica",

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

#' Datos del deflactor para la CP de 2021
#'
#' Son los datos del deflactor implícito del PIB, observados y estimados
#' de la SHCP.
#'
#' @format Data Frame
#' @source Datos del deflactor del PIB de la SHCP disponibles en los Lineamientos para la Integración de la Cuenta Pública 2021 (https://www.cuentapublica.hacienda.gob.mx/es/CP/cuenta)
"CP2021"

globalVariables(c(".", "deflactor_local", "periodo",
                  "n", "id", "inpc_bd",
                  "year", "sp1", "deflactor_year",
                  "CP2021",
                  "deflactor_desc", "clasif_eco", "id_ramo",
                  "ciclo", "id_capitulo", "id_objeto_del_gasto",
                  "id_concepto", "id_partida_especifica", "id_partida_generica",
                  "desc_partida_especifica", "id_ur", "clasif_eco",
                  "sub_clasif_eco", "periodo_col", "index",
                  "num", "desc_ramo", "aprobado",
                  "modificado", "pagado", "ejercido",
                  "proyecto", "monto_aprob_mes", "monto_aprobado_mensual",
                  "monto_modif_mes", "monto_modificado_mensual", "desc_objeto_del_gasto",
                  "monto_aprobado_mensual", "desc_ramo",
                  "inpc_bd"))
