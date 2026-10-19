inputs <- find_inputs()
if (!length(inputs$excel)) {
  stop(
    paste(
      "No se encontró el Excel Base_Migracion dentro del proyecto.",
      "Colóquelo en data/raw/ sin cambiar su contenido y vuelva a ejecutar."
    ),
    call. = FALSE
  )
}
excel_path <- inputs$excel[[1]]
sheets <- readxl::excel_sheets(excel_path)
if (!all(c("Datos", "Notas") %in% sheets)) {
  stop("El Excel debe contener las hojas 'Datos' y 'Notas'. Hojas encontradas: ",
       paste(sheets, collapse = ", "), call. = FALSE)
}

raw <- readxl::read_excel(excel_path, sheet = "Datos", guess_max = 20000)
notas <- readxl::read_excel(excel_path, sheet = "Notas", col_names = FALSE)
raw_names <- names(raw)
datos <- janitor::clean_names(raw)

expected <- c(
  "ano", "mes_cod", "mes", "via", "frontera", "pais", "region",
  "region_dos", "regiones_omt", "mceo", "agrupacion_residencia",
  "tipo_de_viajero", "viajero"
)
missing_cols <- setdiff(expected, names(datos))
if (length(missing_cols)) {
  stop("Faltan columnas esenciales: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

text_cols <- names(datos)[vapply(datos, is.character, logical(1))]
datos[text_cols] <- lapply(datos[text_cols], stringr::str_squish)
datos <- datos |>
  dplyr::mutate(
    ano = suppressWarnings(as.integer(ano)),
    mes_cod = suppressWarnings(as.integer(mes_cod)),
    viajero = safe_num(viajero),
    fecha = as.Date(sprintf("%04d-%02d-01", ano, mes_cod))
  ) |>
  dplyr::arrange(fecha)

invalid_month <- is.na(datos$mes_cod) | datos$mes_cod < 1 | datos$mes_cod > 12
invalid_year <- is.na(datos$ano) | datos$ano < 1900 | datos$ano > as.integer(format(Sys.Date(), "%Y")) + 1
if (any(invalid_month | invalid_year)) {
  stop("Hay años o meses imposibles. Revise outputs/tables/calidad_fechas.csv.", call. = FALSE)
}

natural_dims <- setdiff(expected, "viajero")
exact_dup_n <- sum(duplicated(datos))
natural_dup <- datos |>
  dplyr::count(dplyr::across(dplyr::all_of(natural_dims)), name = "n") |>
  dplyr::filter(n > 1)

faltantes <- data.frame(
  columna = names(datos),
  faltantes = vapply(datos, function(x) sum(is.na(x)), numeric(1)),
  porcentaje = vapply(datos, function(x) mean(is.na(x)) * 100, numeric(1))
)
calidad <- data.frame(
  indicador = c("filas", "columnas", "duplicados_exactos", "combinaciones_duplicadas",
                "viajeros_negativos", "viajeros_cero", "viajeros_decimales"),
  valor = c(nrow(datos), ncol(datos), exact_dup_n, nrow(natural_dup),
            sum(datos$viajero < 0, na.rm = TRUE), sum(datos$viajero == 0, na.rm = TRUE),
            sum(abs(datos$viajero - round(datos$viajero)) > 1e-8, na.rm = TRUE))
)
category_quality <- dplyr::bind_rows(lapply(text_cols, function(col) {
  original <- datos[[col]]
  normalized <- tolower(stringr::str_squish(iconv(original, to = "ASCII//TRANSLIT")))
  data.frame(
    columna = col,
    categorias_distintas = dplyr::n_distinct(original, na.rm = TRUE),
    categorias_normalizadas = dplyr::n_distinct(normalized, na.rm = TRUE),
    posibles_variantes_espacio_mayuscula =
      dplyr::n_distinct(original, na.rm = TRUE) - dplyr::n_distinct(normalized, na.rm = TRUE),
    caracteres_no_ascii = sum(grepl("[^ -~]", original), na.rm = TRUE)
  )
}))

# Las combinaciones repetidas válidas se agregan por suma; así se conserva el flujo total.
datos_limpios <- datos |>
  dplyr::group_by(dplyr::across(dplyr::all_of(c(natural_dims, "fecha")))) |>
  dplyr::summarise(viajero = sum(viajero, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(fecha)

tipos_comparables <- c("turista", "excursionista")
datos_comparables <- datos_limpios |>
  dplyr::filter(tolower(stringr::str_squish(tipo_de_viajero)) %in% tipos_comparables)

saveRDS(raw, "data/processed/datos_originales_importados.rds", compress = "xz")
saveRDS(datos_limpios, "data/processed/datos_limpios.rds", compress = "xz")
saveRDS(datos_comparables, "data/processed/datos_comparables.rds", compress = "xz")
saveRDS(notas, "data/processed/notas.rds")
write_csv_utf8(faltantes, "outputs/tables/faltantes.csv")
write_csv_utf8(calidad, "outputs/tables/duplicados.csv")
write_csv_utf8(natural_dup, "outputs/tables/duplicados_dimensiones.csv")
write_csv_utf8(category_quality, "outputs/tables/calidad_categorias.csv")
write_csv_utf8(as.data.frame(notas), "outputs/tables/notas_metodologicas.csv")
write_csv_utf8(
  data.frame(fila = nrow(raw), columna = length(raw_names), primer_mes = min(datos$fecha),
             ultimo_mes = max(datos$fecha), meses = length(unique(datos$fecha))),
  "outputs/tables/resumen_dataset.csv"
)
