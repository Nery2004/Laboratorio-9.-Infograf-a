all_months <- seq(min(monthly$fecha), max(monthly$fecha), by = "month")
top3_regions <- top_region_dos |> dplyr::slice_head(n = 3) |> dplyr::pull(valor)

complete_series <- function(data, category, name_col, names_keep = NULL) {
  if (!is.null(names_keep)) data <- dplyr::filter(data, .data[[name_col]] %in% names_keep)
  data |>
    dplyr::group_by(fecha, nombre_serie = .data[[name_col]]) |>
    dplyr::summarise(viajeros = sum(viajero, na.rm = TRUE), .groups = "drop") |>
    tidyr::complete(fecha = all_months, nombre_serie = unique(nombre_serie),
                    fill = list(viajeros = 0)) |>
    dplyr::mutate(categoria_analisis = category)
}

total_series <- monthly |>
  dplyr::transmute(fecha, categoria_analisis = "Total", nombre_serie = "Total comparable", viajeros)
via_names <- top_vias |>
  dplyr::filter(tolower(iconv(valor, to = "ASCII//TRANSLIT")) %in%
                  c("aerea", "terrestre", "maritima")) |>
  dplyr::pull(valor)
if (length(via_names) != 3) {
  stop("No se identificaron exactamente las vías Aérea, Terrestre y Marítima.", call. = FALSE)
}
via_series <- complete_series(comp, "Vía de ingreso", "via", via_names)
region_series <- complete_series(comp, "Región dos", "region_dos", top3_regions)

series_master <- dplyr::bind_rows(total_series, via_series, region_series) |>
  dplyr::arrange(nombre_serie, fecha)
if (dplyr::n_distinct(series_master$nombre_serie) != 7) {
  stop("La construcción no produjo las siete series requeridas.", call. = FALSE)
}

n_months <- length(all_months)
n_train <- floor(.70 * n_months)
cutoff <- all_months[n_train]
split_tbl <- data.frame(
  fecha = all_months,
  conjunto = ifelse(seq_along(all_months) <= n_train, "Entrenamiento", "Prueba")
)
series_master <- series_master |>
  dplyr::left_join(split_tbl, by = "fecha") |>
  dplyr::group_by(nombre_serie) |>
  dplyr::mutate(porcentaje_periodo = 100 * viajeros / sum(viajeros)) |>
  dplyr::ungroup()

write_csv_utf8(series_master, "data/processed/series_mensuales.csv")
write_csv_utf8(split_tbl, "data/processed/division_entrenamiento_prueba.csv")
write_csv_utf8(data.frame(region = top3_regions), "outputs/tables/top3_regiones_seleccionadas.csv")
saveRDS(series_master, "data/processed/series_mensuales.rds")
