# Adaptación del commit 5a7d8af1c9cbcaafc6cf1ade07baa8369683cafc de Lab 1.
for (f in sort(list.files("R/lab1", full.names = TRUE))) {
  message("Ejecutando ", f)
  source(f, encoding = "UTF-8")
  if (grepl("01_carga", f)) {
    stopifnot(nrow(raw) == 161036, !anyNA(datos$viajero), all(datos$viajero >= 0))
    writeLines(digest::digest(file = excel_path, algo = "sha256"), "outputs/tables/insumo_sha256.txt")
  }
}
stopifnot(nrow(monthly) == 210, min(monthly$fecha) == as.Date("2009-01-01"),
          max(monthly$fecha) == as.Date("2026-06-01"))
norm <- function(x) tolower(iconv(x, to = "ASCII//TRANSLIT"))
kpi <- c(total = sum(monthly$viajeros),
         caida = 100 * (1 - annual$total[annual$ano == 2020] / annual$total[annual$ano == 2019]),
         terrestre = top_vias$porcentaje[norm(top_vias$valor) == "terrestre"],
         mes = seasonality$mes_cod[which.max(seasonality$promedio)])
write_csv_utf8(data.frame(indicador = names(kpi), valor = unname(kpi)), "outputs/tables/kpis.csv")
comparativo <- dplyr::bind_rows(lapply(series_objects, function(obj) {
  z <- obj$data
  year <- as.integer(format(z$fecha, "%Y"))
  totals <- tapply(z$viajeros, year, sum)
  data.frame(nombre_serie = z$nombre_serie[1],
    crecimiento = 100 * ((totals['2025'] / totals['2009'])^(1/16) - 1),
    caida = 100 * (1 - totals['2020'] / totals['2019']),
    volatilidad = sd(z$viajeros) / mean(z$viajeros))
})) |>
  dplyr::left_join(stats_series |> dplyr::select(nombre_serie, fuerza_estacionalidad), by = "nombre_serie")
write_csv_utf8(comparativo, "outputs/tables/cuadrante.csv")
best_total <- mejores |> dplyr::filter(nombre_serie == "Total comparable")
stopifnot(nrow(best_total) == 1)
winner <- pronosticos |> dplyr::filter(nombre_serie == "Total comparable", modelo == best_total$modelo)
stopifnot(all(is.finite(winner$inferior_80)), all(is.finite(winner$superior_80)))
saveRDS(list(kpi = kpi, ganador = best_total, comparativo = comparativo), "data/processed/resumen_lab9.rds")
save(list = c("monthly", "seasonality", "top_vias", "top_region_dos", "kpi", "comparativo",
              "best_total", "winner", "metricas", "mejores", "series_names", "series_objects",
              "datos", "datos_limpios", "datos_comparables", "raw", "all_months", "pronosticos"),
     file = "data/processed/contexto_presentacion.RData")
