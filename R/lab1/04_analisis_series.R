series_names <- unique(series_master$nombre_serie)
stats_list <- list()
station_list <- list()
adf_list <- list()
kpss_list <- list()
series_objects <- list()

for (nm in series_names) {
  dat <- dplyr::filter(series_master, nombre_serie == nm) |> dplyr::arrange(fecha)
  train <- dplyr::filter(dat, conjunto == "Entrenamiento")
  test <- dplyr::filter(dat, conjunto == "Prueba")
  y_train <- stats::ts(train$viajeros, frequency = 12,
                       start = c(lubridate::year(min(train$fecha)), lubridate::month(min(train$fecha))))
  y_all <- stats::ts(dat$viajeros, frequency = 12,
                     start = c(lubridate::year(min(dat$fecha)), lubridate::month(min(dat$fecha))))

  decomp <- stats::stl(y_train, s.window = "periodic", robust = TRUE)
  tr <- as.numeric(decomp$time.series[, "trend"])
  se <- as.numeric(decomp$time.series[, "seasonal"])
  re <- as.numeric(decomp$time.series[, "remainder"])
  strength_s <- clip01(1 - var(re, na.rm = TRUE) / var(se + re, na.rm = TRUE))
  strength_t <- clip01(1 - var(re, na.rm = TRUE) / var(tr + re, na.rm = TRUE))
  lambda <- if (all(train$viajeros > 0)) {
    tryCatch(forecast::BoxCox.lambda(y_train, method = "guerrero"), error = function(e) NA_real_)
  } else NA_real_
  level_var_cor <- suppressWarnings(cor(
    zoo::rollmean(train$viajeros, 12, fill = NA),
    zoo::rollapply(train$viajeros, 12, sd, fill = NA),
    use = "complete.obs"
  ))
  transform <- if (all(train$viajeros > 0) && is.finite(lambda) && abs(lambda - 1) > .15) {
    "Box-Cox"
  } else if (any(train$viajeros == 0) && is.finite(level_var_cor) && level_var_cor > .5) {
    "log1p"
  } else "Original"

  d <- forecast::ndiffs(y_train, alpha = .05, test = "kpss")
  D <- forecast::nsdiffs(y_train, m = 12, test = "ocsb")
  y_diff <- y_train
  if (D > 0) y_diff <- diff(y_diff, lag = 12, differences = D)
  if (d > 0) y_diff <- diff(y_diff, differences = d)

  adf_before <- tryCatch(tseries::adf.test(y_train), error = function(e) NULL)
  adf_after <- tryCatch(tseries::adf.test(y_diff), error = function(e) NULL)
  kpss_before <- tryCatch(tseries::kpss.test(y_train, null = "Trend"), error = function(e) NULL)
  kpss_after <- tryCatch(tseries::kpss.test(y_diff, null = "Level"), error = function(e) NULL)

  stats_list[[nm]] <- data.frame(
    categoria = unique(dat$categoria_analisis), nombre_serie = nm,
    inicio = min(dat$fecha), fin = max(dat$fecha), observaciones = nrow(dat), frecuencia = 12,
    inicio_entrenamiento = min(train$fecha), fin_entrenamiento = max(train$fecha),
    inicio_prueba = min(test$fecha), fin_prueba = max(test$fecha),
    minimo = min(dat$viajeros), maximo = max(dat$viajeros),
    promedio = mean(dat$viajeros), mediana = median(dat$viajeros),
    desviacion = sd(dat$viajeros), coeficiente_variacion = sd(dat$viajeros) / mean(dat$viajeros),
    fuerza_estacionalidad = strength_s, fuerza_tendencia = strength_t,
    lambda_guerrero = lambda, correlacion_nivel_variabilidad = level_var_cor,
    transformacion = transform, d = d, D = D
  )
  station_list[[nm]] <- data.frame(
    nombre_serie = nm, d = d, D = D,
    estacionaria_adf_antes = !is.null(adf_before) && adf_before$p.value < .05,
    estacionaria_adf_despues = !is.null(adf_after) && adf_after$p.value < .05,
    estacionaria_kpss_despues = !is.null(kpss_after) && kpss_after$p.value > .05
  )
  test_row <- function(obj, etapa) data.frame(
    nombre_serie = nm, etapa = etapa,
    estadistico = if (is.null(obj)) NA else unname(obj$statistic),
    rezagos = if (is.null(obj)) NA else unname(obj$parameter),
    p_valor = if (is.null(obj)) NA else obj$p.value
  )
  adf_list[[paste(nm, "a")]] <- test_row(adf_before, "Antes de diferenciar")
  adf_list[[paste(nm, "d")]] <- test_row(adf_after, "Después de diferenciar")
  kpss_list[[paste(nm, "a")]] <- test_row(kpss_before, "Antes de diferenciar")
  kpss_list[[paste(nm, "d")]] <- test_row(kpss_after, "Después de diferenciar")

  series_objects[[nm]] <- list(data = dat, train = train, test = test, y_train = y_train,
                               y_all = y_all, decomp = decomp, y_diff = y_diff,
                               lambda = lambda, transform = transform, d = d, D = D)

  p <- ggplot2::ggplot(dat, ggplot2::aes(fecha, viajeros, color = conjunto)) +
    ggplot2::annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-01"),
                      ymin = -Inf, ymax = Inf, alpha = .08, fill = "#B2182B") +
    ggplot2::geom_line(linewidth = .55) +
    ggplot2::geom_vline(xintercept = min(test$fecha), linetype = 2) +
    ggplot2::geom_vline(xintercept = as.Date("2023-01-01"), linetype = 3, color = "#2166AC") +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(title = nm, subtitle = "Serie mensual y división cronológica 70/30",
                  x = NULL, y = "Visitantes", color = "Conjunto") + theme_lab()
  save_plot(paste0("serie_", slug(nm), ".png"), p)

  grDevices::png(file.path("outputs/figures", paste0("stl_", slug(nm), ".png")),
                 width = 1800, height = 1300, res = 180)
  plot(decomp, main = paste("STL robusta:", nm))
  grDevices::dev.off()
  grDevices::png(file.path("outputs/diagnostics", paste0("acf_pacf_", slug(nm), ".png")),
                 width = 1800, height = 800, res = 180)
  graphics::par(mfrow = c(1, 2))
  forecast::Acf(y_diff, main = paste("ACF estacionaria:", nm))
  forecast::Pacf(y_diff, main = paste("PACF estacionaria:", nm))
  grDevices::dev.off()
}

stats_series <- dplyr::bind_rows(stats_list)
write_csv_utf8(stats_series, "outputs/tables/estadisticas_series.csv")
write_csv_utf8(dplyr::bind_rows(station_list), "outputs/tables/estacionariedad_series.csv")
write_csv_utf8(dplyr::bind_rows(adf_list), "outputs/tables/pruebas_adf.csv")
write_csv_utf8(dplyr::bind_rows(kpss_list), "outputs/tables/pruebas_kpss.csv")
saveRDS(series_objects, "data/processed/objetos_series.rds")
