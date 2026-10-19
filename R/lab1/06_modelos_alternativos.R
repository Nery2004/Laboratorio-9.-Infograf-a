alternative_models <- list()
alternative_metric_rows <- list()
forecast_rows_alt <- list()
model_errors <- list()
hw_validation <- list()

record_model <- function(nm, label, fit, pred, obj, aic = NA, aicc = NA, bic = NA,
                         lb_p = NA, lower80 = NA, upper80 = NA, lower95 = NA, upper95 = NA) {
  metrics <- metric_values(obj$test$viajeros, pred, obj$train$viajeros)
  alternative_metric_rows[[paste(nm, label)]] <<- data.frame(
    nombre_serie = nm, modelo = label, AIC = aic, AICc = aicc, BIC = bic,
    p_Ljung_Box = lb_p, metrics, error = NA_character_
  )
  n <- length(pred)
  expand <- function(x) if (length(x) == n) as.numeric(x) else rep(NA_real_, n)
  forecast_rows_alt[[paste(nm, label)]] <<- data.frame(
    nombre_serie = nm, modelo = label, fecha = obj$test$fecha,
    real = obj$test$viajeros, pronostico_original = as.numeric(pred),
    pronostico = pmax(0, as.numeric(pred)),
    inferior_80 = expand(lower80), superior_80 = expand(upper80),
    inferior_95 = expand(lower95), superior_95 = expand(upper95)
  )
  alternative_models[[paste(nm, label, sep = "::")]] <<- fit
}

record_failure <- function(nm, label, msg) {
  alternative_metric_rows[[paste(nm, label)]] <<- data.frame(
    nombre_serie = nm, modelo = label, AIC = NA, AICc = NA, BIC = NA,
    p_Ljung_Box = NA, MAE = NA, RMSE = NA, MAPE = NA, sMAPE = NA, MASE = NA,
    error = msg
  )
  model_errors[[paste(nm, label)]] <<- data.frame(
    nombre_serie = nm, modelo = label, error = msg
  )
}

for (nm in series_names) {
  obj <- series_objects[[nm]]
  y <- obj$y_train
  h <- nrow(obj$test)

  # ETS
  tryCatch({
    fit <- forecast::ets(y)
    fc <- forecast::forecast(fit, h = h)
    lb <- tryCatch(stats::Box.test(residuals(fit), lag = 24, type = "Ljung-Box",
                                   fitdf = length(fit$par)), error = function(e) NULL)
    record_model(nm, paste0("ETS_", fit$method), fit, fc$mean, obj,
                 fit$aic, fit$aicc, fit$bic, if (is.null(lb)) NA else lb$p.value,
                 fc$lower[, 1], fc$upper[, 1], fc$lower[, 2], fc$upper[, 2])
    saveRDS(fit, file.path("outputs/models", paste0("ets_", slug(nm), ".rds")))
  }, error = function(e) record_failure(nm, "ETS", conditionMessage(e)))

  # Elegir configuración con los últimos 24 meses DEL ENTRENAMIENTO.
  # La prueba externa queda reservada para comparación final de familias.
  inner_h <- 24L
  inner_y <- head(y, length(y) - inner_h)
  inner_actual <- tail(y, inner_h)
  hw_candidates <- list()
  for (spec in list(
    list(label = "Holt_Winters_aditivo", seasonal = "additive", damped = FALSE),
    list(label = "Holt_Winters_aditivo_amortiguado", seasonal = "additive", damped = TRUE),
    list(label = "Holt_Winters_multiplicativo", seasonal = "multiplicative", damped = FALSE)
  )) {
    if (spec$seasonal == "multiplicative" && any(y <= 0)) next
    ans <- tryCatch({
      inner_fit <- forecast::hw(inner_y, h = inner_h, seasonal = spec$seasonal, damped = spec$damped)
      list(label = spec$label, spec = spec,
           rmse = sqrt(mean((inner_actual - pmax(0, as.numeric(inner_fit$mean)))^2)))
    }, error = function(e) NULL)
    if (!is.null(ans)) hw_candidates[[spec$label]] <- ans
  }
  if (length(hw_candidates)) {
    chosen <- hw_candidates[[which.min(vapply(hw_candidates, `[[`, numeric(1), "rmse"))]]
    hw_validation[[nm]] <- dplyr::bind_rows(lapply(hw_candidates, function(candidate) data.frame(
      nombre_serie = nm, modelo = candidate$label, RMSE_validacion_interna = candidate$rmse,
      entrenamiento_interno_hasta = obj$train$fecha[length(inner_y)],
      validacion_desde = obj$train$fecha[length(inner_y) + 1],
      validacion_hasta = max(obj$train$fecha), elegido = candidate$label == chosen$label)))
    fc <- forecast::hw(y, h = h, seasonal = chosen$spec$seasonal, damped = chosen$spec$damped)
    fit <- fc$model
    record_model(nm, chosen$label, fit, fc$mean, obj,
                 fit$aic, fit$aicc, fit$bic, NA,
                 fc$lower[, 1], fc$upper[, 1], fc$lower[, 2], fc$upper[, 2])
    saveRDS(fit, file.path("outputs/models", paste0("holt_winters_", slug(nm), ".rds")))
  } else record_failure(nm, "Holt_Winters", "No se ajustó ninguna configuración válida.")

  # Seasonal Naive
  tryCatch({
    fc <- forecast::snaive(y, h = h)
    record_model(nm, "Seasonal_Naive", fc$model, fc$mean, obj,
                 lower80 = fc$lower[, 1], upper80 = fc$upper[, 1],
                 lower95 = fc$lower[, 2], upper95 = fc$upper[, 2])
  }, error = function(e) record_failure(nm, "Seasonal_Naive", conditionMessage(e)))

  # Prophet se ajusta solo con entrenamiento y exactamente al horizonte de prueba.
  if (requireNamespace("prophet", quietly = TRUE)) {
    tryCatch({
      train_p <- data.frame(ds = obj$train$fecha, y = obj$train$viajeros)
      fit <- prophet::prophet(
        train_p, yearly.seasonality = TRUE, weekly.seasonality = FALSE,
        daily.seasonality = FALSE, uncertainty.samples = 1000
      )
      future <- data.frame(ds = obj$test$fecha)
      fc <- predict(fit, future)
      record_model(nm, "Prophet", fit, fc$yhat, obj,
                   lower80 = fc$yhat_lower, upper80 = fc$yhat_upper)
      saveRDS(fit, file.path("outputs/models", paste0("prophet_", slug(nm), ".rds")))
      comp <- prophet::prophet_plot_components(fit, predict(fit, data.frame(ds = obj$train$fecha)))
      grDevices::png(file.path("outputs/figures", paste0("prophet_componentes_", slug(nm), ".png")),
                     width = 1800, height = 1200, res = 180)
      print(comp)
      grDevices::dev.off()
    }, error = function(e) record_failure(nm, "Prophet", conditionMessage(e)))
  } else {
    record_failure(nm, "Prophet", "El paquete prophet no está disponible tras el intento de instalación.")
  }
}

alternative_table <- dplyr::bind_rows(alternative_metric_rows)
forecast_table_alt <- dplyr::bind_rows(forecast_rows_alt)
write_csv_utf8(dplyr::bind_rows(model_errors), "outputs/diagnostics/errores_modelos.csv")
saveRDS(alternative_models, "outputs/models/modelos_alternativos.rds")
write_csv_utf8(dplyr::bind_rows(hw_validation), "outputs/tables/seleccion_hw_entrenamiento.csv")
