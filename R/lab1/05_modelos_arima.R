arima_models <- list()
arima_rows <- list()
forecast_rows_arima <- list()
forecast_adjustments <- list()

metric_values <- function(actual, pred, train) {
  pred_eval <- pmax(0, as.numeric(pred))
  stopifnot(length(actual) == length(pred_eval), all(is.finite(actual)), all(is.finite(pred_eval)))
  denom <- abs(actual) + abs(pred_eval)
  scale <- mean(abs(diff(train, lag = 12)), na.rm = TRUE)
  data.frame(
    MAE = mean(abs(actual - pred_eval), na.rm = TRUE),
    RMSE = sqrt(mean((actual - pred_eval)^2, na.rm = TRUE)),
    MAPE = if (any(actual == 0)) NA_real_ else mean(abs((actual - pred_eval) / actual)) * 100,
    sMAPE = mean(ifelse(denom == 0, 0, 200 * abs(actual - pred_eval) / denom), na.rm = TRUE),
    MASE = if (is.finite(scale) && scale > 0) mean(abs(actual - pred_eval)) / scale else NA_real_
  )
}

fit_arima_safe <- function(y, order, seasonal, lambda = NULL, biasadj = FALSE) {
  tryCatch(
    forecast::Arima(y, order = order, seasonal = list(order = seasonal, period = 12),
                    lambda = lambda, biasadj = biasadj, method = "ML"),
    error = function(e) structure(list(error = conditionMessage(e)), class = "failed_model")
  )
}

for (nm in series_names) {
  obj <- series_objects[[nm]]
  y <- obj$y_train
  y_model <- if (obj$transform == "log1p") {
    stats::ts(log1p(as.numeric(y)), start = start(y), frequency = frequency(y))
  } else y
  h <- nrow(obj$test)
  lambda_use <- if (obj$transform == "Box-Cox") obj$lambda else NULL
  auto <- tryCatch(
    forecast::auto.arima(y_model, seasonal = TRUE, stepwise = FALSE, approximation = FALSE,
                         lambda = lambda_use, biasadj = !is.null(lambda_use),
                         max.p = 4, max.q = 4, max.P = 2, max.Q = 2),
    error = function(e) structure(list(error = conditionMessage(e)), class = "failed_model")
  )
  candidates <- list(ARIMA_auto = auto)
  if (!inherits(auto, "failed_model")) {
    ord <- forecast::arimaorder(auto)
    base_p <- unname(ord["p"]); base_q <- unname(ord["q"])
    base_P <- unname(ord["P"]); base_Q <- unname(ord["Q"])
  } else {
    base_p <- 1; base_q <- 1; base_P <- 1; base_Q <- 0
  }
  specs <- list(
    ARIMA_manual = list(c(min(2, base_p), obj$d, min(2, base_q)), c(base_P, obj$D, base_Q)),
    ARIMA_cercano_1 = list(c(min(4, base_p + 1), obj$d, base_q), c(base_P, obj$D, base_Q)),
    ARIMA_cercano_2 = list(c(base_p, obj$d, min(4, base_q + 1)), c(base_P, obj$D, base_Q))
  )
  for (lab in names(specs)) {
    candidates[[lab]] <- fit_arima_safe(y_model, specs[[lab]][[1]], specs[[lab]][[2]],
                                        lambda_use, !is.null(lambda_use))
  }

  for (lab in names(candidates)) {
    fit <- candidates[[lab]]
    if (inherits(fit, "failed_model")) {
      arima_rows[[paste(nm, lab)]] <- data.frame(
        nombre_serie = nm, modelo = lab, orden = NA, orden_estacional = NA,
        AIC = NA, AICc = NA, BIC = NA, logLik = NA, sigma2 = NA,
        Ljung_Box = NA, p_Ljung_Box = NA, MAE = NA, RMSE = NA, error = fit$error
      )
      next
    }
    fc <- forecast::forecast(fit, h = h, biasadj = !is.null(lambda_use))
    invert <- function(x) {
      z <- as.numeric(x)
      if (obj$transform == "log1p") expm1(z) else z
    }
    pred_raw <- invert(fc$mean)
    if (any(!is.finite(pred_raw))) {
      # La corrección de sesgo Box-Cox puede ser indefinida cuando la media
      # transformada cruza el dominio. Usar la inversa sin esa corrección,
      # sin consultar los valores de prueba y sin eliminar horizontes.
      fc <- forecast::forecast(fit, h = h, biasadj = FALSE)
      pred_raw <- invert(fc$mean)
      forecast_adjustments[[paste(nm, lab)]] <- data.frame(
        nombre_serie = nm, modelo = lab,
        ajuste = "Inversa Box-Cox sin corrección de sesgo por media no finita; posterior truncamiento a cero")
    }
    metrics <- metric_values(obj$test$viajeros, pred_raw, obj$train$viajeros)
    ord <- forecast::arimaorder(fit)
    lb <- tryCatch(stats::Box.test(residuals(fit), lag = min(24, floor(length(y) / 5)),
                                   type = "Ljung-Box", fitdf = length(fit$coef)),
                   error = function(e) NULL)
    arima_rows[[paste(nm, lab)]] <- data.frame(
      nombre_serie = nm, modelo = lab,
      orden = sprintf("(%d,%d,%d)", ord["p"], ord["d"], ord["q"]),
      orden_estacional = sprintf("(%d,%d,%d)[12]", ord["P"], ord["D"], ord["Q"]),
      AIC = fit$aic, AICc = fit$aicc, BIC = fit$bic, logLik = as.numeric(logLik(fit)),
      sigma2 = fit$sigma2, Ljung_Box = if (is.null(lb)) NA else unname(lb$statistic),
      p_Ljung_Box = if (is.null(lb)) NA else lb$p.value,
      MAE = metrics$MAE, RMSE = metrics$RMSE, error = NA_character_
    )
    forecast_rows_arima[[paste(nm, lab)]] <- data.frame(
      nombre_serie = nm, modelo = lab, fecha = obj$test$fecha,
      real = obj$test$viajeros, pronostico_original = pred_raw,
      pronostico = pmax(0, pred_raw),
      inferior_80 = invert(fc$lower[, 1]), superior_80 = invert(fc$upper[, 1]),
      inferior_95 = invert(fc$lower[, 2]), superior_95 = invert(fc$upper[, 2])
    )
    arima_models[[paste(nm, lab, sep = "::")]] <- fit
  }

  valid_labs <- names(candidates)[!vapply(candidates, inherits, logical(1), "failed_model")]
  if (length(valid_labs)) {
    best_lab <- valid_labs[which.min(vapply(candidates[valid_labs], function(x) x$aicc, numeric(1)))]
    saveRDS(candidates[[best_lab]], file.path("outputs/models", paste0("arima_", slug(nm), ".rds")))
    grDevices::png(file.path("outputs/diagnostics", paste0("residuos_arima_", slug(nm), ".png")),
                   width = 1800, height = 1300, res = 180)
    forecast::checkresiduals(candidates[[best_lab]], plot = TRUE)
    grDevices::dev.off()
  }
}

arima_table <- dplyr::bind_rows(arima_rows)
write_csv_utf8(arima_table, "outputs/tables/parametros_arima.csv")
saveRDS(arima_models, "outputs/models/candidatos_arima.rds")
write_csv_utf8(dplyr::bind_rows(forecast_adjustments), "outputs/tables/ajustes_pronostico.csv")
