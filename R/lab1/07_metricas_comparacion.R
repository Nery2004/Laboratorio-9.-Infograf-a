arima_metrics <- arima_table |>
  dplyr::transmute(
    nombre_serie, modelo, AIC, AICc, BIC, p_Ljung_Box, MAE, RMSE,
    MAPE = NA_real_, sMAPE = NA_real_, MASE = NA_real_, error
  )

# Recalcular el conjunto completo de métricas para candidatos ARIMA.
if (length(forecast_rows_arima)) {
  arima_fc <- dplyr::bind_rows(forecast_rows_arima)
  extra <- arima_fc |>
    dplyr::group_by(nombre_serie, modelo) |>
    dplyr::summarise(
      MAPE = if (any(real == 0)) NA_real_ else mean(abs((real - pronostico) / real)) * 100,
      sMAPE = mean(ifelse(abs(real) + abs(pronostico) == 0, 0,
                         200 * abs(real - pronostico) / (abs(real) + abs(pronostico)))),
      .groups = "drop"
    )
  arima_metrics <- arima_metrics |> dplyr::select(-MAPE, -sMAPE) |>
    dplyr::left_join(extra, by = c("nombre_serie", "modelo"))
  mase_arima <- lapply(split(arima_fc, interaction(arima_fc$nombre_serie, arima_fc$model)), function(z) {
    train <- series_objects[[z$nombre_serie[1]]]$train$viajeros
    scale <- mean(abs(diff(train, lag = 12)))
    data.frame(nombre_serie = z$nombre_serie[1], modelo = z$modelo[1],
               MASE = mean(abs(z$real - z$pronostico)) / scale)
  }) |> dplyr::bind_rows()
  arima_metrics <- arima_metrics |> dplyr::select(-MASE) |>
    dplyr::left_join(mase_arima, by = c("nombre_serie", "modelo"))
}

metricas <- dplyr::bind_rows(arima_metrics, alternative_table) |>
  dplyr::mutate(
    familia = dplyr::case_when(
      grepl("^ARIMA", modelo) ~ "ARIMA",
      grepl("^ETS", modelo) ~ "ETS",
      grepl("^Holt", modelo) ~ "Holt-Winters",
      modelo == "Prophet" ~ "Prophet",
      modelo == "Seasonal_Naive" ~ "Seasonal Naive",
      TRUE ~ modelo
    )
  ) |>
  dplyr::group_by(nombre_serie) |>
  dplyr::arrange(RMSE, MAE, .by_group = TRUE) |>
  dplyr::mutate(ranking = dplyr::row_number()) |>
  dplyr::ungroup()

mejores <- metricas |>
  dplyr::filter(is.finite(RMSE), is.finite(MAE)) |>
  dplyr::group_by(nombre_serie) |>
  dplyr::arrange(RMSE, MAE, .by_group = TRUE) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()

pronosticos <- dplyr::bind_rows(
  if (length(forecast_rows_arima)) dplyr::bind_rows(forecast_rows_arima) else NULL,
  forecast_table_alt
)

write_csv_utf8(metricas, "outputs/tables/metricas_modelos.csv")
write_csv_utf8(mejores, "outputs/tables/mejor_modelo_por_serie.csv")
write_csv_utf8(metricas |> dplyr::select(nombre_serie, modelo, familia, ranking, RMSE, MAE),
               "outputs/tables/ranking_modelos.csv")
write_csv_utf8(pronosticos, "outputs/tables/pronosticos_prueba.csv")

plot_metric <- function(metric, filename) {
  plot_data <- metricas |> dplyr::filter(is.finite(.data[[metric]]))
  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(x = reorder(modelo, .data[[metric]]),
                                    y = .data[[metric]], fill = familia)) +
    ggplot2::geom_col() + ggplot2::coord_flip() +
    ggplot2::facet_wrap(~nombre_serie, scales = "free", ncol = 2) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(title = paste(metric, "en el conjunto de prueba"),
                  x = NULL, y = metric, fill = "Familia") + theme_lab()
  save_plot(filename, p, width = 11, height = 12)
}
plot_metric("RMSE", "comparacion_rmse.png")
plot_metric("MAE", "comparacion_mae.png")

for (nm in series_names) {
  best <- dplyr::filter(mejores, nombre_serie == nm)
  if (!nrow(best)) next
  z <- pronosticos |>
    dplyr::filter(nombre_serie == nm, modelo == best$modelo[[1]])
  p <- ggplot2::ggplot(z, ggplot2::aes(fecha)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = pmax(0, inferior_95), ymax = superior_95),
                         fill = "#9ECAE1", alpha = .35, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = real, color = "Real"), linewidth = .7) +
    ggplot2::geom_line(ggplot2::aes(y = pronostico, color = "Pronóstico"), linewidth = .7) +
    ggplot2::scale_color_manual(values = c("Real" = "#222222", "Pronóstico" = "#D95F0E")) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(title = paste("Prueba:", nm), subtitle = paste("Mejor modelo:", best$modelo[[1]]),
                  x = NULL, y = "Visitantes", color = NULL) + theme_lab()
  save_plot(paste0("pronostico_", slug(nm), ".png"), p)
}
