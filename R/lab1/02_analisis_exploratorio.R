comp <- datos_comparables
all_data <- datos_limpios

monthly <- comp |>
  dplyr::group_by(fecha) |>
  dplyr::summarise(viajeros = sum(viajero, na.rm = TRUE), .groups = "drop") |>
  tidyr::complete(fecha = seq(min(fecha), max(fecha), by = "month"),
                  fill = list(viajeros = 0)) |>
  dplyr::arrange(fecha) |>
  dplyr::mutate(media_movil_12 = zoo::rollmean(viajeros, 12, fill = NA, align = "right"))

annual <- monthly |>
  dplyr::mutate(ano = lubridate::year(fecha)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(total = sum(viajeros), promedio_mensual = mean(viajeros),
                   meses = dplyr::n(), .groups = "drop") |>
  dplyr::mutate(variacion_anual_pct = 100 * (total / dplyr::lag(total) - 1),
                comparable_anual = meses == 12)

jan_jun <- monthly |>
  dplyr::filter(lubridate::month(fecha) <= 6,
                lubridate::year(fecha) %in% c(2019, 2023, 2024, 2025, 2026)) |>
  dplyr::mutate(ano = lubridate::year(fecha)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(enero_junio = sum(viajeros), .groups = "drop") |>
  dplyr::mutate(cambio_vs_2026_pct = 100 * (enero_junio[ano == 2026] / enero_junio - 1))

seasonality <- monthly |>
  dplyr::mutate(mes_cod = lubridate::month(fecha),
                mes = factor(month.abb[mes_cod], levels = month.abb)) |>
  dplyr::group_by(mes_cod, mes) |>
  dplyr::summarise(promedio = mean(viajeros), mediana = median(viajeros),
                   q1 = quantile(viajeros, .25), q3 = quantile(viajeros, .75), .groups = "drop")

summ_v <- data.frame(
  estadistica = c("Mínimo", "Máximo", "Media", "Mediana", "Desviación estándar",
                  "Primer cuartil", "Tercer cuartil", "Rango intercuartílico", "Suma"),
  valor = c(min(all_data$viajero, na.rm = TRUE), max(all_data$viajero, na.rm = TRUE),
            mean(all_data$viajero, na.rm = TRUE), median(all_data$viajero, na.rm = TRUE),
            sd(all_data$viajero, na.rm = TRUE), quantile(all_data$viajero, .25, na.rm = TRUE),
            quantile(all_data$viajero, .75, na.rm = TRUE), IQR(all_data$viajero, na.rm = TRUE),
            sum(all_data$viajero, na.rm = TRUE))
)

top_table <- function(var, n = 10) {
  comp |>
    dplyr::mutate(valor = dplyr::coalesce(as.character(.data[[var]]), "No reportado")) |>
    dplyr::group_by(valor) |>
    dplyr::summarise(viajeros = sum(viajero), .groups = "drop") |>
    dplyr::mutate(porcentaje = 100 * viajeros / sum(viajeros)) |>
    dplyr::arrange(dplyr::desc(viajeros)) |>
    dplyr::slice_head(n = n)
}

top_paises <- top_table("pais")
top_region <- top_table("region")
top_region_dos <- top_table("region_dos")
top_omt <- top_table("regiones_omt")
top_vias <- top_table("via", 20)
top_fronteras <- top_table("frontera")

outlier_rows <- all_data |>
  dplyr::mutate(
    q1 = quantile(viajero, .25, na.rm = TRUE), q3 = quantile(viajero, .75, na.rm = TRUE),
    iqr = q3 - q1, med = median(viajero, na.rm = TRUE),
    mad_robusto = mad(viajero, na.rm = TRUE),
    atipico_iqr = viajero < q1 - 1.5 * iqr | viajero > q3 + 1.5 * iqr,
    atipico_mad = abs(viajero - med) > 3.5 * mad_robusto,
    clasificacion = dplyr::case_when(
      viajero < 0 ~ "posible error",
      fecha >= as.Date("2020-03-01") & fecha <= as.Date("2021-12-01") ~ "pandemia",
      fecha >= as.Date("2023-01-01") ~ "cambio metodológico u observación que requiere revisión",
      TRUE ~ "observación que requiere revisión"
    )
  ) |>
  dplyr::filter(atipico_iqr | atipico_mad) |>
  dplyr::select(-q1, -q3, -iqr, -med, -mad_robusto)

stl_all <- stl(stats::ts(monthly$viajeros, frequency = 12), s.window = "periodic", robust = TRUE)
resid_stl <- as.numeric(stl_all$time.series[, "remainder"])
med_r <- median(resid_stl, na.rm = TRUE)
mad_r <- mad(resid_stl, na.rm = TRUE)
outlier_months <- monthly |>
  dplyr::mutate(residuo_stl = resid_stl,
                atipico_stl = abs(residuo_stl - med_r) > 3.5 * mad_r,
                clasificacion = dplyr::case_when(
                  fecha >= as.Date("2020-03-01") & fecha <= as.Date("2021-12-01") ~ "pandemia",
                  fecha >= as.Date("2023-01-01") ~ "cambio metodológico",
                  TRUE ~ "evento real u observación que requiere revisión"
                )) |>
  dplyr::filter(atipico_stl)

write_csv_utf8(summ_v, "outputs/tables/estadisticas_viajero.csv")
write_csv_utf8(annual, "outputs/tables/totales_anuales.csv")
write_csv_utf8(jan_jun, "outputs/tables/comparacion_enero_junio.csv")
write_csv_utf8(seasonality, "outputs/tables/estacionalidad_mensual.csv")
write_csv_utf8(top_paises, "outputs/tables/top_paises.csv")
write_csv_utf8(dplyr::bind_rows(
  dplyr::mutate(top_region, nivel = "Región"),
  dplyr::mutate(top_region_dos, nivel = "Región dos"),
  dplyr::mutate(top_omt, nivel = "Regiones OMT")
), "outputs/tables/top_regiones.csv")
write_csv_utf8(top_vias, "outputs/tables/top_vias.csv")
write_csv_utf8(top_fronteras, "outputs/tables/top_fronteras.csv")
write_csv_utf8(outlier_rows, "outputs/tables/atipicos_filas.csv")
write_csv_utf8(outlier_months, "outputs/tables/atipicos_mensuales.csv")

via_region <- comp |>
  dplyr::group_by(via, region_dos) |>
  dplyr::summarise(viajeros = sum(viajero), .groups = "drop") |>
  dplyr::group_by(via) |>
  dplyr::mutate(porcentaje_en_via = 100 * viajeros / sum(viajeros)) |>
  dplyr::ungroup()
border_via <- comp |>
  dplyr::group_by(frontera, via) |>
  dplyr::summarise(viajeros = sum(viajero), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(viajeros))
via_annual <- comp |>
  dplyr::mutate(ano = lubridate::year(fecha)) |>
  dplyr::group_by(ano, via) |>
  dplyr::summarise(viajeros = sum(viajero), .groups = "drop")
traveler_type <- all_data |>
  dplyr::group_by(tipo_de_viajero) |>
  dplyr::summarise(viajeros = sum(viajero), .groups = "drop") |>
  dplyr::mutate(porcentaje = 100 * viajeros / sum(viajeros))
write_csv_utf8(via_region, "outputs/tables/cruce_via_region_dos.csv")
write_csv_utf8(border_via, "outputs/tables/cruce_frontera_via.csv")
write_csv_utf8(via_annual, "outputs/tables/evolucion_anual_vias.csv")
write_csv_utf8(traveler_type, "outputs/tables/totales_tipo_viajero.csv")

markers <- list(
  ggplot2::geom_vline(xintercept = as.Date("2020-03-01"), linetype = 2, color = "#B2182B"),
  ggplot2::geom_vline(xintercept = as.Date("2023-01-01"), linetype = 3, color = "#2166AC")
)
p_month <- ggplot2::ggplot(monthly, ggplot2::aes(fecha, viajeros)) +
  ggplot2::geom_line(color = "#173F5F", linewidth = .55) +
  ggplot2::geom_line(ggplot2::aes(y = media_movil_12), color = "#F28E2B", linewidth = .8) +
  markers + ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  ggplot2::labs(
    title = "Visitantes internacionales comparables por mes",
    subtitle = "Turista + Excursionista; línea naranja: media móvil de 12 meses",
    x = NULL, y = "Visitantes",
    caption = "Líneas: marzo de 2020 (pandemia) y enero de 2023 (cambio metodológico)."
  ) + theme_lab()
save_plot("total_mensual_comparable.png", p_month)

p_annual <- ggplot2::ggplot(annual, ggplot2::aes(ano, total, fill = comparable_anual)) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(values = c("#9ECAE1", "#2171B5"), guide = "none") +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  ggplot2::labs(title = "Total anual comparable", subtitle = "2026 contiene únicamente enero-junio",
                x = "Año", y = "Visitantes") + theme_lab()
save_plot("total_anual_comparable.png", p_annual)

p_season <- ggplot2::ggplot(monthly |>
                              dplyr::mutate(mes = factor(month.abb[lubridate::month(fecha)],
                                                         levels = month.abb)),
                            ggplot2::aes(mes, viajeros)) +
  ggplot2::geom_boxplot(fill = "#6BAED6", outlier.alpha = .35) +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  ggplot2::labs(title = "Distribución estacional por mes", x = "Mes", y = "Visitantes") +
  theme_lab()
save_plot("distribucion_estacional.png", p_season)

plot_top_evolution <- function(var, top_values, filename, title) {
  dat <- comp |>
    dplyr::filter(.data[[var]] %in% top_values) |>
    dplyr::group_by(fecha, serie = .data[[var]]) |>
    dplyr::summarise(viajeros = sum(viajero), .groups = "drop")
  p <- ggplot2::ggplot(dat, ggplot2::aes(fecha, viajeros, color = serie)) +
    ggplot2::geom_line(linewidth = .45) + markers +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(title = title, x = NULL, y = "Visitantes", color = NULL,
                  caption = "Comparabilidad de países pequeños y detalle marítimo cambian en el período.") +
    theme_lab()
  save_plot(filename, p)
}
plot_top_evolution("pais", head(top_paises$valor, 5), "evolucion_mercados.png",
                   "Principales países o agrupaciones reportadas")
plot_top_evolution("region_dos", head(top_region_dos$valor, 5), "evolucion_regiones.png",
                   "Evolución de las principales regiones")
plot_top_evolution("via", head(top_vias$valor, 3), "evolucion_vias.png",
                   "Evolución mensual por vía de ingreso")
plot_top_evolution("frontera", head(top_fronteras$valor, 5), "evolucion_fronteras.png",
                   "Evolución de las principales fronteras")
plot_top_evolution("tipo_de_viajero",
                   unique(all_data$tipo_de_viajero), "evolucion_tipo_viajero.png",
                   "Quiebre en las categorías de tipo de viajero")
