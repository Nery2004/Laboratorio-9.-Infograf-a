library(ggplot2)
library(patchwork)
navy <- "#17324D"; teal <- "#0A8C95"; orange <- "#E87E35"
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
numero <- function(x, digits = 1) formatC(x, format = "f", digits = digits, big.mark = ",")
text_panel <- function(lines, size = 3.2, title = NULL) {
  z <- data.frame(label = lines, y = rev(seq_along(lines)))
  ggplot(z, aes(x = 0, y = y, label = label)) +
    geom_text(hjust = 0, size = size, color = navy, family = "Arial", lineheight = 1.1) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(.3, length(lines) + .7)) +
    labs(title = title) + theme_void(base_family = "Arial") +
    theme(plot.title = element_text(size = 12, face = "bold", color = navy),
          plot.margin = margin(5, 10, 5, 10))
}
heading <- text_panel(c("Guatemala: 17 años de cambios", "en el turismo y lo que viene."), size = 7)
kpi_data <- data.frame(x = 1:4,
  value = c(paste0(numero(kpi['total']/1e6, 2), " M"),
            paste0("−", numero(kpi['caida'], 2), "%"),
            paste0(numero(kpi['terrestre'], 2), "%"), meses[kpi['mes']]),
  label = c("Visitantes · ene 2009–jun 2026", "2020 frente a 2019", "Ingresos por vía terrestre", "Mayor promedio mensual"))
kpis <- ggplot(kpi_data, aes(x, 1)) +
  geom_text(aes(label = value), color = teal, size = 7, fontface = "bold") +
  geom_text(aes(y = .55, label = label), color = navy, size = 2.9) +
  xlim(.45, 4.55) + ylim(.2, 1.4) + theme_void()
history <- ggplot(monthly, aes(fecha, viajeros/1000)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-01"), ymin = -Inf, ymax = Inf, fill = orange, alpha = .10) +
  geom_line(aes(color = "Observado"), linewidth = .4) +
  geom_line(aes(y = media_movil_12/1000, color = "Media móvil · 12 meses"), linewidth = .7, na.rm = TRUE) +
  geom_vline(xintercept = as.Date("2023-01-01"), linetype = 3, color = navy) +
  geom_vline(xintercept = as.Date("2026-01-01"), linetype = 3, color = navy) +
  scale_color_manual(values = c("Observado" = teal, "Media móvil · 12 meses" = orange)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(.01,.03))) +
  labs(title = "02  |  Panorama histórico", subtitle = "Turista + Excursionista · enero 2009 a junio 2026",
    caption = "2020–2021: pandemia  •  2023: sistema depurado  •  2026: solo enero–junio",
    x = NULL, y = "Miles de visitantes", color = NULL) + theme_lab()
season <- ggplot(seasonality, aes(factor(mes_cod, levels = 1:12, labels = meses), promedio/1000)) +
  geom_col(aes(fill = mes_cod == kpi['mes']), width = .65) +
  scale_fill_manual(values = c(teal, orange), guide = "none") +
  labs(title = "03  |  Estacionalidad, vías y mercados", subtitle = "Promedio por mes · todos los años disponibles",
       x = NULL, y = "Miles de visitantes") + theme_lab()
via_plot <- ggplot(top_vias, aes(reorder(valor, porcentaje), porcentaje)) +
  geom_col(fill = teal, width = .6) + geom_text(aes(label = paste0(numero(porcentaje), "%")), hjust = -.1, size = 3) +
  coord_flip() + scale_y_continuous(limits = c(0, max(top_vias$porcentaje)*1.25)) +
  labs(title = "Vías de ingreso", subtitle = "Participación acumulada · 2009–jun 2026", x = NULL, y = "% de visitantes") + theme_lab()
regions <- head(top_region_dos, 5)
market_plot <- ggplot(regions, aes(reorder(valor, viajeros), viajeros/1e6)) +
  geom_col(fill = teal, width = .6) +
  geom_text(aes(label = paste0(numero(porcentaje), "%")), hjust = -.1, size = 3) +
  coord_flip() + scale_y_continuous(limits = c(0, max(regions$viajeros/1e6)*1.28)) +
  labs(title = "Principales regiones", subtitle = "Clasificación Región dos · participación del total", x = NULL, y = "Millones de visitantes") + theme_lab()
winner_stat <- function(col, label, suffix, digits) {
  z <- comparativo[which.max(comparativo[[col]]), ]
  paste0(label, "\n", z$nombre_serie, " · ", numero(z[[col]], digits), suffix)
}
quad_labels <- c(winner_stat("fuerza_estacionalidad", "MAYOR ESTACIONALIDAD", " / 1", 2),
                 winner_stat("crecimiento", "MAYOR CRECIMIENTO", "% anual", 1),
                 winner_stat("volatilidad", "MAYOR VOLATILIDAD", " CV", 2),
                 winner_stat("caida", "MAYOR CAÍDA EN 2020", "%", 1))
quad <- ggplot(data.frame(x = c(1,2,1,2), y = c(2,2,1,1), label = quad_labels), aes(x,y,label = label)) +
  geom_tile(fill = "#F2F7F8", width = .96, height = .9) +
  geom_text(size = 3.1, color = navy, lineheight = 1.3) +
  labs(caption = "STL: entrenamiento · crecimiento: CAGR 2009–2025 · CV: desviación/media, período completo\nCaída: total 2020 vs. 2019 · siete series; los niveles incluyen quiebres metodológicos") +
  theme_void() + theme(plot.caption = element_text(size = 8, color = navy, hjust = 0))
forecast_plot <- ggplot(winner, aes(fecha)) +
  geom_ribbon(aes(ymin = inferior_80/1000, ymax = superior_80/1000, fill = "Estimación · intervalo predictivo 80%"), alpha = .18) +
  geom_line(aes(y = real/1000, color = "Observado"), linewidth = .6) +
  geom_line(aes(y = pronostico/1000, color = "Estimación"), linewidth = .7, linetype = 2) +
  scale_color_manual(values = c("Observado" = navy, "Estimación" = orange)) +
  scale_fill_manual(values = c("Estimación · intervalo predictivo 80%" = teal)) +
  labs(title = "04  |  Modelos y pronósticos", subtitle = paste("Total comparable · ganador por RMSE:", best_total$modelo),
       x = NULL, y = "Miles de visitantes", color = NULL, fill = NULL,
       caption = "Entrenamiento: ene 2009–mar 2021 (147 meses).\nPrueba: abr 2021–jun 2026 (63 meses). Evaluación retrospectiva.\nEl intervalo no garantiza cobertura ante cambios de método.") + theme_lab() +
  theme(legend.text = element_text(size = 8), legend.box = "vertical",
        plot.caption = element_text(hjust = 0, size = 8))
total_metrics <- metricas |> dplyr::filter(nombre_serie == "Total comparable", is.finite(RMSE)) |>
  dplyr::group_by(familia) |> dplyr::slice_min(RMSE, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |> dplyr::arrange(RMSE)
metric_rows <- data.frame(y = rev(seq_len(nrow(total_metrics))),
                          familia = total_metrics$familia, RMSE = total_metrics$RMSE, MAE = total_metrics$MAE)
metrics_panel <- ggplot(metric_rows, aes(y = y)) +
  geom_text(aes(x = 0, label = familia), hjust = 0, size = 3, color = navy) +
  geom_text(aes(x = .69, label = numero(RMSE, 0)), hjust = 1, size = 3, color = navy) +
  geom_text(aes(x = 1, label = numero(MAE, 0)), hjust = 1, size = 3, color = navy) +
  annotate("text", x = c(0,.69,1), y = nrow(metric_rows) + 1,
           label = c("Familia", "RMSE", "MAE"), hjust = c(0,1,1), size = 3, fontface = "bold", color = navy) +
  scale_x_continuous(limits = c(0,1), expand = expansion(mult = .02)) +
  scale_y_continuous(limits = c(0,nrow(metric_rows) + 2)) +
  labs(title = "Errores en prueba", subtitle = "Visitantes · menor es mejor") + theme_void() +
  theme(plot.title = element_text(size = 12, face = "bold", color = navy),
        plot.subtitle = element_text(size = 9, color = navy), plot.margin = margin(10,8,5,8))
recommendations <- c(
  paste0("1. Preparar personal y capacidad para ", meses[kpi['mes']], "; registra el mayor promedio mensual."),
  paste0("2. Priorizar atención en fronteras terrestres: concentran ", numero(kpi['terrestre']), "% del flujo comparable."),
  paste0("3. Segmentar campañas para ", regions$valor[1], " (", numero(regions$porcentaje[1]), "% del total)."),
  "4. Actualizar los pronósticos mensualmente y planificar escenarios con el intervalo predictivo.")
footer <- text_panel(c(recommendations,
  "Fuente: base académica INGUAT/IGM; no constituye cifras oficiales. Turista + Excursionista.",
  "Desde 2023: mercados agrupados y sistema depurado; marítima pierde detalle desde 2017.",
  "Cruceristas excluidos; 2026 parcial. Elaboración: Nery Molina · 23218 · UVG · CC3084."),
  size = 3, title = "05  |  Recomendaciones y fuentes")
composition <- heading / kpis / history / season / (via_plot | market_plot) / quad /
  ((forecast_plot | metrics_panel) + plot_layout(widths = c(2.1,1))) / footer +
  plot_layout(heights = c(.85,.8,2.15,1.6,1.65,1.35,2.65,1.65))
composition <- composition & theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("infografia/infografia_final.png", composition, width = 11, height = 17, dpi = 300,
       device = ragg::agg_png, bg = "white")
ggsave("infografia/infografia_final.pdf", composition, width = 11, height = 17,
       device = cairo_pdf, family = "Arial", bg = "white")
save_plot("panorama.png", history)
save_plot("estacionalidad.png", season)
save_plot("vias.png", via_plot)
save_plot("regiones.png", market_plot)
save_plot("cuadrante.png", quad)
save_plot("ganador_total.png", forecast_plot)
