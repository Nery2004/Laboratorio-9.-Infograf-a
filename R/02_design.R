library(ggplot2)
library(grid)
navy <- "#17324D"; teal <- "#0A8C95"; orange <- "#E87E35"
muted <- "#536373"; rule <- "#DDE4E7"
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
numero <- function(x, digits = 1) formatC(x, format = "f", digits = digits, big.mark = ",")
# Los nombres visibles cambian; las categorías y sus valores permanecen intactos.
region_label <- function(x) {
  map <- c("América Del Centro" = "América del Centro", "América Del Norte" = "América del Norte",
           "América Del Sur y el Caribe" = "América del Sur y el Caribe")
  out <- unname(map[x]); out[is.na(out)] <- x[is.na(out)]; out
}
editorial_theme <- function() theme_minimal(base_size = 9, base_family = "Arial") +
  theme(text = element_text(color = navy), panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#E8EDEF", linewidth = .3),
    axis.title = element_blank(), axis.text = element_text(color = muted, size = 8.5),
    plot.title = element_text(face = "bold", size = 11, color = navy, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 8.5, color = muted, margin = margin(b = 9)),
    plot.margin = margin(3, 5, 3, 0), plot.title.position = "plot", legend.position = "none")
# Historia: las notas se sitúan junto a los períodos que explican.
history <- ggplot(monthly, aes(fecha, viajeros/1000)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-01"),
           ymin = -Inf, ymax = Inf, fill = "#EDF2F3") +
  geom_line(color = teal, linewidth = .42) +
  geom_line(aes(y = media_movil_12/1000), color = navy, linewidth = .85, na.rm = TRUE) +
  annotate("segment", x = as.Date("2020-03-01"), xend = as.Date("2020-03-01"),
           y = 35, yend = 375, color = orange, linewidth = .45) +
  annotate("text", x = as.Date("2019-12-01"), y = 423, label = "Marzo de 2020\nInicio de la pandemia",
           hjust = 1, size = 3, color = navy, lineheight = 1.15) +
  annotate("segment", x = as.Date("2023-01-01"), xend = as.Date("2023-01-01"),
           y = 310, yend = 405, color = muted, linewidth = .4) +
  annotate("text", x = as.Date("2023-03-01"), y = 435, label = "2023\nCambio de metodología",
           hjust = 0, size = 3, color = navy, lineheight = 1.15) +
  scale_x_date(breaks = as.Date(paste0(c(2009,2012,2015,2018,2021,2023,2026), "-01-01")),
               date_labels = "%Y", expand = expansion(mult = c(.005,.015))) +
  scale_y_continuous(breaks = seq(0,400,100), limits = c(0,475), expand = c(0,0)) + editorial_theme()
season <- ggplot(seasonality, aes(factor(mes_cod, levels = 1:12, labels = meses), promedio/1000)) +
  geom_col(aes(fill = mes_cod == kpi['mes']), width = .68) +
  geom_text(data = subset(seasonality, mes_cod == kpi['mes']), aes(label = numero(promedio/1000, 0)),
            vjust = -.5, color = orange, fontface = "bold", size = 3) +
  scale_fill_manual(values = c(teal, orange), guide = "none") +
  scale_y_continuous(breaks = c(0,100,200,300), expand = expansion(mult = c(0,.16))) +
  labs(title = "Diciembre reúne más visitantes", subtitle = "Promedio mensual, en miles · 2009–junio de 2026") + editorial_theme() +
  theme(axis.text.x = element_text(size = 8))
via_plot <- ggplot(top_vias, aes(reorder(valor, porcentaje), porcentaje)) +
  geom_col(fill = teal, width = .48) +
  geom_text(aes(label = paste0(numero(porcentaje, 2), "%")), hjust = -.16, size = 3, color = navy) +
  coord_flip() + scale_y_continuous(limits = c(0,73), breaks = c(0,20,40,60), expand = c(0,0)) +
  labs(title = "La mayoría ingresa por tierra", subtitle = "Participación de cada vía en el total, %") + editorial_theme() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#E8EDEF", linewidth = .3))
regions <- head(top_region_dos, 5)
recommendations <- c(
  paste0("Reforzar la atención en diciembre, el mes de mayor promedio, y en los accesos terrestres, que reciben cerca del ",
         numero(kpi['terrestre'],0), "% del flujo."),
  paste0("Enfocar las campañas regionales en ", region_label(regions$valor[1]), ", que concentra el ",
         numero(regions$porcentaje[1]), "% de visitantes."),
  "Contrastar las estimaciones con los ingresos recientes antes de asignar recursos: el modelo se queda corto en la recuperación.")
market_plot <- ggplot(regions, aes(reorder(region_label(valor), porcentaje), porcentaje)) +
  geom_col(fill = teal, width = .52) +
  geom_text(aes(label = paste0(numero(porcentaje), "%")), hjust = -.15, size = 3, color = navy) +
  coord_flip() + scale_y_continuous(limits = c(0,88), breaks = c(0,20,40,60,80), expand = c(0,0)) +
  labs(title = "El peso de los mercados cercanos", subtitle = "Principales regiones de origen · participación del total, %") + editorial_theme() +
  theme(axis.text.y = element_text(size = 8), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#E8EDEF", linewidth = .3))
# Se conservan las cuatro comparaciones; no se dibujan tarjetas ni fondos.
stat_row <- function(col, label, suffix, digits) {
  z <- comparativo[which.max(comparativo[[col]]), ]
  c(label, paste0(region_label(z$nombre_serie), " · ", numero(z[[col]], digits), suffix))
}
quad_values <- rbind(
  stat_row("fuerza_estacionalidad", "Mayor estacionalidad", " de 1", 2),
  stat_row("crecimiento", "Mayor crecimiento", "% al año", 1),
  stat_row("volatilidad", "Mayor volatilidad", " CV", 2),
  stat_row("caida", "Mayor caída en 2020", "%", 1))
forecast_plot <- ggplot(winner, aes(fecha)) +
  geom_ribbon(aes(ymin = inferior_80/1000, ymax = superior_80/1000), fill = teal, alpha = .12) +
  geom_line(aes(y = real/1000), color = navy, linewidth = .65) +
  geom_line(aes(y = pronostico/1000), color = orange, linewidth = .65, linetype = "22") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(.01,.01))) +
  scale_y_continuous(breaks = seq(0,400,100), expand = expansion(mult = c(.03,.05))) + editorial_theme()
total_metrics <- metricas |> dplyr::filter(nombre_serie == "Total comparable", is.finite(RMSE)) |>
  dplyr::group_by(familia) |> dplyr::slice_min(RMSE, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |> dplyr::arrange(RMSE)

# Coordenadas en pulgadas: márgenes iguales y alturas distintas según el contenido.
# El mismo dibujo se usa para los dos formatos y evita diferencias de composición.
draw_infographic <- function() {
  grid.newpage()
  text <- function(label, x, top, size = 9, color = navy, bold = FALSE, family = "Arial", just = "left") {
    grid.text(label, x = unit(x,"in"), y = unit(17-top,"in"), just = c(just,"top"),
      gp = gpar(fontfamily = family, fontsize = size, col = color,
                fontface = if (bold) "bold" else "plain", lineheight = 1.18))
  }
  line <- function(x1, x2, top, color = rule, width = .6) grid.lines(
    x = unit(c(x1,x2),"in"), y = unit(rep(17-top,2),"in"), gp = gpar(col = color,lwd = width))
  chart <- function(plot,x,top,w,h) print(plot, newpage = FALSE,
    vp = viewport(x = unit(x,"in"), y = unit(17-top,"in"), width = unit(w,"in"),
                  height = unit(h,"in"), just = c("left","top")))
  legend_line <- function(label,x,top,color,dashed = FALSE) {
    grid.lines(x = unit(c(x,x+.24),"in"), y = unit(rep(17-top-.055,2),"in"),
               gp = gpar(col = color,lwd = 1.5,lty = if (dashed) "22" else "solid"))
    text(label,x+.32,top,size = 8)
  }
  text("TURISMO  /  GUATEMALA", .65,.43,8.5,color = muted)
  text("Guatemala: 17 años de cambios\nen el turismo y lo que viene.",.65,.72,27,bold = TRUE,family = "Georgia")
  text("Enero de 2009 a junio de 2026 · Visitantes: turistas y excursionistas",.65,1.62,9,color = muted)
  kx <- c(.65,3.16,5.67,8.18)
  vals <- c(numero(kpi['total']/1e6,2),paste0("−",numero(kpi['caida'],2),"%"),
            paste0(numero(kpi['terrestre'],2),"%"),"Diciembre")
  labels <- c("millones de visitantes","caída en 2020 frente a 2019","ingresó por vía terrestre","mayor promedio mensual")
  for (i in 1:4) {
    text(vals[i],kx[i],2.02,if(i==4) 21 else 25,bold = TRUE,color = if(i==2) orange else teal)
    text(labels[i],kx[i],2.44,8.5,color = muted)
  }
  line(.65,10.35,2.79)
  text("Una caída que interrumpe años de crecimiento",.65,2.99,15,bold = TRUE)
  text("Visitantes por mes, en miles",.65,3.31,9,color = muted)
  legend_line("Cada mes",6.55,3.31,teal)
  legend_line("Media móvil de 12 meses",7.94,3.31,navy)
  chart(history,.65,3.62,9.7,2.92)
  text("2026 incluye únicamente enero a junio. Desde 2023 cambia el sistema de conteo y la agrupación de mercados.",.65,6.58,8,color = muted)
  line(.65,10.35,6.87)
  text("Cuándo llegan y por dónde",.65,7.08,15,bold = TRUE)
  chart(season,.65,7.51,5.05,1.72)
  chart(via_plot,6.06,7.51,4.29,1.72)
  chart(market_plot,.65,9.55,5.05,1.96)
  text("Qué distingue a las siete series",6.06,9.56,11,bold = TRUE)
  for(i in 1:4) {
    ty <- 9.94 + (i-1)*.36
    text(quad_values[i,1],6.06,ty,8.5,color = muted)
    text(quad_values[i,2],10.35,ty,9,bold = TRUE,just = "right")
  }
  text("Estacionalidad: fuerza STL en entrenamiento.\nCrecimiento: tasa anual 2009–2025. CV: desviación/media.\nCaída: 2020 frente a 2019; volatilidad del período completo.",
       6.06,11.39,7.5,color = muted)
  text("Promedios con todos los meses disponibles; 2026 aporta solo seis meses. Región de origen según «Región dos».",.65,11.91,7.8,color = muted)
  line(.65,10.35,12.18)
  text("Pronosticar después de la pandemia",.65,12.38,15,bold = TRUE)
  text("Prophet obtuvo el menor RMSE, pero subestima buena parte de la recuperación.",.65,12.71,9)
  chart(forecast_plot,.65,13.05,6.45,1.65)
  text("Error en visitantes",7.42,13.07,10,bold = TRUE)
  text("Modelo",7.42,13.40,8,color = muted)
  text("RMSE",9.35,13.40,8,color = muted,just = "right")
  text("MAE",10.35,13.40,8,color = muted,just = "right")
  for(i in seq_len(nrow(total_metrics))) {
    y <- 13.67+(i-1)*.24
    text(total_metrics$familia[i],7.42,y,8.2,bold = i==1)
    text(numero(total_metrics$RMSE[i],0),9.35,y,8.2,just = "right")
    text(numero(total_metrics$MAE[i],0),10.35,y,8.2,just = "right")
  }
  legend_line("Observado",.65,14.83,navy)
  legend_line("Estimación",2.08,14.83,orange,TRUE)
  grid.rect(x = unit(3.69,"in"),y = unit(17-14.885,"in"),width = unit(.23,"in"),height = unit(.11,"in"),
            just = c("left","center"),gp = gpar(fill = "#DDEEF0",col = NA))
  text("Intervalo predictivo del 80%",4.00,14.83,8)
  text("RMSE y MAE miden el error; menor es mejor.\nSe muestra la mejor variante de cada familia.",7.42,14.94,7.4,color = muted)
  text("Prueba: abr 2021–jun 2026. Entrenamiento: ene 2009–mar 2021 (70/30). La banda expresa incertidumbre, no precisión garantizada.",
       .65,15.13,7.7,color = muted)
  line(.65,10.35,15.43)
  text("Para la planificación de INGUAT",.65,15.61,12,bold = TRUE)
  text(paste(recommendations,collapse = "\n"),
       .65,15.94,8.3)
  text("Fuente: base académica INGUAT/IGM; no son cifras oficiales. Turista + Excursionista; no incluye Cruceristas.\nMarítima pierde detalle desde 2017; quiebre metodológico en 2023. Nery Molina · 23218 · UVG · CC3084 Data Science.",
       .65,16.57,7.5,color = muted)
}
ragg::agg_png("infografia/infografia_final.png",width = 3300,height = 5100,res = 300,background = "white")
draw_infographic()
invisible(dev.off())
cairo_pdf("infografia/infografia_final.pdf",width = 11,height = 17,family = "Arial",bg = "white")
draw_infographic()
invisible(dev.off())
save_plot("panorama.png",history)
save_plot("estacionalidad.png",season)
save_plot("vias.png",via_plot)
save_plot("regiones.png",market_plot)
save_plot("ganador_total.png",forecast_plot)
# Auxiliar del cuadrante sin cajas, con la misma redacción de la página.
quad <- ggplot(data.frame(label = paste(quad_values[,1],quad_values[,2],sep = "\n"),
                          x = c(0,1,0,1),y = c(1,1,0,0)),aes(x,y,label = label)) +
  geom_text(hjust = 0,size = 4,color = navy,lineheight = 1.5) +
  xlim(0,2) + ylim(-.5,1.5) + theme_void()
save_plot("cuadrante.png",quad)
