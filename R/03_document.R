body_text <- c(
  "Elección de gráficos" = paste(
    "Usé una línea mensual para seguir los cambios de los visitantes entre enero de 2009 y junio de 2026.",
    "La media móvil de doce meses ayuda a leer la tendencia sin ocultar los valores observados.",
    "Las barras permiten comparar promedios mensuales, vías de ingreso y regiones sin depender de áreas o perspectivas.",
    "Las regiones se presentan horizontalmente para conservar sus nombres legibles.",
    "El cuadrante resume las series con mayor fuerza estacional STL, crecimiento anual compuesto, coeficiente de variación y caída en 2020."),
  "Orden de presentación y jerarquía visual" = paste(
    "Organicé la página de arriba hacia abajo en cinco bloques: titular e indicadores, panorama histórico,",
    "estacionalidad con vías y mercados, evaluación de modelos y recomendaciones con fuentes.",
    "El título y los cuatro indicadores ofrecen una entrada rápida; después se muestran el contexto y las comparaciones.",
    "El tamaño de los números, los encabezados numerados y los espacios en blanco distinguen los niveles de lectura."),
  "Paleta de colores" = paste(
    "El azul oscuro #17324D sostiene títulos y textos por su contraste sobre el blanco.",
    "El turquesa #0A8C95 mantiene una relación de colores cercanos con el azul y unifica las barras y las series históricas.",
    "El naranja #E87E35 introduce un contraste cálido con esos tonos fríos para destacar el máximo mensual, la media móvil y las estimaciones.",
    "También usé una línea discontinua para que el pronóstico se distinga sin depender solo del color."),
  "Tipografía e indicadores" = paste(
    "La infografía utiliza Arial para leer números, etiquetas y títulos con facilidad.",
    "Este documento utiliza Times New Roman de doce puntos y márgenes de una pulgada.",
    "Los indicadores calculados son el acumulado de visitantes, la disminución de 2020 frente a 2019,",
    "la participación terrestre y el mes de mayor promedio. Visitantes significa Turista más Excursionista.",
    "Los promedios mensuales incluyen todos los años disponibles; enero a junio tienen una observación adicional por el período parcial de 2026.",
    "El crecimiento del cuadrante compara los años completos 2009 y 2025 y la estacionalidad usa únicamente entrenamiento."),
  "Tratamiento de los pronósticos" = paste(
    "La comparación utiliza 147 meses de entrenamiento y 63 meses de prueba, en orden cronológico.",
    "Se comparan ARIMA, Prophet, Holt-Winters, ETS y Seasonal Naive con RMSE y MAE.",
    "La configuración de Holt-Winters se escoge con los últimos 24 meses del entrenamiento; luego se reajusta con los 147 meses.",
    "El modelo mostrado para el total es", best_total$modelo, "porque obtuvo el menor RMSE de prueba.",
    "Las predicciones negativas se limitan a cero para evaluar los errores, igual que en el Laboratorio 1; los límites originales se conservan.",
    "Si la corrección de sesgo al invertir Box-Cox produce una media no finita, se utiliza la inversa sin esa corrección y se registra el ajuste; no se omiten meses al medir errores.",
    "La banda corresponde al intervalo predictivo real del 80 % producido por el modelo y se identifica como Estimación.",
    "El gráfico muestra una evaluación retrospectiva sobre prueba, no datos futuros observados ni una garantía de cobertura.",
    "Las notas señalan la pandemia, el cambio metodológico de 2023, la pérdida de detalle marítimo desde 2017 y el semestre parcial de 2026.",
    "La fuente es una base académica INGUAT/IGM y sus cifras no se presentan como oficiales.")
)
writeLines(c("# Decisiones de diseño", "", paste0("## ", names(body_text), "\n\n", body_text)),
           "docs/decisiones_diseno.md", useBytes = TRUE)
doc <- officer::read_docx()
doc <- officer::body_set_default_section(doc, officer::prop_section(
  page_size = officer::page_size(width = 8.5, height = 11),
  page_margins = officer::page_mar(top = 1, bottom = 1, left = 1, right = 1)))
add_par <- function(doc, text, bold = FALSE, center = FALSE, size = 12) {
  officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(text, officer::fp_text(font.family = "Times New Roman", font.size = size, bold = bold)),
    fp_p = officer::fp_par(text.align = if (center) "center" else "left", padding.bottom = 10)))
}
for (line in c("UNIVERSIDAD DEL VALLE DE GUATEMALA", "CC3084 · Data Science", "Laboratorio 9 · Visualización Estática",
               "Decisiones de diseño de la infografía", "Nery Molina", "Carné 23218", "2026"))
  doc <- add_par(doc, line, bold = TRUE, center = TRUE, size = 14)
doc <- officer::body_add_break(doc)
for (i in seq_along(body_text)) {
  doc <- add_par(doc, names(body_text)[i], bold = TRUE)
  doc <- add_par(doc, body_text[i])
}
print(doc, target = "docs/explicacion_diseno.docx")
