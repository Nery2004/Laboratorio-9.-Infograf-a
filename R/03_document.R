body_text <- list(
  "Organización y jerarquía visual" = c(
    "Organicé la infografía para que se lea de arriba hacia abajo. Primero aparecen el título y cuatro cifras que resumen el período: cuántos visitantes hubo, cuánto cayó el flujo en 2020, qué proporción ingresó por tierra y cuál fue el mes de mayor promedio. Después, el gráfico histórico ocupa todo el ancho porque permite entender los cambios antes de comparar meses, vías y mercados.",
    "La parte central reúne las comparaciones en dos columnas, sin repetir el mismo formato en cada apartado. En la parte inferior quedan las estimaciones y las recomendaciones. Separé los temas con espacios en blanco y líneas delgadas. Los títulos no llevan números y las cifras principales tienen más tamaño que las notas, para distinguir los niveles de lectura sin llenar la página de recuadros."),
  "Elección de gráficos" = c(
    "Para la historia de los visitantes mantuve la línea mensual y la media móvil de doce meses. La primera muestra los cambios de cada mes; la segunda ayuda a seguir la tendencia. La pandemia se señala cerca de marzo de 2020 y el período de caída tiene un fondo gris muy suave. La nota de 2023 aparece junto al cambio de metodología. Debajo del gráfico se aclara que 2026 solo llega a junio.",
    "Las barras mensuales parten de cero y permiten comparar los promedios sin exagerar las diferencias. Diciembre aparece en naranja porque tiene el valor más alto. En vías y regiones usé barras horizontales: así caben los nombres y los porcentajes se leen al lado de cada barra. Ajusté las mayúsculas de los nombres regionales para facilitar su lectura, sin cambiar las categorías.",
    "Las cuatro comparaciones entre series se presentan como pares de texto alineados, sin cajas de color. Cada una conserva el nombre de la serie y su medida. La nota inferior identifica la fuerza estacional, la tasa anual de crecimiento, el coeficiente de variación y la caída de 2020 frente a 2019. Esta aclaración importa porque no todas las comparaciones describen el mismo aspecto."),
  "Colores y tipografía" = c(
    "El fondo blanco deja espacio entre los gráficos y facilita la lectura. El azul oscuro #17324D se usa en títulos, textos y líneas de referencia; el turquesa #0A8C95 identifica las barras y la serie histórica. Ambos son colores fríos cercanos y ayudan a que la página mantenga unidad. El naranja #E87E35 aporta un contraste cálido y se reserva para la caída de 2020, diciembre y las estimaciones. Así, los acentos de color tienen un motivo relacionado con los datos.",
    "Elegí Georgia para el título y Arial para las cifras, etiquetas y textos de la infografía. El cambio de letra distingue el encabezado del contenido sin añadir adornos. Los tamaños más pequeños se reservan para las fuentes y aclaraciones. Este documento utiliza Times New Roman de doce puntos y márgenes de una pulgada, con una carátula separada."),
  "Presentación de los pronósticos" = c(
    "En el gráfico del conjunto de prueba, los valores observados se muestran con una línea azul continua y las estimaciones de Prophet con una línea naranja discontinua. La franja turquesa clara corresponde al intervalo predictivo del 80 %. Su transparencia permite ver las líneas y no oculta los cambios mensuales. La escala conserva los límites originales del intervalo, incluso cuando bajan de cero.",
    "La tabla de RMSE y MAE se ubica al lado del gráfico y conserva los errores en visitantes. Se muestra la mejor variante de cada familia. Prophet tuvo el menor RMSE, pero la línea estimada queda por debajo de buena parte de la recuperación. Por eso el texto señala esa limitación y no lo presenta como un modelo preciso. También se indican los períodos de entrenamiento y prueba y se aclara que la banda no garantiza que los valores observados queden dentro de ella."),
  "Recomendaciones y fuentes" = c(
    "Las recomendaciones retoman resultados visibles: el mayor promedio de diciembre, el peso de la vía terrestre, la concentración de visitantes en América del Centro y la subestimación del modelo durante la recuperación. Se presentan en frases breves, sin un cierre adicional. En el pie quedan la fuente académica INGUAT/IGM, la definición de visitantes, los cambios de registro indispensables y los créditos. Estas notas evitan que la base se interprete como una publicación de cifras oficiales.")
)
writeLines(c("# Decisiones de diseño", "", unlist(lapply(names(body_text), function(h)
  c(paste0("## ",h),"",paste(body_text[[h]],collapse = "\n\n"),"")))),
  "docs/decisiones_diseno.md",useBytes = TRUE)
cover <- c("UNIVERSIDAD DEL VALLE DE GUATEMALA", "Facultad de Ingeniería",
           "Departamento de Ciencias de la Computación", "CC3084 · Data Science",
           "Laboratorio 9 · Visualización Estática", "Explicación de las decisiones de diseño",
           "Nery Molina", "Carné 23218", "Guatemala, 2026")
doc <- officer::read_docx()
doc <- officer::body_set_default_section(doc,officer::prop_section(
  page_size = officer::page_size(width = 8.5,height = 11),
  page_margins = officer::page_mar(top = 1,bottom = 1,left = 1,right = 1)))
add_par <- function(doc,text,bold = FALSE,center = FALSE,size = 12,space = 8,keep = FALSE) {
  officer::body_add_fpar(doc,officer::fpar(
    officer::ftext(text,officer::fp_text(font.family = "Times New Roman",font.size = size,bold = bold)),
    fp_p = officer::fp_par(text.align = if(center) "center" else "left",padding.bottom = space,
                          line_spacing = 1.15,keep_with_next = keep)))
}
for(i in seq_along(cover)) {
  if(i %in% c(5,7,9)) doc <- add_par(doc,"",space = if(i==5) 65 else 30)
  doc <- add_par(doc,cover[i],bold = i %in% c(1,5),center = TRUE,size = if(i==1) 14 else 12)
}
doc <- officer::body_add_break(doc)
for(h in names(body_text)) {
  doc <- add_par(doc,h,bold = TRUE,space = 8,keep = TRUE)
  for(par in body_text[[h]]) doc <- add_par(doc,par)
}
print(doc,target = "docs/explicacion_diseno.docx")

# PDF con el mismo contenido y tipografía; paginación medida en puntos.
# No depende de Word ni de un servicio de conversión externo.
cairo_pdf("docs/explicacion_diseno.pdf",width = 8.5,height = 11,family = "Times New Roman")
grid::grid.newpage()
cover_y <- c(100,130,153,180,300,330,445,469,570)
for(i in seq_along(cover)) grid::grid.text(cover[i],x = unit(306,"bigpts"),
  y = unit(792-cover_y[i],"bigpts"),just = c("center","top"),
  gp = gpar(fontfamily = "Times New Roman",fontsize = if(i==1) 14 else 12,
            fontface = if(i %in% c(1,5)) "bold" else "plain"))
grid::grid.newpage()
y <- 76
wrap_par <- function(text,bold = FALSE) {
  words <- strsplit(text," ",fixed = TRUE)[[1]]
  lines <- character(); current <- ""
  for(word in words) {
    next_line <- if(nzchar(current)) paste(current,word) else word
    grob <- textGrob(next_line,gp = gpar(fontfamily = "Times New Roman",fontsize = 12,
                                        fontface = if(bold) "bold" else "plain"))
    width <- convertWidth(grobWidth(grob),"bigpts",valueOnly = TRUE)
    if(width > 468 && nzchar(current)) {lines <- c(lines,current); current <- word} else current <- next_line
  }
  c(lines,current)
}
for(h in names(body_text)) {
  # Evitar un encabezado aislado al final de página.
  if(y+65 > 720) {grid.newpage(); y <- 76}
  for(j in seq_along(c(h,body_text[[h]]))) {
    par <- c(h,body_text[[h]])[j]
    bold <- j==1
    lines <- wrap_par(par,bold)
    for(label in lines) {
      if(y+15 > 720) {grid.newpage(); y <- 76}
      grid.text(label,x = unit(72,"bigpts"),y = unit(792-y,"bigpts"),just = c("left","top"),
        gp = gpar(fontfamily = "Times New Roman",fontsize = 12,fontface = if(bold) "bold" else "plain"))
      y <- y+15
    }
    y <- y+8
  }
}
invisible(dev.off())
