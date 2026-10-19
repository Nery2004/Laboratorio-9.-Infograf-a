checks <- list()
check <- function(name, value) checks[[length(checks) + 1]] <<- data.frame(
  comprobacion = name, resultado = if (isTRUE(value)) "PASS" else "FAIL")
check("161036 filas originales", nrow(raw) == 161036)
check("210 meses consecutivos", identical(monthly$fecha, seq(as.Date("2009-01-01"), as.Date("2026-06-01"), by = "month")))
check("Conservación de totales después de limpieza", isTRUE(all.equal(sum(datos$viajero), sum(datos_limpios$viajero))))
check("Definición comparable Lab 1", all(norm(datos_comparables$tipo_de_viajero) %in% c("turista", "excursionista")))
check("Siete series", length(series_names) == 7)
# Reconstrucción independiente sobre filas originales, sin funciones del pipeline.
original_comp <- datos[norm(datos$tipo_de_viajero) %in% c("turista", "excursionista"), ]
independent <- aggregate(viajero ~ fecha, original_comp, sum)
check("Total mensual independiente coincide con Lab 1", isTRUE(all.equal(independent$viajero, monthly$viajeros)))
for (nm in series_names) {
  obj <- series_objects[[nm]]
  if (nm == "Total comparable") subset_raw <- original_comp else {
    col <- if (unique(obj$data$categoria_analisis) == "Vía de ingreso") "via" else "region_dos"
    subset_raw <- original_comp[original_comp[[col]] == nm & !is.na(original_comp[[col]]), ]
  }
  agg <- aggregate(viajero ~ fecha, subset_raw, sum)
  expected_y <- agg$viajero[match(all_months, agg$fecha)]
  expected_y[is.na(expected_y)] <- 0
  check(paste("Reconstrucción original", nm), isTRUE(all.equal(expected_y, obj$data$viajeros)))
  check(paste("70/30 sin traslape", nm), nrow(obj$train) == 147 && nrow(obj$test) == 63 && max(obj$train$fecha) < min(obj$test$fecha))
  check(paste("Cinco familias completas", nm), length(unique(metricas$familia[metricas$nombre_serie == nm & is.finite(metricas$RMSE)])) == 5)
}
check("KPI acumulado ~46.71 millones", abs(kpi['total']/1e6 - 46.71) < .01)
check("KPI caída ~74.35%", abs(kpi['caida'] - 74.35) < .02)
check("KPI terrestre ~59.07%", abs(kpi['terrestre'] - 59.07) < .02)
check("KPI máximo diciembre", kpi['mes'] == 12)
for (group in split(pronosticos, interaction(pronosticos$nombre_serie, pronosticos$modelo, drop = TRUE))) {
  m <- metricas[metricas$nombre_serie == group$nombre_serie[1] & metricas$modelo == group$modelo[1], ]
  obj <- series_objects[[group$nombre_serie[1]]]
  check(paste("Fechas y reales", group$nombre_serie[1], group$modelo[1]),
    identical(as.Date(group$fecha), obj$test$fecha) && identical(as.numeric(group$real), as.numeric(obj$test$viajeros)))
  check(paste("RMSE y MAE independientes", group$nombre_serie[1], group$modelo[1]),
    nrow(m) == 1 && abs(sqrt(sum((group$real-group$pronostico)^2)/nrow(group)) - m$RMSE) < 1e-6 &&
      abs(sum(abs(group$real-group$pronostico))/nrow(group)-m$MAE) < 1e-6)
}
check("Ganador menor RMSE total", best_total$RMSE == min(metricas$RMSE[metricas$nombre_serie == "Total comparable"], na.rm = TRUE))
arima_audit <- readRDS("outputs/models/candidatos_arima.rds")
for (key in names(arima_audit)) {
  nm <- strsplit(key, "::", fixed = TRUE)[[1]][1]
  obj <- series_objects[[nm]]
  expected <- if (obj$transform == "log1p") log1p(obj$train$viajeros) else obj$train$viajeros
  check(paste("Entrada real ARIMA solo entrenamiento", key),
        isTRUE(all.equal(as.numeric(arima_audit[[key]]$x), as.numeric(expected))))
}
alternative_audit <- readRDS("outputs/models/modelos_alternativos.rds")
for (key in names(alternative_audit)) {
  nm <- strsplit(key, "::", fixed = TRUE)[[1]][1]
  fit <- alternative_audit[[key]]
  obj <- series_objects[[nm]]
  if (grepl("::Prophet$", key)) {
    actual_input <- fit$history$y
    check(paste("Fechas reales de ajuste Prophet", nm), identical(as.Date(fit$history$ds), obj$train$fecha))
  } else actual_input <- fit$x
  check(paste("Entrada real alternativa solo entrenamiento", key),
        isTRUE(all.equal(as.numeric(actual_input), as.numeric(obj$train$viajeros))))
}
hw_selection <- read.csv("outputs/tables/seleccion_hw_entrenamiento.csv")
check("Holt-Winters selección interna anterior a prueba", nrow(hw_selection) >= 14 &&
      all(as.Date(hw_selection$validacion_hasta) < as.Date("2021-04-01")))
check("Intervalo 80 real finito y ordenado", all(is.finite(winner$inferior_80)) && all(is.finite(winner$superior_80)) && all(winner$inferior_80 <= winner$superior_80))
img <- png::readPNG("infografia/infografia_final.png", native = TRUE)
check("PNG 3300 x 5100", identical(dim(img)[1:2], c(5100L, 3300L)))
pdf <- pdftools::pdf_info("infografia/infografia_final.pdf")
check("PDF exactamente una página", pdf$pages == 1)
page <- pdftools::pdf_pagesize("infografia/infografia_final.pdf")
check("PDF 11 x 17 pulgadas", abs(page$width[1] - 792) < .01 && abs(page$height[1] - 1224) < .01)
check("Documento Word presente", file.exists("docs/explicacion_diseno.docx"))
doc_dir <- tempfile("lab9_doc_")
dir.create(doc_dir)
unzip("docs/explicacion_diseno.docx", files = "word/document.xml", exdir = doc_dir)
doc_xml <- xml2::read_xml(file.path(doc_dir, "word/document.xml"))
ns <- xml2::xml_ns(doc_xml)
margins <- xml2::xml_find_all(doc_xml, "//w:pgMar", ns)
check("Word márgenes de una pulgada", all(vapply(c("top", "bottom", "left", "right"),
  function(side) all(xml2::xml_attr(margins, paste0("w:", side), ns) == "1440"), logical(1))))
fonts <- xml2::xml_find_all(doc_xml, "//w:rFonts", ns)
check("Word Times New Roman", length(fonts) > 0 && all(xml2::xml_attr(fonts, "w:ascii", ns) == "Times New Roman"))
pdf_text <- paste(pdftools::pdf_text("infografia/infografia_final.pdf"), collapse = " ")
check("PDF créditos y estimación", grepl("Nery Molina", pdf_text, fixed = TRUE) && grepl("23218", pdf_text) && grepl("Estimación", pdf_text, fixed = TRUE))
check("Infografía sin caracteres sustituidos", !grepl("?", pdf_text, fixed = TRUE) && !grepl("\uFFFD", pdf_text, fixed = TRUE))
check("Encabezados sin numeración decorativa", !grepl("0[2-5] *[|]", pdf_text))
check("Limitación de Prophet explícita", grepl("subestima", pdf_text, fixed = TRUE))
doc_pdf <- "docs/explicacion_diseno.pdf"
check("Documento PDF presente", file.exists(doc_pdf))
doc_pdf_text <- paste(pdftools::pdf_text(doc_pdf), collapse = " ")
doc_word_text <- paste(xml2::xml_text(xml2::xml_find_all(doc_xml, "//w:t", ns)), collapse = " ")
compact <- function(x) gsub("[[:space:]]+", "", x)
check("Word y PDF contienen el mismo texto", identical(compact(doc_pdf_text), compact(doc_word_text)))
doc_fonts <- pdftools::pdf_fonts(doc_pdf)
check("PDF de explicación Times New Roman", nrow(doc_fonts) > 0 && all(grepl("TimesNewRoman", doc_fonts$name)))
doc_size <- pdftools::pdf_pagesize(doc_pdf)
check("PDF de explicación tamaño carta", all(abs(doc_size$width - 612) < .01) && all(abs(doc_size$height - 792) < .01))
excluded_text <- c("Listo para entregar", "Entrega final completada", "Trabajo generado y validado",
  "Análisis exhaustivo de datos", "Resultados estratégicos clave", "Solución integral",
  "Elaborado con inteligencia artificial", "Generado automáticamente", "213 validaciones PASS")
check("Textos sin mensajes de ejecución o entrega", !any(vapply(excluded_text, function(phrase)
  grepl(tolower(phrase), tolower(paste(pdf_text, doc_pdf_text)), fixed = TRUE), logical(1))))
doc_boxes <- dplyr::bind_rows(pdftools::pdf_data(doc_pdf))
check("PDF de explicación respeta márgenes", all(doc_boxes$x >= 71) && all(doc_boxes$x + doc_boxes$width <= 541) &&
      all(doc_boxes$y >= 71) && all(doc_boxes$y + doc_boxes$height <= 721))
protected_paths <- c("data/raw/Base_Migracion_2009-2026jun(3).xlsx",
  "data/processed/series_mensuales.csv", "outputs/tables/kpis.csv",
  "infografia/infografia_final.png", "infografia/infografia_final.pdf",
  "docs/explicacion_diseno.docx", "docs/explicacion_diseno.pdf", "docs/decisiones_diseno.md", "validation/resultados.csv")
for (path in protected_paths) check(paste("Git ignora", path),
  system2("git", c("check-ignore", "--quiet", "--", shQuote(path)), stdout = FALSE, stderr = FALSE) == 0)
check("Sin familias completamente fallidas", all(vapply(series_names, function(nm)
  length(unique(metricas$familia[metricas$nombre_serie == nm & is.finite(metricas$RMSE)])) == 5, logical(1))))
results <- dplyr::bind_rows(checks)
write_csv_utf8(results, "validation/resultados.csv")
writeLines(capture.output(sessionInfo()), "validation/sessionInfo.txt")
if (any(results$resultado == "FAIL")) stop("Validaciones fallidas: ", paste(results$comprobacion[results$resultado == "FAIL"], collapse = "; "))
writeLines(paste(nrow(results), "validaciones PASS. Laboratorio generado y validado localmente."), "validation/estado.txt")
message(readLines("validation/estado.txt"))
