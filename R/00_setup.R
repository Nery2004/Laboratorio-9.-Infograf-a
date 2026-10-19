required <- c("readxl", "dplyr", "tidyr", "ggplot2", "janitor", "lubridate",
              "stringr", "scales", "forecast", "tseries", "zoo", "patchwork",
              "ragg", "png", "pdftools", "officer", "digest", "prophet", "xml2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, dependencies = NA, Ncpus = 2)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Dependencias no disponibles: ", paste(missing, collapse = ", "))
set.seed(23218)
theme_lab <- function() ggplot2::theme_minimal(base_size = 10, base_family = "Arial") +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#17324D"),
                 panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom")
save_plot <- function(filename, plot, width = 10, height = 6)
  ggplot2::ggsave(file.path("outputs/figures", filename), plot, width = width, height = height, dpi = 220, bg = "white")
write_csv_utf8 <- function(x, path) utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
safe_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x), fixed = TRUE)))
clip01 <- function(x) pmax(0, pmin(1, x))
slug <- function(x) gsub("[^a-z0-9]+", "_", tolower(iconv(x, to = "ASCII//TRANSLIT")))
find_inputs <- function() list(excel = "data/raw/Base_Migracion_2009-2026jun(3).xlsx")
if (!file.exists(find_inputs()$excel)) stop("Coloque el Excel original en: ", normalizePath("data/raw"), "/Base_Migracion_2009-2026jun(3).xlsx")
