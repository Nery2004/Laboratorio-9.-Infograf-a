options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 1200)
if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", ".UTF-8")
args <- commandArgs(FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
if (length(script)) setwd(dirname(normalizePath(script)))
dirs <- c("data/raw", "data/processed", "outputs/tables", "outputs/figures",
          "outputs/models", "outputs/diagnostics", "infografia", "validation", "docs")
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
writeLines("EN EJECUCION: aun no validado.", "validation/estado.txt")
tryCatch({
  source("R/00_setup.R", encoding = "UTF-8")
  source("R/01_analysis.R", encoding = "UTF-8")
  source("R/02_design.R", encoding = "UTF-8")
  source("R/03_document.R", encoding = "UTF-8")
  source("validation/validate.R", encoding = "UTF-8")
}, error = function(e) {
  writeLines(c("INCOMPLETO", conditionMessage(e)), "validation/estado.txt", useBytes = TRUE)
  stop(conditionMessage(e), call. = FALSE)
})
