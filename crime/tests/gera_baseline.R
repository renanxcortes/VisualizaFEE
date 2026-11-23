# Grava o baseline dos outputs do server.R ORIGINAL (rodar antes da modularização).
# Uso, a partir de crime/:  Rscript tests/gera_baseline.R
setwd("tests/testthat")
source("helper-baseline.R", encoding = "UTF-8")

srv <- local({
  old <- setwd(raiz_app); on.exit(setwd(old))
  source("server.R", encoding = "UTF-8")$value
})
app_server <- function(input, output, session) srv(input, output)

set.seed(1)
res <- lapply(cenarios, function(c) captura(app_server, c$inputs, c$outputs))
saveRDS(res, arq_baseline)
cat("baseline gravado:", arq_baseline, "\n")
str(lapply(res, function(r) vapply(r, function(o) class(o)[1], "")))
