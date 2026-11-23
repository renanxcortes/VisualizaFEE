# O HTML da UI modular deve ser igual ao da UI original (fixtures/ui_original.R),
# a menos dos prefixos de namespace e dos ids aleatórios das abas.
test_that("UI modular gera o mesmo HTML que a original", {
  old <- setwd(raiz_app); on.exit(setwd(old))
  orig <- as.character(source(file.path("tests", "testthat", "fixtures", "ui_original.R"), encoding = "UTF-8")$value)
  nova <- as.character(source("ui.R", encoding = "UTF-8")$value)
  ids <- "mapa|markov|compara_crimes|compara_municipios|dispersao|treemap|dados"
  norm <- function(x) {
    x <- gsub(' data-ns-prefix="[^"]*"', "", x)
    x <- gsub(paste0("([\"'#])(", ids, ")-"), "\\1", x, perl = TRUE)
    gsub("(tabsetid=\"|tab-)[0-9]+", "\\1N", x, perl = TRUE)
  }
  expect_identical(strsplit(norm(nova), "\n")[[1]], strsplit(norm(orig), "\n")[[1]])
})

test_that("todo id de input/output da UI tem prefixo de módulo", {
  old <- setwd(raiz_app); on.exit(setwd(old))
  html <- as.character(source("ui.R", encoding = "UTF-8")$value)
  ids <- regmatches(html, gregexpr(' id="[^"]+"', html))[[1]]
  ids <- sub(' id="([^"]+)"', "\\1", ids)
  ids <- ids[!grepl("^tab-[0-9]+-[0-9]+$", ids)]  # ids das abas, gerados pelo Shiny
  modulos <- "mapa|markov|compara_crimes|compara_municipios|dispersao|treemap|dados"
  sem_prefixo <- ids[!grepl(paste0("^(", modulos, ")-"), ids)]
  expect_identical(sem_prefixo, character(0))
})
