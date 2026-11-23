# Testes gerais do app (carregamento e deploy)

test_that("ui e server carregam", {
  old <- setwd(raiz_app); on.exit(setwd(old))
  expect_s3_class(source("ui.R", encoding = "UTF-8")$value, "shiny.tag.list")
  expect_type(source("server.R", encoding = "UTF-8")$value, "closure")
})

test_that(".rscignore exclui testes", {
  expect_true("tests" %in% trimws(readLines(file.path(raiz_app, ".rscignore"))))
})

test_that("abas na mesma ordem e aninhamento de antes", {
  old <- setwd(raiz_app); on.exit(setwd(old))
  html <- as.character(source("ui.R", encoding = "UTF-8")$value)
  # Sequência de links de abas/menus no HTML gerado pelo ui.R original (antes dos módulos):
  # primeiro a barra de navegação, depois as sub-abas de "Mapas" e de "Compara Crimes".
  original <- c("Mapas", "Séries Temporais", "Compara Crimes", "Compara Municípios",
                "Relação entre crimes", "Representação no estado", "Download dos Dados",
                "Sobre", "Mapas Dinâmicos", "Cadeias de Markov", "Municípios", "Estado")
  links <- regmatches(html, gregexpr('data-toggle="(tab|dropdown)"[^>]*>[^<]*', html))[[1]]
  expect_identical(trimws(sub(".*>", "", links)), original)
})

test_that("server só chama módulos", {
  srv <- readLines(file.path(raiz_app, "server.R"), encoding = "UTF-8")
  expect_false(any(grepl("output$", srv, fixed = TRUE)))
  expect_equal(sum(grepl('Server("', srv, fixed = TRUE)), 7)
})

test_that("baseline não é feito de mensagens de erro", {
  v <- unlist(baseline)
  expect_false(any(startsWith(v[is.character(v)], "ERRO")))
})
