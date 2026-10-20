# VisualizaViolênciaMulherRS — Plano de Implementação

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Criar o app Shiny `crime_mulher/` (VisualizaViolênciaMulherRS), cópia adaptada de `crime/`, com os indicadores de violência contra a mulher da SSP-RS (2012–2025) e taxas por 100 mil mulheres.

**Architecture:** Um script de atualização (`atualiza_base_crime_mulher.R`) usa funções de leitura isoladas em `funcoes_atualiza_base.R`. Essas funções baixam as planilhas da SSP, agregam por ano e juntam a população feminina da RIPSA. O resultado é `base_crimemulher_2025.rds`, no mesmo formato longo da base do app de crime (`Mun, CodIBGE, Ano, Crime, Qtd, Populacao`). Por isso, `global.R`, `ui.R` e `server.R` são copiados de `crime/` e só recebem ajustes de textos, padrões e tema.

**Tech Stack:** R 4.5.2 (`C:/Program Files/R/R-4.5.2/bin/Rscript.exe`, que não está no PATH), shiny, shinythemes, plotly, leaflet, D3plusR, C3, spdep, tidyverse, readxl, stringi, testthat.

**Spec:** `docs/superpowers/specs/2026-10-20-crime-mulher-design.md` (leia antes de começar; a seção "Fontes de dados" descreve a estrutura exata das planilhas).

## Global Constraints

- Pasta do app: `crime_mulher/`. Não modificar nada em `crime/`.
- Nome do app: `VisualizaViolênciaMulherRS`.
- Anos: 2012 a 2025. 2026 fica de fora.
- Indicadores (valores de `Crime`, grafia exata): `Feminicídio Consumado`, `Feminicídio Tentado`, `Ameaça`, `Estupro`, `Lesão Corporal`, `Total`.
- "Femicídio" (grafia de 2012–2017) vira sempre "Feminicídio".
- `Populacao` = coluna `Mulheres` da RIPSA (`Classe == "Total"`). Taxa = `Qtd / Populacao * 100000`.
- Feminicídio Tentado em 2012 **não tem linhas** na base (nem NA, nem 0).
- Linhas "NÃO INFORMADO" (2018) são descartadas.
- Pinto Bandeira não aparece em 2012, a mesma convenção da base do app de crime (496 municípios em 2012, 497 a partir de 2013).
- População: copiar `crime/estimativas_24_09_2026.csv` para `crime_mulher/estimativas_24_09_2026.csv` e ler essa cópia. Seguindo a convenção do app de crime, adicionar `crime_mulher/estimativas_*.csv` ao `.gitignore` da raiz e ao `.rscignore`. A correspondência Mun↔CodIBGE vem de `../crime/base_crimevis_2025.rds` (`distinct(Mun, CodIBGE)`).
- Cache dos .xlsx: `C:/Users/renan/AppData/Local/Temp/ssp_mulher_samples` (fora do git).
- Tema: `shinytheme("united")` com sobrescrita CSS na cor rosa `#D63384`. As paletas de gráficos e mapas não mudam.
- Rótulos: "Taxa por 100.000" → "Taxa por 100 mil mulheres". "População" / "Pop.:" → "População feminina" / "Pop. feminina:".
- Seleções padrão: "Homicídio Doloso" → "Feminicídio Consumado". "Roubo" e "Roubo de Veículos" → "Ameaça".
- Comentários e textos em português, no estilo do código de `crime/`.

## Review Focus

1. **Muitos zeros no k-means**: ao criar grupos na aba "Relação entre crimes" com Feminicídio Consumado × Feminicídio Tentado e um filtro de população pequeno, há menos pontos distintos do que grupos e `kmeans` falha. Isso deve aparecer como mensagem de `validate` e não como gráfico em branco. Coberto na Task 3, Step 4.
2. **Feminicídio Tentado + 2012** em qualquer aba (mapa, treemap, dispersão, Markov com janela começando em 2012) deve mostrar a mensagem "não possui dados", sem quebrar a aba. Coberto na Task 3, Step 5 (verificação manual).
3. **Grafias novas de municípios** no arquivo 2012–2017, diferentes das de 2018+. O script tem de parar e listar os nomes sem correspondência, e não pode descartá-los em silêncio. Coberto no teste `associa_codibge para quando falta município` (Task 1).
4. **Total RS ≠ SSP**: a soma dos municípios por ano e indicador deve bater com a aba Geral, com exceção apenas da diferença documentada de 2018 (−1 em Ameaça, Lesão Corporal e Total). Coberto no teste `totais RS batem com a aba Geral` (Task 2).
5. **Encoding**: nomes com acento ("Lesão Corporal", "Ameaça", nomes de município) devem aparecer corretos nos menus e nos tooltips no Windows/shinyapps. Coberto no teste `níveis de Crime com grafia exata` (Task 2) e na verificação visual (Task 3).

---

### Task 1: Funções de leitura das planilhas da SSP

**Files:**
- Create: `crime_mulher/funcoes_atualiza_base.R`
- Test: `crime_mulher/tests/testthat/test_funcoes_atualiza_base.R`

**Interfaces:**
- Produces (todas em `funcoes_atualiza_base.R`, que só depende de tidyverse, readxl e stringi):
  - `urls_ssp_mulher`: vetor nomeado `c("2012_2017" = ..., "2018" = ..., ..., "2025" = ...)`, com as URLs da tabela do spec (2023 = versão de jan/2026).
  - `cache_dir_mulher`: `"C:/Users/renan/AppData/Local/Temp/ssp_mulher_samples"`.
  - `baixa_arquivo_ssp_mulher(chave: character) -> character` (caminho local; baixa só se não existir; `mode = "wb"`).
  - `normaliza(x: character) -> character`: mesma função de `crime/atualiza_base_crime.R`.
  - `indicadores`: `c("Feminicídio Consumado", "Feminicídio Tentado", "Ameaça", "Estupro", "Lesão Corporal")`.
  - `le_ano_ssp_mulher(arq: character, ano: integer) -> tibble(MunSSP chr, Ano int, Crime chr, Qtd dbl)`: arquivos de 2018–2025. Lê as 5 abas pelo nome, localiza a linha de cabeçalho procurando "Município" na coluna 1 (sem `skip` fixo), usa a coluna "Total" e descarta linhas vazias, notas, linhas de total e "NÃO INFORMADO".
  - `le_2012_2017_ssp_mulher(arq: character) -> tibble(MunSSP, Ano, Crime, Qtd)`: tabela larga com uma coluna por ano. As abas se chamam "Femicídio …" e o resultado sai como "Feminicídio …". As colunas de ano são as que têm nome numérico de 2012 a 2017. "Total", "% vítimas/total" e as de população são ignoradas. Feminicídio Tentado não tem coluna 2012, então não gera linhas para 2012.
  - `le_geral_ssp_mulher(chave: character) -> tibble(Ano int, Crime chr, Qtd dbl)`: totais RS da aba "Geral". A linha "Geral" vira `Crime == "Total"`. Para "2012_2017", os blocos empilhados são localizados pelo título "no Ano de AAAA". O "-" de Tentado 2012 não gera linha.
  - `associa_codibge(df: tibble com MunSSP, crosswalk_key: tibble(CodIBGE, key)) -> df com CodIBGE no lugar de MunSSP`: normaliza, aplica `name_exceptions` (as 3 do app de crime e as novas que surgirem) e chama `stop()` listando os nomes sem correspondência.

- [ ] **Step 1: Escrever os testes que falham**

Os testes usam os arquivos reais (via `baixa_arquivo_ssp_mulher`). Nos testes, o `crosswalk_key` é montado a partir de `../../../crime/base_crimevis_2025.rds`.

```r
source("../../funcoes_atualiza_base.R")

test_that("le_ano_ssp_mulher 2025 traz 497 municípios e os totais da SSP", {
  d <- le_ano_ssp_mulher(baixa_arquivo_ssp_mulher("2025"), 2025L)
  expect_setequal(unique(d$Crime), indicadores)
  expect_true(all(table(d$Crime) == 497))
  expect_equal(d$Qtd[d$MunSSP == "ALVORADA" & d$Crime == "Ameaça"], 769)
  expect_equal(d$Qtd[d$MunSSP == "ALEGRETE" & d$Crime == "Feminicídio Consumado"], 1)
  expect_equal(sum(d$Qtd[d$Crime == "Feminicídio Tentado"]), 263)
})

test_that("le_ano_ssp_mulher 2018 ignora a linha vazia e NÃO INFORMADO", {
  d <- le_ano_ssp_mulher(baixa_arquivo_ssp_mulher("2018"), 2018L)
  expect_false(any(grepl("INFORMADO", d$MunSSP)))
  expect_true(all(table(d$Crime) == 497))
})

test_that("le_2012_2017_ssp_mulher padroniza Feminicídio e omite Tentado 2012", {
  d <- le_2012_2017_ssp_mulher(baixa_arquivo_ssp_mulher("2012_2017"))
  expect_setequal(unique(d$Ano), 2012:2017)
  expect_false(any(grepl("Femicídio", d$Crime)))
  expect_equal(nrow(dplyr::filter(d, Crime == "Feminicídio Tentado", Ano == 2012)), 0)
})

test_that("le_geral_ssp_mulher traz os totais RS", {
  g25 <- le_geral_ssp_mulher("2025")
  expect_equal(g25$Qtd[g25$Crime == "Total"], 53411)
  expect_equal(g25$Qtd[g25$Crime == "Feminicídio Consumado"], 80)
  g12 <- le_geral_ssp_mulher("2012_2017")
  expect_equal(g12$Qtd[g12$Ano == 2012 & g12$Crime == "Total"], 73647)
})

test_that("associa_codibge para quando falta município", {
  df <- tibble::tibble(MunSSP = c("PORTO ALEGRE", "CIDADE INEXISTENTE"), Qtd = 1)
  expect_error(associa_codibge(df, crosswalk_key), "CIDADE INEXISTENTE")
})
```

- [ ] **Step 2: Rodar e confirmar que falham**

Run (em `crime_mulher/`): `"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" -e 'testthat::test_file("tests/testthat/test_funcoes_atualiza_base.R")'`
Expected: FAIL (o arquivo de funções não existe)

- [ ] **Step 3: Implementar as funções do bloco Interfaces em `crime_mulher/funcoes_atualiza_base.R`**

Siga o estilo de `crime/atualiza_base_crime.R`: cabeçalho de comentário, `read_excel(..., col_names = FALSE)` para localizar o cabeçalho e `map_dfr` sobre as abas. Se aparecerem municípios sem correspondência em algum ano, inclua-os em `name_exceptions` com um comentário.

- [ ] **Step 4: Rodar e confirmar que passam**

Mesmo comando do Step 2. Expected: todos os testes PASS.

- [ ] **Step 5: Commit**

```bash
git add crime_mulher/funcoes_atualiza_base.R crime_mulher/tests/testthat/test_funcoes_atualiza_base.R
git commit -m "crime_mulher: funções de leitura das planilhas da SSP"
```

---

### Task 2: Script de atualização e base `base_crimemulher_2025.rds`

**Files:**
- Create: `crime_mulher/atualiza_base_crime_mulher.R`
- Create (cópia, ignorada pelo git): `crime_mulher/estimativas_24_09_2026.csv`
- Modify: `.gitignore` (adicionar a linha `crime_mulher/estimativas_*.csv`)
- Create (gerado): `crime_mulher/base_crimemulher_2025.rds`
- Test: `crime_mulher/tests/testthat/test_base.R`

**Interfaces:**
- Consumes: tudo o que a Task 1 produz.
- Produces: `base_crimemulher_2025.rds`, um tibble `Mun chr, CodIBGE int/dbl (mesmo tipo da base de crime), Ano dbl, Crime factor (6 níveis), Qtd dbl, Populacao int`, ordenado por `Ano, Crime, Mun`. `Mun` usa a grafia com acentos da base de crime.

- [ ] **Step 1: Escrever os testes que falham** (`test_base.R`, lendo `../../base_crimemulher_2025.rds`)

```r
test_that("estrutura e níveis de Crime com grafia exata", {
  expect_named(b, c("Mun", "CodIBGE", "Ano", "Crime", "Qtd", "Populacao"))
  expect_setequal(levels(b$Crime), c(indicadores, "Total"))
  expect_setequal(unique(b$Ano), 2012:2025)
  expect_false(anyNA(b))
})

test_that("Tentado 2012 ausente e municípios por ano", {
  expect_equal(nrow(filter(b, Crime == "Feminicídio Tentado", Ano == 2012)), 0)
  n <- b %>% filter(Crime == "Ameaça") %>% count(Ano)
  expect_equal(n$n[n$Ano == 2012], 496); expect_true(all(n$n[n$Ano >= 2013] == 497))
})

test_that("Total é a soma dos indicadores disponíveis", {
  # por município e ano: Qtd(Total) == soma dos outros
})

test_that("Populacao é a população feminina da RIPSA", {
  # Porto Alegre (4314902) 2025: Populacao == Mulheres do CSV com Classe == "Total"
})

test_that("totais RS batem com a aba Geral", {
  # para cada chave de urls_ssp_mulher: soma por Ano/Crime == le_geral_ssp_mulher(chave)
  # única diferença aceita: 2018, Crime em c("Ameaça", "Lesão Corporal", "Total"), RS = Geral - 1
})
```

Os três últimos blocos recebem as asserções descritas nos comentários. Os valores esperados vêm do CSV e de `le_geral_ssp_mulher`, e não são fixados à mão.

- [ ] **Step 2: Rodar e confirmar que falham**

Run: `"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" -e 'testthat::test_file("tests/testthat/test_base.R")'` (em `crime_mulher/`)
Expected: FAIL (o .rds não existe)

- [ ] **Step 3: Implementar `atualiza_base_crime_mulher.R`**

O script faz `source("funcoes_atualiza_base.R")` e lê os anos (2012–2017 e 2018–2025). Em seguida:
1. Junta `CodIBGE` com `associa_codibge` e descarta Pinto Bandeira em 2012.
2. Adiciona o indicador "Total" (soma por município e ano).
3. Junta `Mun` e `Populacao` (Mulheres).
4. Para com `stop()` se faltar população.
5. Imprime o resumo (linhas, anos, indicadores, municípios), como o script do app de crime, e salva o .rds.

O cabeçalho deve documentar as fontes, a ausência de Tentado em 2012 e a diferença de −1 em 2018 causada por "NÃO INFORMADO".

- [ ] **Step 4: Gerar a base e rodar os testes**

Run: `"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" atualiza_base_crime_mulher.R` e depois o comando do Step 2.
Expected: o resumo impresso mostra "Anos: 2012-2025", "Tipos de crime: 6", "Municípios: 497", e os testes de `test_base.R` passam.

- [ ] **Step 5: Commit**

```bash
git add .gitignore crime_mulher/atualiza_base_crime_mulher.R crime_mulher/base_crimemulher_2025.rds crime_mulher/tests/testthat/test_base.R
git commit -m "crime_mulher: script de atualização e base 2012-2025"
```

---

### Task 3: App Shiny (cópia adaptada de `crime/`)

**Files:**
- Create (copiados de `crime/`): `crime_mulher/global.R`, `crime_mulher/ui.R`, `crime_mulher/server.R`, `crime_mulher/MapaRS.rds`, `crime_mulher/estilocrime.css`, `crime_mulher/www/` (bigorb.png, logoGovernoantigo.png)
- Create: `crime_mulher/crime_mulher.Rproj` (cópia de `crime/crime.Rproj`), `crime_mulher/.rscignore`
- Test: `crime_mulher/tests/testthat/test_app.R`

**Interfaces:**
- Consumes: `base_crimemulher_2025.rds` (Task 2). O nome do objeto em `global.R` continua `base_crime`, para não ter de mexer no `server.R` inteiro.

- [ ] **Step 1: Escrever os testes que falham** (`test_app.R`)

```r
ui  <- readLines("../../ui.R", encoding = "UTF-8")
srv <- readLines("../../server.R", encoding = "UTF-8")
glb <- readLines("../../global.R", encoding = "UTF-8")

test_that("sem resquícios do app de crime", {
  txt <- c(ui, srv, glb)
  for (s in c("VisualizaCrimeRS", "Homicídio Doloso", "\"Roubo", "Taxa por 100.000", "base_crimevis_2025.rds"))
    expect_false(any(grepl(s, txt, fixed = TRUE)), info = s)
})

test_that("textos e base novos", {
  expect_true(any(grepl("VisualizaViolênciaMulherRS", ui, fixed = TRUE)))
  expect_true(any(grepl("Taxa por 100 mil mulheres", c(ui, srv), fixed = TRUE)))
  expect_true(any(grepl("base_crimemulher_2025.rds", glb, fixed = TRUE)))
  expect_true(any(grepl("2013", ui)))  # aviso: Feminicídio Tentado começa em 2013
})

test_that("ui e server carregam", {
  old <- setwd("../.."); on.exit(setwd(old))
  source("global.R", encoding = "UTF-8")
  expect_s3_class(source("ui.R", encoding = "UTF-8")$value, "shiny.tag.list")
  expect_type(source("server.R", encoding = "UTF-8")$value, "closure")
})
```

- [ ] **Step 2: Copiar os arquivos e rodar os testes, que devem falhar**

Copie os arquivos listados acima. Run: `"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" -e 'testthat::test_file("tests/testthat/test_app.R")'`
Expected: FAIL em "sem resquícios" e "textos e base novos"

- [ ] **Step 3: Adaptar `global.R`, `ui.R` e `server.R`**

- `global.R`: ler `base_crimemulher_2025.rds` (o comentário passa a citar `atualiza_base_crime_mulher.R`).
- `ui.R`:
  - Navbar e textos da Apresentação: VisualizaViolênciaMulherRS, "violência contra a mulher". O link da SSP passa a ser `https://www.ssp.rs.gov.br/indicadores-da-violencia-contra-a-mulher`. Manter RIPSA, autoria, contato e licença.
  - Nota: as taxas são por 100 mil mulheres (população feminina estimada pela RIPSA). Feminicídio Tentado só tem dados a partir de 2013. Estupro inclui estupro de vulnerável.
  - Seleções padrão e rótulos conforme as Global Constraints. O `max` de `n_grupos_kmeans` passa a ser 497.
- `server.R`: rótulos dos eixos e tooltips conforme as Global Constraints. Na dispersão, adicionar um `shiny::validate(need(...))` antes do `kmeans` para o caso de haver menos pontos distintos do que `n_grupos`, com a mensagem "Há menos municípios distintos do que grupos; reduza o número de grupos ou amplie o filtro de população.".
- `.rscignore`: excluir `tests`, `atualiza_base_crime_mulher.R`, `funcoes_atualiza_base.R` e `estimativas_24_09_2026.csv`.

- [ ] **Step 4: Rodar os testes e confirmar que passam**

Mesmo comando do Step 2. Expected: PASS.

- [ ] **Step 5: Verificação no navegador**

Suba o app com `"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" -e 'shiny::runApp(".", port = 8765)'` (em background) e abra `http://127.0.0.1:8765`. Confira:
- Todas as abas renderizam com os padrões.
- Feminicídio Tentado + 2012 no mapa, no treemap e na dispersão mostra "não possui dados".
- Cadeias de Markov com Feminicídio Tentado: a evolução anual mostra a mensagem de período incompleto.
- k-means com Feminicídio Consumado × Feminicídio Tentado e 10 grupos mostra a mensagem do `validate`.
- Acentos corretos nos menus e nos tooltips.
- O console do R não mostra erros.

- [ ] **Step 6: Commit**

```bash
git add crime_mulher/
git commit -m "crime_mulher: app VisualizaViolênciaMulherRS"
```

---

### Task 4: Tema rosa

**Files:**
- Create: `crime_mulher/estilocrimemulher.css` (o conteúdo de `crime/estilocrime.css` mais as sobrescritas)
- Modify: `crime_mulher/ui.R` (`includeCSS("estilocrimemulher.css")`)
- Test: `crime_mulher/tests/testthat/test_app.R` (adicionar um bloco)

- [ ] **Step 1: Adicionar o teste que falha**

```r
test_that("tema rosa", {
  css <- readLines("../../estilocrimemulher.css")
  expect_true(any(grepl("#D63384", css, ignore.case = TRUE)))
  expect_true(any(grepl("estilocrimemulher.css", ui, fixed = TRUE)))
})
```

- [ ] **Step 2: Rodar e confirmar que falha.** Mesmo comando da Task 3. Expected: FAIL em "tema rosa".

- [ ] **Step 3: Escrever as sobrescritas**

O CSS sobrescreve a cor de destaque do tema "united" com `#D63384` (e um tom mais escuro, ex. `#B02A6B`, para hover e ativo) nestes elementos:
- `.navbar-default` (fundo e borda), itens ativos e hover do navbar e dos dropdowns
- `.nav-tabs`/`.nav-pills` ativos
- `a`
- `.btn-primary`/`.btn-default` em foco
- `.irs-bar`, `.irs-single`, `.irs-from`, `.irs-to`, `.irs-handle`
- realce do selectize (`.selectize-dropdown .active`, `.selectize-input.focus`)

Troque `includeCSS("estilocrime.css")` por `includeCSS("estilocrimemulher.css")` e apague `crime_mulher/estilocrime.css` (git rm).

- [ ] **Step 4: Rodar os testes e conferir no navegador**

Os testes passam. No navegador, nenhum elemento da interface continua laranja ou vermelho, e os mapas e gráficos mantêm as paletas originais.

- [ ] **Step 5: Commit**

```bash
git add crime_mulher/estilocrimemulher.css crime_mulher/ui.R crime_mulher/tests/testthat/test_app.R
git commit -m "crime_mulher: tema rosa"
```
