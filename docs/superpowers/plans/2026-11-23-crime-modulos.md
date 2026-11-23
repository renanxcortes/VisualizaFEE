# Modularização do app `crime/` — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Quebrar `crime/ui.R` e `crime/server.R` em 8 módulos Shiny (um por aba) sem mudar nada do que o app mostra.

**Architecture:** Pasta `crime/R/` com um arquivo por módulo (`xxxUI(id)` devolve o `tabPanel` inteiro; `xxxServer(id)` usa `moduleServer` com o corpo dos renders copiado sem alteração). Antes de mover qualquer código, gravamos um "baseline" dos outputs do server original; cada módulo é testado com `shiny::testServer` contra esse baseline.

**Tech Stack:** R 4.5.2, shiny 1.12.1, testthat 3.3.2 (já instalados). Rscript: `"/c/Program Files/R/R-4.5.2/bin/Rscript.exe"` (não está no PATH).

**Spec:** `docs/superpowers/specs/2026-11-23-crime-modulos-design.md`

## Global Constraints

- Não tocar em nada dentro de `crime_mulher/`.
- Não mudar lógica, rótulos, defaults, ids curtos de input/output, CSS ou tema. Corpo dos renders copiado literalmente.
- Nomes de input/output dentro do módulo continuam os mesmos de hoje (ex.: `ano_mapa`, `mapa_final`); só ganham prefixo via `ns()`.
- Ids de módulo: `"mapa"`, `"markov"`, `"compara_crimes"`, `"compara_municipios"`, `"dispersao"`, `"treemap"`, `"dados"`, `"sobre"`.
- Arquivos novos em UTF-8 (os atuais são UTF-8 com CRLF; manter CRLF é opcional).
- `global.R` não muda.
- **Sem commits** (preferência do usuário): o passo final de cada task é listar os arquivos alterados.
- Rodar testes a partir de `crime/`: `"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"` (ou `test_file(...)` para um arquivo).

## Review Focus

1. `conditionalPanel` sem `ns = ns` → painel nunca aparece (sem erro). Pinado pelos testes de UI que exigem `data-ns-prefix="<id>-"` em cada módulo que tem `conditionalPanel`.
2. `bsButton`/`bsPopover`/`bsTooltip` com id sem `ns()` → popover de ajuda (Moran, K-Means) e tooltip do gauge param de funcionar. Pinado em Tasks 4 e 6 (`"dispersao-q_kmeans"`, `"mapa-q_moran"`, `"mapa-gauge_moran"` no HTML).
3. Ordem/aninhamento das abas: "Mapas Dinâmicos" e "Cadeias de Markov" dentro de "Mapas"; as duas de séries dentro do `navbarMenu` "Séries Temporais". Pinado no teste final de `ui.R` (Task 8).
4. Download do CSV (`base_crime.csv`, `fileEncoding = "latin1"`). Pinado em Task 2 com `testServer` + `output$downloadData`.
5. Deploy no shinyapps.io sem a pasta de testes/baseline. Pinado em Task 1 (`tests` no `.rscignore`).

---

### Task 1: Infra de testes + baseline do server original

**Files:**
- Create: `crime/tests/testthat/helper-baseline.R`
- Create: `crime/tests/gera_baseline.R`
- Create: `crime/tests/testthat/fixtures/baseline.rds` (gerado)
- Create: `crime/tests/testthat/test_app.R`
- Modify: `crime/.rscignore` (acrescentar linha `tests`)

**Interfaces:**
- Produces (em `helper-baseline.R`, carregado automaticamente pelo testthat):
  - `cenarios`: lista nomeada; cada elemento `list(modulo = "<id do módulo>", inputs = list(...), outputs = c("<nomes curtos>"))`.
  - `normaliza(valor)`: se `valor` é string JSON de htmlwidget (começa com `{` e tem campo `x`), devolve `x` sem os campos `attrs`, `cur_data`, `visdat`, `base_url`, `elementId` (ids aleatórios); se é lista com `src` (renderPlot), devolve `"plot"`; senão devolve o valor como está.
  - `captura(server_fun, inputs, outputs)`: roda `shiny::testServer(server_fun, { do.call(session$setInputs, inputs); lapply(setNames(outputs, outputs), function(o) normaliza(tryCatch(output[[o]], error = function(e) conditionMessage(e)))) })` e devolve a lista.
  - `baseline`: `readRDS(test_path("fixtures", "baseline.rds"))` (lista nomeada pelos nomes de `cenarios`).
  - Carrega o app: `shiny::loadSupport("../..")` (sobe `global.R` e, depois da Task 2, `R/`), com `setwd` temporário para `../..`.

Cenários (inputs com os defaults da UI; anos via `max(base_crime$Ano)` = 2025, `min` = 2002):

| nome | modulo | inputs (além dos defaults) | outputs |
|---|---|---|---|
| `mapa_ocorre` | mapa | `tipo_dado_mapa="ocorre_radio_mapa"`, `checkbox_moran=TRUE`, `tipo_estrutura_espacial="Municípios que fazem fronteira"` | `mapa_final`, `gauge_moran`, `mapinha_grafo` |
| `mapa_taxa` | mapa | `tipo_dado_mapa="taxa_radio_mapa"`, `tipo_estrutura_espacial="Municipios mais próximos"`, `n_vizinhos_moran=2` | idem |
| `mapa_pop` | mapa | `tipo_dado_mapa="pop_radio_mapa"`, `sens_mapa=1.9` | `mapa_final` |
| `markov_temporal` | markov | `crime_markov="Roubo de Veículos"`, `anos_markov=c(2002,2025)`, `tipo_analise_markov="radio_matriz_markov"`, `checkbox_inclui_teste_temporal=TRUE`, janelas = defaults da UI (`c(2002,2013)`, `c(2013,2025)`), `checkbox_inclui_evolucao_instantaneo_markoviana=TRUE` | `mapas_markov`, `tabela_markov_simples`, `odds_ratio_simples`, `tabela_bruta`, `estatistica_chi`, `teste_chi_homog_temporal`, `anos_janelas`, `evolucao_odds_temporal` |
| `markov_espacial` | markov | idem com `tipo_analise_markov="radio_matriz_markov_estratificada"` | `tabela_markov_estratificada`, `odds_ratio_estratificado`, `teste_chi_homog_spat`, `evolucao_odds_espaco_temporal` |
| `compara_crimes` | compara_crimes | `cidade_compara_crime="Porto Alegre"`, `crimes_compara_crimes=c("Homicídio Doloso","Roubo")`, `tipo_dado_compara_crime` = cada um dos 2 valores (2 cenários: `_ocorre`, `_taxa`) | `ts_compara_crime_cidades`, `ts_compara_crime_rs` |
| `compara_mun_rs` / `compara_mun_rmpa` | compara_municipios | `cidades_compara="Porto Alegre"`, `crime_compara="Roubo"`, `tipo_dado_compara_municipio="taxa_radio_compara_municipio"`, `checkbox_inclui_rs_rmpa_ts=TRUE`, `radio_estado_rmpa` = `"radio_rs"` / `"radio_rmpa"` | `ts_compara_cidades` |
| `dispersao` | dispersao | `ano_disp=2025`, `dispersao_mun_pop_control=c(0,1500000)`, `crimex="Homicídio Doloso"`, `crimey="Roubo"`, `tipo_dado_disp="ocorre_radio_disp"`, `checkbox_kmeans=FALSE` | `dispersao` |
| `treemap_ocorre` / `treemap_pop` | treemap | `ano_tree=2025`, `crime_tree="Homicídio Doloso"`, `tipo_dado_tree` = `"ocorre_radio_tree"` / `"pop_radio_tree"` | `tree_map` |
| `dados` | dados | — | `tabela` |

(K-means fica fora do baseline porque `kmeans` é aleatório; é coberto na verificação manual da Task 8.)

- [ ] **Step 1: Escrever `helper-baseline.R`** com `cenarios`, `normaliza`, `captura`, carga do app e leitura de `baseline` protegida por `if (file.exists(...))`.

- [ ] **Step 2: Escrever `tests/gera_baseline.R`**: faz `setwd` para `crime/`, `source("global.R")`, `srv <- source("server.R")$value`, embrulha como `function(input, output, session) srv(input, output)`, define `set.seed(1)` e roda `captura()` para cada cenário (todos contra o server inteiro, pois os nomes curtos ainda são globais), salvando a lista em `tests/testthat/fixtures/baseline.rds`.

- [ ] **Step 3: Gerar o baseline (antes de qualquer mudança em `ui.R`/`server.R`)**

Run: `git diff --quiet -- server.R ui.R && "/c/Program Files/R/R-4.5.2/bin/Rscript.exe" tests/gera_baseline.R`
Expected: cria `fixtures/baseline.rds`; rodar duas vezes gera objetos `identical()` (confirma que `normaliza` removeu tudo que é aleatório). Se não for idêntico, ampliar a lista de campos removidos em `normaliza` e repetir.

- [ ] **Step 4: Escrever `test_app.R`** (smoke test que vale até o fim):

```r
test_that("ui e server carregam", {
  expect_s3_class(source("../../ui.R", encoding = "UTF-8")$value, "shiny.tag.list")
  expect_type(source("../../server.R", encoding = "UTF-8")$value, "closure")
})
test_that(".rscignore exclui testes", {
  expect_true("tests" %in% trimws(readLines("../../.rscignore")))
})
```

- [ ] **Step 5: Adicionar `tests` ao `.rscignore` e rodar a suíte**

Run: `"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"`
Expected: PASS (2 testes).

- [ ] **Step 6: Listar arquivos alterados** (`git status --short -- .`), sem commit.

---

### Task 2: Módulos `dados` e `sobre` (o padrão que os outros seguem)

**Files:**
- Create: `crime/R/mod_dados.R`, `crime/R/mod_sobre.R`
- Create: `crime/tests/testthat/test_mod_dados_sobre.R`
- Modify: `crime/ui.R:325-387` (substituir as duas abas por `dadosUI("dados")`, `sobreUI("sobre")`)
- Modify: `crime/server.R:442-459` (remover `output$tabela`, `datasetInput`, `output$downloadData`; adicionar `dadosServer("dados")`)

**Interfaces:**
- Produces: `dadosUI(id) -> tabPanel`, `dadosServer(id) -> NULL`, `sobreUI(id) -> tabPanel` (sem server).
- Padrão de todos os módulos: `ns <- NS(id)` na UI; server = `moduleServer(id, function(input, output, session) { <corpo copiado> })`. Comentário curto em português no topo de cada arquivo dizendo qual aba o módulo representa e quais outputs tem.
- `server.R` passa a ter a assinatura `shinyServer(function(input, output, session) { ... })` (necessário para chamar módulos).

- [ ] **Step 1: Escrever o teste**

```r
test_that("dados: tabela igual ao baseline e download em latin1", {
  testServer(dadosServer, {
    expect_identical(normaliza(output$tabela), baseline$dados$tabela)
    f <- output$downloadData
    expect_identical(basename(f), "base_crime.csv")
    expect_equal(nrow(read.csv(f, fileEncoding = "latin1")), nrow(base_crime))
  })
})
test_that("UIs com ids namespaced", {
  expect_match(as.character(dadosUI("dados")), 'id="dados-tabela"', fixed = TRUE)
  expect_match(as.character(dadosUI("dados")), 'id="dados-downloadData"', fixed = TRUE)
  expect_match(as.character(sobreUI("sobre")), "Sobre o VisualizaCrimeRS", fixed = TRUE)
})
```

(Se `basename(f)` não devolver o nome do `filename`, trocar por checar só o conteúdo — `testServer` pode gravar em arquivo temporário.)

- [ ] **Step 2: Rodar e ver falhar** — `test_file('tests/testthat/test_mod_dados_sobre.R')`; Expected: FAIL `could not find function "dadosServer"`.
- [ ] **Step 3: Criar os dois módulos e ligar em `ui.R`/`server.R`.**
- [ ] **Step 4: Rodar a suíte inteira** — Expected: PASS.
- [ ] **Step 5: Listar arquivos alterados.**

---

### Task 3: Módulos de séries temporais

**Files:**
- Create: `crime/R/mod_compara_crimes.R`, `crime/R/mod_compara_municipios.R`
- Create: `crime/tests/testthat/test_mod_series.R`
- Modify: `crime/ui.R:176-239` (o `navbarMenu("Séries Temporais", ...)` fica no `ui.R` e passa a conter `comparaCrimesUI("compara_crimes")`, `comparaMunicipiosUI("compara_municipios")`)
- Modify: `crime/server.R:8-146` (remover os 3 renders; chamar os 2 servers)

**Interfaces:**
- Produces: `comparaCrimesUI(id)`, `comparaCrimesServer(id)`, `comparaMunicipiosUI(id)`, `comparaMunicipiosServer(id)`.
- Consumes: `cenarios`, `captura`, `baseline`, `normaliza` (Task 1).

- [ ] **Step 1: Escrever o teste** — um `test_that` por cenário dos módulos `compara_crimes` e `compara_municipios`:

```r
for (nome in names(Filter(function(c) c$modulo %in% c("compara_crimes", "compara_municipios"), cenarios))) {
  test_that(paste("baseline:", nome), {
    c <- cenarios[[nome]]
    srv <- switch(c$modulo, compara_crimes = comparaCrimesServer, compara_municipios = comparaMunicipiosServer)
    expect_identical(captura(srv, c$inputs, c$outputs), baseline[[nome]])
  })
}
test_that("conditionalPanel da série extra usa namespace", {
  expect_match(as.character(comparaMunicipiosUI("compara_municipios")), 'data-ns-prefix="compara_municipios-"', fixed = TRUE)
})
```

(Esse laço é o mesmo nas Tasks 4–7; vale extraí-lo para `helper-baseline.R` como `testa_baseline(modulo, server_fun)` aqui na Task 3 e reusar.)

- [ ] **Step 2: Rodar e ver falhar.**
- [ ] **Step 3: Criar os módulos e ligar.**
- [ ] **Step 4: Rodar a suíte inteira** — Expected: PASS.
- [ ] **Step 5: Listar arquivos alterados.**

---

### Task 4: Módulo `dispersao`

**Files:**
- Create: `crime/R/mod_dispersao.R`, `crime/tests/testthat/test_mod_dispersao.R`
- Modify: `crime/ui.R:241-295`, `crime/server.R:148-254`

**Interfaces:** Produces `dispersaoUI(id)`, `dispersaoServer(id)`. Consumes `testa_baseline` (Task 3).

- [ ] **Step 1: Escrever o teste** — `testa_baseline("dispersao", dispersaoServer)`, mais:

```r
ui <- as.character(dispersaoUI("dispersao"))
expect_match(ui, 'data-ns-prefix="dispersao-"', fixed = TRUE)
expect_match(ui, "dispersao-q_kmeans", fixed = TRUE)   # bsButton + bsPopover
```

E um teste de k-means sem baseline: com `checkbox_kmeans=TRUE`, `n_grupos_kmeans=3`, `output$dispersao` não dá erro.

- [ ] **Step 2: Rodar e ver falhar.**
- [ ] **Step 3: Criar o módulo e ligar.** Atenção: `bsButton(ns("q_kmeans"))` e `bsPopover(id = ns("q_kmeans"))`.
- [ ] **Step 4: Rodar a suíte inteira** — Expected: PASS.
- [ ] **Step 5: Listar arquivos alterados.**

---

### Task 5: Módulo `treemap`

**Files:**
- Create: `crime/R/mod_treemap.R`, `crime/tests/testthat/test_mod_treemap.R`
- Modify: `crime/ui.R:297-323`, `crime/server.R:403-440`

**Interfaces:** Produces `treemapUI(id)`, `treemapServer(id)`.

- [ ] **Step 1: Escrever o teste** — `testa_baseline("treemap", treemapServer)` + `data-ns-prefix="treemap-"` na UI.
- [ ] **Step 2: Rodar e ver falhar.**
- [ ] **Step 3: Criar o módulo e ligar.**
- [ ] **Step 4: Rodar a suíte inteira** — Expected: PASS.
- [ ] **Step 5: Listar arquivos alterados.**

---

### Task 6: Módulo `mapa` (Mapas Dinâmicos + Moran)

**Files:**
- Create: `crime/R/mod_mapa.R`, `crime/tests/testthat/test_mod_mapa.R`
- Modify: `crime/ui.R:27-90` (o `tabPanel("Mapas", mainPanel(tabsetPanel(...)))` fica no `ui.R` e passa a conter `mapaUI("mapa")`), `crime/server.R:256-401`

**Interfaces:** Produces `mapaUI(id)`, `mapaServer(id)`.

- [ ] **Step 1: Escrever o teste** — `testa_baseline("mapa", mapaServer)`, mais:

```r
ui <- as.character(mapaUI("mapa"))
expect_match(ui, 'data-ns-prefix="mapa-"', fixed = TRUE)
for (id in c("mapa-q_moran", "mapa-gauge_moran", "mapa-mapa_final", "mapa-ano_mapa"))
  expect_match(ui, id, fixed = TRUE)
```

- [ ] **Step 2: Rodar e ver falhar.**
- [ ] **Step 3: Criar o módulo e ligar.** Os `conditionalPanel` aninhados (`tipo_estrutura_espacial` dentro de `checkbox_moran`) recebem `ns = ns` cada um; `bsTooltip("gauge_moran")` vira `bsTooltip(ns("gauge_moran"))`.
- [ ] **Step 4: Rodar a suíte inteira** — Expected: PASS.
- [ ] **Step 5: Listar arquivos alterados.**

---

### Task 7: Módulo `markov`

**Files:**
- Create: `crime/R/mod_markov.R`, `crime/tests/testthat/test_mod_markov.R`
- Modify: `crime/ui.R:92-170`, `crime/server.R:461-1200`

**Interfaces:** Produces `markovUI(id)`, `markovServer(id)`. As funções `calcula_odds_dinamico_temporal`, `calcula_odds_dinamico_espaco_temporal` e as cópias locais de `verifica_vizinhos` ficam dentro do `moduleServer`, exatamente onde estão hoje em relação aos renders (sem deduplicar — fora de escopo).

- [ ] **Step 1: Escrever o teste** — `testa_baseline("markov", markovServer)` + `data-ns-prefix="markov-"` e `id="markov-mapas_markov"` na UI.
- [ ] **Step 2: Rodar e ver falhar.**
- [ ] **Step 3: Criar o módulo e ligar.** Depois disto `server.R` deve conter só as 7 chamadas `xxxServer("<id>")` (`sobre` não tem server).
- [ ] **Step 4: Rodar a suíte inteira** — Expected: PASS.
- [ ] **Step 5: Listar arquivos alterados.**

---

### Task 8: `ui.R`/`server.R` enxutos + verificação no navegador

**Files:**
- Modify: `crime/ui.R`, `crime/server.R` (só limpeza: comentários, indentação)
- Modify: `crime/tests/testthat/test_app.R`

- [ ] **Step 1: Acrescentar testes de estrutura em `test_app.R`**

```r
test_that("abas na mesma ordem e aninhamento de antes", {
  html <- as.character(source("../../ui.R", encoding = "UTF-8")$value)
  titulos <- c("Mapas", "Mapas Dinâmicos", "Cadeias de Markov", "Séries Temporais",
               "Compara Crimes", "Compara Municípios", "Relação entre crimes",
               "Representação no estado", "Download dos Dados", "Sobre")
  pos <- vapply(titulos, function(t) regexpr(t, html, fixed = TRUE)[1], 1)
  expect_true(all(pos > 0)); expect_false(is.unsorted(pos))
})
test_that("server só chama módulos", {
  srv <- readLines("../../server.R", encoding = "UTF-8")
  expect_false(any(grepl("output\\$", srv)))
  expect_equal(sum(grepl("Server\\(\"", srv)), 7)
})
```

- [ ] **Step 2: Rodar a suíte inteira** — Expected: PASS.
- [ ] **Step 3: Verificação manual** — rodar `"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" -e "shiny::runApp('.', port = 8765, launch.browser = FALSE)"` em background e abrir `http://127.0.0.1:8765`. Conferir em cada aba: valores padrão iguais aos de antes; painéis condicionais aparecem/somem (Moran + vizinhos, sensibilidade com População, opções do Markov, série RS/RMPA, nº de grupos do k-means); popovers "Ajuda" abrem; tooltip do gauge; animação dos sliders de ano; k-means desenha grupos; download baixa `base_crime.csv`. Console do navegador sem erros JS.
- [ ] **Step 4: Listar todos os arquivos alterados/criados** para revisão do usuário, sem commit.
