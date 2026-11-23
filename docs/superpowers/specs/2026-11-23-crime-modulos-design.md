# Modularização do app `crime/` em módulos Shiny — design

## Por quê
Demonstrar módulos Shiny para uma turma usando um app real. O código precisa ficar
fácil de ler e de comparar com a versão original (antes/depois).

## Objetivos
- Quebrar `crime/ui.R` (393 linhas) e `crime/server.R` (1210 linhas) em um módulo por
  aba / tipo de visualização, cada um com par `xxxUI(id)` / `xxxServer(id)`.
- `ui.R` e `server.R` passam a apenas montar o `navbarPage` e chamar os módulos.

## Não-objetivos
- Não tocar em `crime_mulher/` (decisão do usuário).
- Não refatorar a lógica interna (ex.: duplicação no Markov) — fica para uma etapa futura.
- Não criar comunicação entre módulos (nenhuma aba depende de input de outra).

## Anti-objetivos
- Qualquer mudança no que o app mostra: mesmos inputs, defaults, rótulos, gráficos e números.

## Abordagens consideradas
- **A. Um arquivo por aba, lógica intacta (escolhida).** Melhor material didático: o diff
  mostra só reorganização.
- B. A + limpar o Markov (helpers e reactives compartilhados) — mistura duas lições e
  aumenta risco de mudar resultados.
- C. A + módulos conversando via reactives — artificial neste app e muda comportamento.

## Design
Pasta `crime/R/` (carregada automaticamente pelo Shiny ≥ 1.5):

| Arquivo | Módulo (id) | Outputs que absorve |
|---|---|---|
| `mod_mapa.R` | `mapaUI/mapaServer` ("mapa") | `mapa_final`, `gauge_moran`, `mapinha_grafo` |
| `mod_markov.R` | `markovUI/markovServer` ("markov") | `mapas_markov`, tabelas/odds/testes, `evolucao_odds_*`, `anos_janelas` |
| `mod_compara_crimes.R` | `comparaCrimesUI/Server` ("compara_crimes") | `ts_compara_crime_cidades`, `ts_compara_crime_rs` |
| `mod_compara_municipios.R` | `comparaMunicipiosUI/Server` ("compara_municipios") | `ts_compara_cidades` |
| `mod_dispersao.R` | `dispersaoUI/Server` ("dispersao") | `dispersao` |
| `mod_treemap.R` | `treemapUI/Server` ("treemap") | `tree_map` |
| `mod_dados.R` | `dadosUI/Server` ("dados") | `tabela`, `downloadData` |
| `mod_sobre.R` | `sobreUI` ("sobre") | só UI, sem server |

- Cada `xxxUI(id)` devolve o `tabPanel(...)` completo (título incluso), para o `ui.R`
  ficar como um índice legível do app. "Mapas Dinâmicos" e "Cadeias de Markov" continuam
  sub-abas do `tabsetPanel` de "Mapas"; as duas de séries continuam no `navbarMenu`.
- Dentro da UI: todo id passa por `ns <- NS(id)`. Atenção aos pontos que quebram em
  silêncio se esquecidos (vale mostrar na aula):
  - `conditionalPanel(condition = "input.x == ...", ns = ns)` — a condição usa o nome
    curto, e o argumento `ns` faz o prefixo.
  - `bsButton`/`bsPopover`/`bsTooltip`: ids com `ns()`.
- Server: `moduleServer(id, function(input, output, session) { ... })` com o corpo dos
  renders copiado sem alteração. Helpers locais (`calcula_odds_dinamico_*`,
  `verifica_vizinhos`) vão para dentro do módulo de Markov como estão.
- `global.R` não muda (dados e pacotes continuam globais). CSS/tema/estilos ficam no `ui.R`.
- `.rscignore` não precisa mudar (não ignora `R/`).

## Verificação
- Rodar o app e conferir cada aba com os defaults: mesmos gráficos/tabelas de antes.
- Testar os `conditionalPanel` (Moran, população no mapa/treemap, opções do Markov,
  série extra RS/RMPA, k-means) e o download.
- Sem commits: listar arquivos alterados para revisão do usuário.

## Deixado para quem implementar
- Ordem e comentários didáticos curtos no topo de cada arquivo de módulo.
- Obs.: `Rscript` não está no PATH do bash; usar o R local do usuário para rodar.
