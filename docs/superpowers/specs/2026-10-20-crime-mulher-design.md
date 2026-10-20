# VisualizaViolênciaMulherRS — design

Data: 2026-10-20
Pasta do novo app: `crime_mulher/`
App de referência: `crime/` (VisualizaCrimeRS)

## Intenção

Responder à pergunta: **qual o panorama da criminalidade contra a mulher no Rio Grande do Sul?**
O novo app é um irmão do VisualizaCrimeRS: mesmas abas, mesmos tipos de gráfico,
mesma lógica de agregados anuais por município e para o estado. A diferença está nos
dados: indicadores de violência contra a mulher publicados pela SSP-RS, com taxas
calculadas por **100 mil mulheres**.

## Objetivos (primeira versão)

- Agregados anuais por município e para o RS, de **2012 a 2025**.
- Os cinco indicadores da SSP-RS:
  - Feminicídio Consumado
  - Feminicídio Tentado
  - Ameaça
  - Estupro (a SSP inclui estupro de vulnerável)
  - Lesão Corporal
- Mais um indicador **"Total"**, a soma dos cinco, que replica a linha "Geral" da SSP.
  *(Sugestão minha, aceita pelo usuário.)*
- Taxa = `Qtd / população feminina * 100000`.
- Todas as abas do app `crime/`, com o mesmo comportamento:
  - Apresentação
  - Séries Temporais: Compara Crimes (abas Municípios e Estado) e Compara Municípios
    (Estado ou RMPA)
  - Relação entre crimes (dispersão)
  - Mapas: Mapas Dinâmicos (Leaflet, com Moran e grafo de vizinhança) e Cadeias de Markov
  - Representação no estado (treemap)
  - Download dos Dados
- Nome do app: **VisualizaViolênciaMulherRS**. O texto da aba Apresentação é adaptado do
  app de crime: aponta para a página da SSP de violência contra a mulher, cita a
  RIPSA e explica que as taxas são por 100 mil mulheres.

## Fora do escopo

- Dados de 2026, porque o ano está incompleto. A SSP já publica a planilha, mas ela não entra.
- Dados mensais (as planilhas os têm, mas esta versão usa só o total anual).
- Publicação no shinyapps.io.
- Versão em inglês.
- Mudanças no app `crime/`.

## Anti-objetivos

- Calcular taxas pela população total. O denominador é sempre a população feminina.
- Divergir dos totais da SSP. O total anual de cada município e de cada indicador tem de
  bater com a coluna "Total" da planilha. O total RS tem de bater com a aba "Geral".

## Fontes de dados

### Planilhas SSP-RS

Página: https://www.ssp.rs.gov.br/indicadores-da-violencia-contra-a-mulher (todas em .xlsx)

| Ano(s) | URL |
|---|---|
| 2012–2017 | https://www.ssp.rs.gov.br/upload/arquivos/202307/12160259-site-violencia-contra-as-mulheres-2012-a-2017-atualizado-em-09-janeiro-2018-publicacao.xlsx |
| 2018 | https://www.ssp.rs.gov.br/upload/arquivos/202307/12160426-site-violencia-contra-as-mulheres-2018-atualizado-em-07-janeiro-2019-publicacao.xlsx |
| 2019 | https://www.ssp.rs.gov.br/upload/arquivos/202307/12160432-site-violencia-contra-as-mulheres-2019-atualizado-em-21-fevereiro-2020-publicacao.xlsx |
| 2020 | https://www.ssp.rs.gov.br/upload/arquivos/202307/12163332-site-violencia-contra-as-mulheres-2020-atualizado-em-05-maio-2023-publicacao.xlsx |
| 2021 | https://admin.ssp.rs.gov.br/upload/arquivos/202312/04105912-site-violencia-contra-as-mulheres-2021-atualizado-em-04-dez-2023.xlsx |
| 2022 | https://www.ssp.rs.gov.br/upload/arquivos/202501/17161756-indicadores-de-violencia-contra-a-mulher-geral-e-por-municipio-2022-1.xlsx |
| 2023 | https://admin.ssp.rs.gov.br/upload/arquivos/202601/15143232-site-violencia-contra-as-mulheres-2023-atualizado-em-05-janeiro-2026-publicacao.xlsx |
| 2024 | https://admin.ssp.rs.gov.br/upload/arquivos/202601/15143237-site-violencia-contra-as-mulheres-2024-atualizado-em-05-janeiro-2026-publicacao.xlsx |
| 2025 | https://www.ssp.rs.gov.br/upload/arquivos/202610/08160049-site-violencia-contra-as-mulheres-2025-atualizado-em-06-de-outubro-de-2026-publicacao.xlsx |

Para 2023, usar a versão de janeiro de 2026, a mais recente. A página também tem uma
versão antiga (jun/2025) e um link "2022" duplicado sob o título 2021. Os dois devem ser ignorados.

Estrutura observada:

- **2018–2025**: uma aba por indicador ("Feminicídio Consumado", "Feminicídio Tentado",
  "Ameaça", "Estupro", "Lesão Corporal") e uma aba "Geral" com o total RS.
  - Linha 1: título.
  - Linha 2: cabeçalho ("Município", Jan..Dez, Total).
  - Municípios em CAIXA ALTA sem acento.
  - Em 2018 há uma linha vazia antes dos municípios e uma aba extra,
    "Fórmulas - Demais indicadores", que deve ser ignorada.
  - A ordem das abas varia entre os anos, então as abas devem ser lidas pelo nome.
  - O número de linhas varia um pouco entre as abas (504–507). Verificar se no fim de cada
    aba há uma linha de total ou notas.
- **2012–2017**: um único arquivo com a mesma lista de abas, mas a aba de feminicídio
  se chama "**Femicídio** Consumado/Tentado". Cada aba de indicador é uma tabela larga:
  municípios nas linhas e **uma coluna por ano** (2012–2017). Em seguida vêm as colunas
  "Total" (a soma dos seis anos, que **não** deve ser usada), "% vítimas/total" e algumas
  colunas de população. Só a aba "Geral" é empilhada por ano, com os meses nas colunas e
  apenas os totais do RS. Ela serve para conferir os dados.
  *(Verificado pelo revisor: em cada ano, a soma dos municípios bate com a linha Geral.)*
- **Feminicídio Tentado não existe para 2012**: a aba só tem as colunas de 2013 a 2017,
  e a linha Geral de 2012 mostra "-". **Decisão do usuário:** os registros de
  Feminicídio Tentado em 2012 ficam **fora da base**. Eles não entram nem como NA nem
  como 0, e o app continua começando em 2012. É o mesmo tratamento que o app de crime dá
  aos indicadores que começam mais tarde (`anos_com_dados` em `server.R`). Consequências
  aceitas:
  - A Cadeia de Markov dinâmica mostra "não possui dados" para Feminicídio Tentado.
  - O "Total" de 2012 soma os 4 indicadores disponíveis. Ele bate com o Geral da SSP de
    2012 (73.647).
  - A aba Apresentação ou uma nota no gráfico deve avisar que Feminicídio Tentado começa
    em 2013.
- **"NÃO INFORMADO" em 2018**: as abas Ameaça e Lesão Corporal de 2018 têm uma linha
  "NÃO INFORMADO" com 1 caso cada, e esse caso entra no total Geral.
  *Decisão minha:* essa linha é descartada, porque não tem município. Por isso, o total
  RS calculado pelo app fica 1 caso abaixo do total da SSP para esses dois indicadores
  (e para o "Total") em 2018. A conferência contra a aba Geral aceita essa diferença,
  que é documentada no script. Fora isso, de 2018 a 2025 cada aba tem exatamente 497
  municípios, e os meses, a coluna Total e a aba Geral batem entre si.
- O nome "Femicídio" é padronizado para "Feminicídio" em toda a série.

### População

`crime/estimativas_24_09_2026.csv` (RIPSA) tem as colunas Município, CodIBGE, Ano,
Homens, Mulheres, Total, Classe, Corede e Região Funcional. Usar `Classe == "Total"` e a
coluna **`Mulheres`** como `Populacao`. **Decisão do usuário:** o CSV é **copiado** para
`crime_mulher/estimativas_24_09_2026.csv` e o script de atualização lê essa cópia. O CSV
tem cerca de 17 MB e não deve ir para o pacote publicado (ver `.rscignore`).

## Formato da base

Mesmo formato longo da base do app de crime, para que `server.R` e `ui.R` possam ser
reaproveitados quase sem mudanças:

`Mun, CodIBGE, Ano, Crime, Qtd, Populacao`

- `Crime` é um fator com os 6 níveis: os 5 indicadores mais "Total".
- `Populacao` é a população feminina do município naquele ano.
- `Mun` usa a grafia com acentos já usada na base do app de crime.
- Um registro para cada combinação de município × ano × indicador, de 2012 a 2025.
  A única exceção é Feminicídio Tentado em 2012, que fica de fora (ver acima).

## Peças a construir (em `crime_mulher/`)

- `atualiza_base_crime_mulher.R`: segue o modelo de `crime/atualiza_base_crime.R`.
  1. Baixa os arquivos e os guarda em cache local, fora do git.
  2. Lê cada ano e pega a coluna "Total" de cada município.
  3. Faz a correspondência entre os nomes da SSP e o CodIBGE, com a mesma normalização
     (maiúsculas, sem acento) e as mesmas exceções (`DR MAURICIO CARDOSO`, `ENTRE IJUIS`,
     `FAZENDA VILA NOVA`), mais as novas que surgirem. Se algum município não tiver
     correspondência, o script deve parar com erro.
  4. Junta a população feminina e salva `base_crimemulher_2025.rds`.
  5. Confere os totais contra a aba "Geral" de cada ano.
- `global.R`, `ui.R`, `server.R`: cópias adaptadas do app de crime.
  - Título e textos.
  - Seleções padrão nos `selectInput` (hoje "Homicídio Doloso" e "Roubo"). Sugestão:
    "Feminicídio Consumado" e "Ameaça".
  - Rótulos que dizem "por 100 mil habitantes" passam a dizer "por 100 mil mulheres".
  - Rótulos de população passam a dizer "População feminina".
- **Tema rosa** *(pedido do usuário)*: o app de crime usa `shinytheme("united")`, cuja cor
  de destaque é laranja/vermelho. No novo app, essa cor vira **rosa** na barra de navegação,
  nas abas ativas, nos botões, nos links, nos sliders (ionRangeSlider) e no realce dos
  `selectInput`. Como o shinythemes não tem tema rosa, a troca é feita com sobrescrita
  CSS no arquivo de estilo do app.
  - *Decisão minha:* o tom base é `#D63384`, o rosa do Bootstrap, com variações mais
    escuras para hover e ativo.
  - *Decisão minha:* só a interface muda. As paletas dos gráficos e dos mapas continuam
    como estão (o gradiente cinza→amarelo→laranja→vermelho dos mapas de calor, as cores
    do plotly e do treemap).
- `MapaRS.rds`, `estilocrime.css` e `www/` copiados de `crime/`.
- Um `.Rproj` e um `.rscignore` próprios.

## Pontos de atenção para o builder

- **Eventos raros**: feminicídio consumado tem cerca de 80 casos por ano no estado. A
  maioria dos municípios tem zero, e as taxas dos municípios pequenos ficam instáveis.
  Isso é esperado e não muda o escopo. Os gráficos de dispersão, os mapas, o índice de
  Moran e as Cadeias de Markov (presença/ausência) devem continuar funcionando com muitos
  zeros, sem erro.
- **Municípios criados no período** (ex.: Pinto Bandeira, instalado em 2013): verificar
  como aparecem em 2012 e manter a mesma convenção da base do app de crime.
- **Valores fixos no código**: o app de crime tem alguns pontos amarrados aos próprios
  dados, como anos mínimo e máximo derivados da base (isso funciona), nomes de crimes
  usados como seleção padrão e textos. O builder precisa revisar todos.

## Deixado para o builder

- A organização interna do script de leitura, principalmente o parser dos blocos de 2012–2017.
- O texto exato da Apresentação, desde que siga o modelo do app de crime e os pontos acima.
- Verificar o app rodando localmente com `shiny::runApp("crime_mulher")`.
