# Mestrado Gigi — Modelagem (estrutura do projeto)

Este diretório contém o script de modelagem R **compartimentado em etapas** para facilitar manutenção.

## Requisitos (Windows)

- **R 4.3.3** (obrigatório: o `main_modelagem.R` valida a versão antes de rodar)
- Pacote **renv** (o script tenta instalar no *user library* se não existir)

Ao executar `main_modelagem.R`, o projeto tenta rodar `renv::restore()` automaticamente se existir `renv.lock`.

## Como rodar

Execute o arquivo principal:

- `main_modelagem.R`

Ele chama, em sequência, cada arquivo dentro de `Etapas Modelagem/` via `source()`.


## Estrutura de pastas (Input/Output)

Este projeto separa dados e resultados assim:

- `Input/` → **dados de entrada** (não versionados)
- `Output/` → **saídas geradas** (não versionadas)

Ambas as pastas possuem `.gitkeep` e um `.gitignore` que ignora todo o conteúdo.

### Onde colocar os dados (Input)
O script espera encontrar:

- `Input/Ocorrencias_15km/` (CSV de ocorrências por espécie)
- `Input/Buffers_15km/` (shapefiles de buffer por espécie)
- `Input/Variaveis/climate/wc2.1_country/BRA_wc2.1_30s_bio.tif` (raster climático)

### Onde saem os resultados (Output)
As saídas são geradas em:

- `Output/Modelagem_15km/` (rasters `*_ensemble.tif` + relatórios)
- `Output/Checkpoints/Modelagem/` (checkpoint `progresso.rds`)
- `Output/temp_raster/` (temporários)

## Arquivos e o que cada um faz

### `main_modelagem.R`
Orquestrador do pipeline.

- Valida versão do R (4.3.3)
- Configura CRAN
- Garante `renv` e roda `renv::restore()` (se existir `renv.lock`)
- Executa (`source(...)`) as etapas na ordem correta

### Pasta `Etapas Modelagem/`
Contém os módulos separados por etapa.

#### `Etapas Modelagem/00_setup.R`
Configuração inicial do ambiente.

- `Sys.unsetenv("JAVA_HOME")`
- Carrega pacotes (`terra`, `sf`, `tidyverse`, `sdm`, `usdm`, `progress`, `parallel`)
- Configura opções do `terra` e do `sf` (`terraOptions`, `sf_use_s2(FALSE)`)

#### `Etapas Modelagem/01_config_dirs.R`
Define e prepara os diretórios de trabalho.

- Define caminhos base e subpastas (ocorrências, buffers, modelagem, variáveis, checkpoints, temporários, relatórios)
- Cria as pastas necessárias caso não existam
- **Checkpoint da modelagem** fica em `Checkpoints/Modelagem/`

#### `Etapas Modelagem/02_params.R`
Parâmetros gerais da modelagem.

- Tamanho de lote, pausas e número de tentativas
- Número de cores (paralelismo), limiar de VIF
- Número de replicações e percentual de teste
- Limites de background (min/max)
- Métodos de modelagem (`maxent`, `rf`, `gam`)
- Espécie de partida
- Se `especie_partida <- ""`, o loop começa da **primeira espécie** encontrada na pasta de ocorrências.
- Flag `safe_mode` (habilita/desabilita redução automática de cores após erros de memória)

#### `Etapas Modelagem/03_background_adaptativo.R`
Função de background adaptativo.

- Define `calcular_background(n_ocorrencias)`
- Calcula o número de pontos de background com base na quantidade de ocorrências (proporção 1:1) respeitando `background_min` e `background_max`

#### `Etapas Modelagem/04_cleanup.R`
Limpeza inicial antes de começar o processamento.

- Imprime cabeçalho “INICIANDO MODELAGEM”
- Limpa **apenas** a pasta temporária do projeto (`dir_temp` / `temp_raster`)
- Chama `gc()`

#### `Etapas Modelagem/05_load_data.R`
Carregamento dos dados e preparação da lista de espécies.

- Carrega o raster de variáveis climáticas (`bioclimaticas`)
- Lista arquivos de ocorrência (`.csv`) e extrai nome da espécie a partir do nome do arquivo
- Detecta espécies já processadas (arquivos `*_ensemble.tif`)
- Calcula `especies_pendentes` a partir de `especie_partida` e removendo as já processadas

#### `Etapas Modelagem/06_processar_especie.R`
Função principal de processamento por espécie.

- Define `processar_especie(especie_info, bioclimaticas, tentativa = 1)`
- Fluxo típico:
  - lê ocorrências e valida quantidade
  - calcula background adaptativo
  - carrega buffer da espécie
  - recorta e mascara variáveis climáticas no buffer
  - extrai valores nas ocorrências
  - faz seleção por VIF (`vifstep` / `exclude`)
  - escreve raster temporário e cria `raster::stack` (ponte para `sdm`)
  - monta `sdmData` com background
  - calibra modelos com `sdm()` (com paralelismo)
  - avalia (AUC/TSS)
  - gera ensemble (weighted por TSS; fallback para mean)
  - salva raster final `*_ensemble.tif`
  - salva avaliação individual `*_avaliacao.csv`
  - retorna um `data.frame` com resumo/estatísticas da espécie
- Cria **log por espécie** em `relatorios/logs_especies/`
- Inclui `n_background_usado` e informações de métodos (solicitados/rodados/faltando) no CSV de avaliação individual

#### `Etapas Modelagem/07_run_modelagem.R`
Loop principal (lotes + checkpoints + relatórios).

- Processa em lotes (`lote_tamanho`) com barra de progresso
- Para cada espécie:
  - tenta até `max_tentativas`
  - salva checkpoint (`progresso.rds`) após cada espécie
  - salva CSV do lote ao final
- Ao final:
  - salva `resultados_finais.csv`
  - imprime resumo (sucessos, falhas, médias de AUC/TSS/background/ocorrências)
- Salva também `relatorios/parametros_execucao.csv` com snapshot de parâmetros
- Se `safe_mode == TRUE`, ativa **redução automática** de `n_cores` após falhas por memória (reduz pela metade até 1)

## Saídas geradas (resumo)

Dependendo dos dados, o pipeline tende a produzir:

- `Modelagem_15km/<especie>_ensemble.tif` (mapa final por espécie)
- `relatorios/lote_<n>.csv` (resumo do lote)
- `relatorios/resultados_finais.csv` (resumo consolidado)
- `relatorios/parametros_execucao.csv` (snapshot de parâmetros usados)
- `relatorios/erros_por_especie.csv` (último erro por espécie)
- `relatorios/logs_especies/<especie>_YYYYmmdd_HHMMSS.log` (log por espécie)
- `relatorios/avaliacoes_individuais/<especie>_avaliacao.csv` (avaliação por réplica/modelo)
- `Output/Output/Checkpoints/Modelagem/progresso.rds` (checkpoint do progresso)

---

## Como retomar (checkpoint)

O script salva um checkpoint automaticamente em:

- `Output/Output/Checkpoints/Modelagem/progresso.rds`

Se a execução for interrompida (queda de energia, travamento, etc.), basta rodar o `main_modelagem.R` novamente.

- Se o checkpoint existir, o pipeline carrega o `progresso.rds` e **pula automaticamente** as espécies que já estão com `status == "sucesso"` nesse arquivo, continuando de onde parou.

### Arquivo de erros (último erro por espécie)
Durante a execução, se uma espécie falhar, o script atualiza o arquivo:

- `relatorios/erros_por_especie.csv`

Esse CSV mantém **o último erro registrado por espécie** (tentativa, mensagem e timestamp).

### Recomeçar do zero
Se você quiser forçar uma execução do zero, apague o checkpoint:

- `Output/Output/Checkpoints/Modelagem/progresso.rds`

## Modo seguro (menos uso de memória)

O controle do safe mode fica em `Etapas Modelagem/02_params.R` na variável `safe_mode`.

- `safe_mode <- TRUE`: permite redução automática de `n_cores` após erros de memória (reduz pela metade: 8→4→2→1; 6→3→1).
- `safe_mode <- FALSE`: desabilita a redução automática (segue com `n_cores` normal).
