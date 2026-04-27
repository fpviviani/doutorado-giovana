# Doutorado Gigi — Modelagem (estrutura do projeto)

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

### Onde colocar os dados (Input)
Os caminhos atuais são definidos em `Etapas Modelagem/01_config_dirs.R`.

O pipeline espera encontrar (estrutura atual):

- `Input/new_occ_10km/` → CSVs de ocorrências por espécie (ex.: `Mitu_tuberosum_thin_10km.csv`)
- `Input/new_buffers/` → shapefiles de buffer por espécie (`<especie>*.shp`)
- `Input/Clima/bio_brasil_30s.tif` → raster bioclim
- (opcional) `Input/Clima/cobertura_arborea_ambdata.tif` → preditor extra (cobertura arbórea), **na mesma grade** do bioclim

### Onde saem os resultados (Output)
As saídas são geradas em:

- `Output/Modelagem_10km/` (rasters `*_ensemble.tif` + relatórios)
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
- Número de cores (`n_cores`) e `safe_mode` (reduz automaticamente `n_cores` após erros de memória)
- Número de replicações e percentual de teste
- Limites de background (`background_min/max`)
- Métodos de modelagem (atual: `maxent`, `rf`, `mars`)
- **Modo de execução**:
  - `modo_execucao = "loop"` (todas as espécies)
  - `modo_execucao = "single"` (uma espécie: `especie_unica`)
  - `modo_execucao = "list"` (lista: `especies_lista`)
- `verificar_pontos` (se `TRUE`, pede confirmação visual por espécie antes de rodar o `sdmData` — útil para depuração)
- Preditores opcionais:
  - `usar_cobertura_arborea` (inclui `cobertura_arborea_ambdata.tif`)
  - `na_cobertura_strategy` (como tratar NA da cobertura: `zero` ou `median`)

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

- Carrega o raster `Input/Clima/bio_brasil_30s.tif`
- Se `usar_cobertura_arborea=TRUE`, adiciona `Input/Clima/cobertura_arborea_ambdata.tif` como camada extra
- Lista arquivos de ocorrência (`.csv`) e extrai o nome da espécie removendo sufixos comuns:
  - `_rarefeito.csv`, `_bruto.csv`, `_thin_<n>km.csv` etc.
- Detecta espécies já processadas (`*_ensemble.tif`)
- Define `especies_pendentes` conforme `modo_execucao` (`loop|single|list`) e remove as já processadas

#### `Etapas Modelagem/06_processar_especie.R`
Função principal de processamento por espécie.

- Define `processar_especie(especie_info, bioclimaticas, tentativa = 1)`
- Fluxo típico:
  - lê ocorrências e valida quantidade
  - calcula background adaptativo
  - carrega buffer da espécie
  - recorta e mascara preditores no buffer
  - extrai valores nas ocorrências (`terra::extract`)
    - se existir `cobertura_arborea`, aplica `na_cobertura_strategy` para não penalizar ocorrências (imputa NA com `0` ou `mediana`; se tudo NA, remove a variável)
  - seleção por VIF (`vifstep` / `exclude`)
  - grava raster temporário (`.tif`) e cria `raster::stack` (ponte para `sdm`)
  - monta `sdmData` com background
  - (opcional) `verificar_pontos=TRUE`: plota presença + amostra de background e pede confirmação
  - calibra modelos com `sdm()` (paralelo)
  - avalia (AUC/TSS)
  - gera ensemble em **chunks**, escrevendo direto em arquivo via `sdm::ensemble(..., filename=...)`
    - tenta `weighted` (AUC) e faz fallback para `mean`
  - salva raster final `*_ensemble.tif`
  - salva avaliação individual `*_avaliacao.csv`
  - retorna um `data.frame` com resumo/estatísticas da espécie
- Cria **log por espécie** em `relatorios/logs_especies/`
- Salva no CSV de avaliação: `n_background_usado` + métodos solicitados/rodados/faltando

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
- `Output/Checkpoints/Modelagem/progresso.rds` (checkpoint do progresso)

---

## Como retomar (checkpoint)

O script salva um checkpoint automaticamente em:

- `Output/Checkpoints/Modelagem/progresso.rds`

Se a execução for interrompida (queda de energia, travamento, etc.), basta rodar o `main_modelagem.R` novamente.

- Se o checkpoint existir, o pipeline carrega o `progresso.rds` e **pula automaticamente** as espécies que já estão com `status == "sucesso"` nesse arquivo, continuando de onde parou.

### Arquivo de erros (último erro por espécie)
Durante a execução, se uma espécie falhar, o script atualiza o arquivo:

- `relatorios/erros_por_especie.csv`

Esse CSV mantém **o último erro registrado por espécie** (tentativa, mensagem e timestamp).

### Recomeçar do zero
Se você quiser forçar uma execução do zero, apague o checkpoint:

- `Output/Checkpoints/Modelagem/progresso.rds`

## Modo seguro (menos uso de memória)

O controle do safe mode fica em `Etapas Modelagem/02_params.R` na variável `safe_mode`.

- `safe_mode <- TRUE`: permite redução automática de `n_cores` após erros de memória (reduz pela metade: 8→4→2→1; 6→3→1).
- `safe_mode <- FALSE`: desabilita a redução automática (segue com `n_cores` normal).


## Scripts auxiliares (ocorrências / mapas)

Os scripts abaixo foram adicionados/atualizados pela Gi para download, limpeza e rarefação de ocorrências (GBIF):

- `scripts/ocorrencias_limpas_ind.R` → processa **uma espécie** (download + limpeza + thinning)
- `scripts/ocorrencias_limpas_loop.R` → processa uma **lista de espécies** (`Input/lista_completa_spp.csv`)
- `scripts/mapa_interativo.R` → utilitário para visualização (mapa)

> Nota: esses scripts atualmente estão com caminhos Windows hardcoded (ex.: `C:/Users/giova/...`).
> Se você for rodar no Linux deste repo, é só ajustar `dir_lista/dir_saida`.

## Cobertura arbórea (preditor opcional)

O pipeline suporta adicionar cobertura arbórea via `usar_cobertura_arborea=TRUE` em `02_params.R`.
Ele espera encontrar:

- `Input/Clima/cobertura_arborea_ambdata.tif`

E assume que o raster já está **alinhado** ao bioclim (`bio_brasil_30s.tif`) em CRS/res/extent.
