# Mestrado Gigi — Modelagem (estrutura do projeto)

Este diretório contém o script de modelagem R **compartimentado em etapas** para facilitar manutenção.

## Como rodar

## Requisitos (Windows)

- **R 4.3.3** (obrigatório: o `main_modelagem.R` valida a versão antes de rodar)
- Pacote **renv** (o script instala no user library se não existir)

Ao executar `main_modelagem.R`, o projeto vai tentar rodar `renv::restore()` automaticamente se existir `renv.lock`.


Execute o arquivo principal:

- `main_modelagem.R`

Ele chama, em sequência, cada arquivo dentro de `Etapas Modelagem/` via `source()`.

## Arquivos e o que cada um faz

### `main_modelagem.R`
Orquestrador do pipeline.

- Não contém a lógica “pesada” do modelo.
- Apenas executa (`source(...)`) as etapas na ordem correta.

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

#### `Etapas Modelagem/02_params.R`
Parâmetros gerais da modelagem.

- Tamanho de lote, pausas e número de tentativas
- Número de cores (paralelismo), limiar de VIF
- Número de replicações e percentual de teste
- Limites de background (min/max)
- Métodos de modelagem (`maxent`, `rf`, `gam`)
- Espécie de partida

#### `Etapas Modelagem/03_background_adaptativo.R`
Função de background adaptativo.

- Define `calcular_background(n_ocorrencias)`
- Calcula o número de pontos de background com base na quantidade de ocorrências (proporção 1:1) respeitando `background_min` e `background_max`

#### `Etapas Modelagem/04_cleanup.R`
Limpeza inicial antes de começar o processamento.

- Imprime cabeçalho “INICIANDO MODELAGEM”
- Limpa arquivos temporários (Windows Temp e `dir_temp` do projeto)
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
  - escreve raster temporário e cria `raster::stack`
  - monta `sdmData` com background
  - calibra modelos com `sdm()` (com paralelismo)
  - avalia (AUC/TSS)
  - gera ensemble (weighted por TSS; fallback para mean)
  - salva raster final `*_ensemble.tif`
  - salva avaliação individual `*_avaliacao.csv`
  - retorna um `data.frame` com resumo/estatísticas da espécie

#### `Etapas Modelagem/07_run_modelagem.R`
Loop principal (lotes + checkpoints + relatórios).

- Decide se há espécies pendentes
- Processa em lotes (`lote_tamanho`) com barra de progresso
- Para cada espécie:
  - tenta até `max_tentativas`
  - salva checkpoint (`progresso.rds`) após cada espécie
  - salva CSV do lote ao final
- Ao final:
  - salva `resultados_finais.csv`
  - imprime resumo (sucessos, falhas, médias de AUC/TSS/background/ocorrências)

## Saídas geradas (resumo)

Dependendo dos dados, o pipeline tende a produzir:

- `Modelagem_15km/<especie>_ensemble.tif` (mapa final por espécie)
- `relatorios/lote_<n>.csv` (resumo do lote)
- `relatorios/resultados_finais.csv` (resumo consolidado)
- `relatorios/avaliacoes_individuais/<especie>_avaliacao.csv` (avaliação por réplica/modelo)
- `Checkpoints/Modelagem/progresso.rds` (checkpoint do progresso)

---

## Como retomar (checkpoint)

O script salva um checkpoint automaticamente em:

- `Checkpoints/Modelagem/progresso.rds`

Se a execução for interrompida (queda de energia, travamento, etc.), basta rodar o `main_modelagem.R` novamente.

- Se o checkpoint existir, o pipeline carrega o `progresso.rds` e **pula automaticamente** as espécies que já estão com `status == "sucesso"` nesse arquivo, continuando de onde parou.

### Arquivo de erros (último erro por espécie)
Durante a execução, se uma espécie falhar, o script atualiza o arquivo:

- `relatorios/erros_por_especie.csv`

Esse CSV mantém **o último erro registrado por espécie** (tentativa, mensagem e timestamp), para facilitar depuração sem precisar procurar no console.

### Recomeçar do zero
Se você quiser forçar uma execução do zero, apague o checkpoint:

- `Checkpoints/Modelagem/progresso.rds`


## Modo seguro (menos uso de memória)

Se o script estiver travando/fechando por falta de memória, você pode ativar o **modo seguro**, que reduz o paralelismo (limita `n_cores` a no máximo 3).

No Windows (PowerShell), antes de rodar:

```powershell
$env:MODELAGEM_SAFE_MODE = "1"
Rscript .\main_modelagem.R
```

No CMD:

```bat
set MODELAGEM_SAFE_MODE=1
Rscript main_modelagem.R
```


### Safe mode automático

Além do modo seguro manual, o pipeline também tenta ativar um **safe mode automático**: se uma espécie falhar com mensagens típicas de falta de memória, ele reduz o `n_cores` para a **próxima** espécie (primeiro para 3; se repetir com 3, reduz para 1).

