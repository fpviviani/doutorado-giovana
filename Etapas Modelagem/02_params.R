# -----------------------------------------------------------------------------
# 2. PARÂMETROS DE MODELAGEM
# -----------------------------------------------------------------------------

lote_tamanho <- 5
pausa_minutos <- 1
max_tentativas <- 3
n_cores <- 4

# MODO SEGURO (controle do safe mode automático)
# Por padrão fica ATIVADO. Use safe_mode <- FALSE se quiser desabilitar.
# IMPORTANTE: quando ativo, permite que o script reduza n_cores automaticamente
# (3 e depois 1) após erros de memória. Aqui, o valor de n_cores não é alterado manualmente.
safe_mode <- TRUE
limiar_vif <- 10
n_vars_max <- 6  # máximo de variáveis após o filtro (seleção por menor correlação média)
n_replicacoes <- 10
test_percent <- 30

# BACKGROUND:
# Número de pontos de background é fixo em 10.000 (ver 03_background_adaptativo.R)

# PREDITORES
# O pipeline utiliza apenas o raster bioclimático (bio_brasil_30s.tif).

# MÉTODOS
metodos_modelagem <- c('maxent', 'rf', 'mars')

# EXECUÇÃO: LOOP (padrão) OU ESPÉCIE ÚNICA
# - "loop": roda todas as espécies encontradas na pasta (a partir de especie_partida)
# - "single": roda somente a espécie indicada em especie_unica
modo_execucao <- "single"  # "loop" | "single" | "list"

# Usado somente se modo_execucao == "single".
# Exemplo: "Crypturellus_duidae" (deve bater com o nome do arquivo sem sufixos)
especie_unica <- "Mitu_tuberosum"

# Usado somente se modo_execucao == "list".
# Exemplo: c("Crypturellus_duidae", "Capito_auratus")
especies_lista <- NULL

# ESPÉCIE DE PARTIDA (somente no modo "loop")
# Deixe vazio ("") para começar da primeira espécie encontrada na pasta de ocorrências
especie_partida <- ""

# VERIFICAÇÃO VISUAL DE PONTOS
# Se TRUE: antes de rodar sdmData, plota presença + background (amostra) no buffer
# e pede confirmação interativa. Use FALSE em execuções em loop (batch).
verificar_pontos <- FALSE
