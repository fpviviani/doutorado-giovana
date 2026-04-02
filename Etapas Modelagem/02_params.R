# -----------------------------------------------------------------------------
# 2. PARÂMETROS DE MODELAGEM
# -----------------------------------------------------------------------------

lote_tamanho <- 3
pausa_minutos <- 1
max_tentativas <- 3
n_cores <- 4

# MODO SEGURO (controle do safe mode automático)
# Por padrão fica ATIVADO. Use safe_mode <- FALSE se quiser desabilitar.
# IMPORTANTE: quando ativo, permite que o script reduza n_cores automaticamente
# (3 e depois 1) após erros de memória. Aqui NÃO alteramos n_cores.
safe_mode <- TRUE
limiar_vif <- 5
n_replicacoes <- 6
test_percent <- 30

# BACKGROUND: Proporção 1:1 (Rausell-Moreno 2025)
background_min <- 5000
background_max <- 10000

# PREDITORES
# Se TRUE: usa bioclim + cobertura_arborea.tif (mesma grade/resolução)
# Se FALSE: usa somente o bioclim (comportamento antigo)
usar_cobertura_arborea <- FALSE

# NA da cobertura arbórea: estratégia para não punir ocorrências
# - "zero": substitui NA por 0 (% cobertura)
# - "median": substitui NA pela mediana observada na extração
na_cobertura_strategy <- "zero"  # "zero" | "median"

# MÉTODOS
metodos_modelagem <- c('maxent', 'rf', 'mars')

# EXECUÇÃO: LOOP (padrão) OU ESPÉCIE ÚNICA
# - "loop": roda todas as espécies encontradas na pasta (a partir de especie_partida)
# - "single": roda somente a espécie indicada em especie_unica
modo_execucao <- "loop"  # "loop" | "single" | "list"

# Usado somente se modo_execucao == "single".
# Exemplo: "Crypturellus_duidae" (deve bater com o nome do arquivo sem sufixos)
especie_unica <- NULL

# Usado somente se modo_execucao == "list".
# Exemplo: c("Crypturellus_duidae", "Capito_auratus")
especies_lista <- NULL

# ESPÉCIE DE PARTIDA (somente no modo "loop")
# Deixe vazio ("") para começar da primeira espécie encontrada na pasta de ocorrências
especie_partida <- ""
