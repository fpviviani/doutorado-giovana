# -----------------------------------------------------------------------------
# 2. PARÂMETROS DE MODELAGEM
# -----------------------------------------------------------------------------

lote_tamanho <- 3
pausa_minutos <- 15
max_tentativas <- 3
n_cores <- 6

# MODO SEGURO (reduz paralelismo para evitar estouro de memória/travamentos)
# Por padrão fica DESATIVADO. Você pode ativar definindo safe_mode <- TRUE aqui
# ou forçar via variável de ambiente: MODELAGEM_SAFE_MODE=1
safe_mode <- FALSE
safe_mode_env <- tolower(Sys.getenv("MODELAGEM_SAFE_MODE", ""))
if (safe_mode_env %in% c("1", "true", "yes")) safe_mode <- TRUE
if (safe_mode) {
  n_cores <- max(1, min(3, n_cores))
}
limiar_vif <- 10
n_replicacoes <- 10
test_percent <- 30

# BACKGROUND: Proporção 1:1 (Rausell-Moreno 2025)
background_min <- 200
background_max <- 10000

# MÉTODOS
metodos_modelagem <- c('maxent', 'rf', 'gam')

# ESPÉCIE DE PARTIDA
especie_partida <- "Aburria_cujubi"
