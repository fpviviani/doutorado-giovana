# -----------------------------------------------------------------------------
# 2. PARÂMETROS DE MODELAGEM
# -----------------------------------------------------------------------------

lote_tamanho <- 3
pausa_minutos <- 15
max_tentativas <- 3
n_cores <- 6
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
