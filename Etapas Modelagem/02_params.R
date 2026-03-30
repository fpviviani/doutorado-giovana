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
limiar_vif <- 10
n_replicacoes <- 6
test_percent <- 30

# BACKGROUND: Proporção 1:1 (Rausell-Moreno 2025)
background_min <- 5000
background_max <- 10000

# MÉTODOS
metodos_modelagem <- c('maxent', 'rf', 'mars')

# ESPÉCIE DE PARTIDA
# Deixe vazio ("") para começar da primeira espécie encontrada na pasta de ocorrências
especie_partida <- ""
