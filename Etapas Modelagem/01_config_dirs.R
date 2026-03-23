# -----------------------------------------------------------------------------
# 1. CONFIGURAÇÕES INICIAIS
# -----------------------------------------------------------------------------

# DIRETÓRIOS
dir_base <- "C:/Users/giova/OneDrive/Documentos/Dados-doutorado"
dir_ocorrencias <- file.path(dir_base, "Ocorrencias_15km")
dir_buffers <- file.path(dir_base, "Buffers_15km")
dir_modelagem <- file.path(dir_base, "Modelagem_15km")
dir_variaveis <- file.path(dir_base, "Variaveis/climate")
dir_checkpoint <- file.path(dir_base, "Checkpoints")
dir_temp <- file.path(dir_base, "temp_raster")
dir_relatorios <- file.path(dir_modelagem, "relatorios")
dir_avaliacoes <- file.path(dir_relatorios, "avaliacoes_individuais")

# Criar pastas
for (pasta in c(dir_modelagem, dir_checkpoint, dir_temp, dir_relatorios, dir_avaliacoes)) {
  if (!dir.exists(pasta)) dir.create(pasta, recursive = TRUE)
}
