# -----------------------------------------------------------------------------
# 1. CONFIGURAÇÕES INICIAIS
# -----------------------------------------------------------------------------

# DIRETÓRIOS
# Base do projeto: pasta raiz do repositório
dir_base <- normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."), winslash = "/", mustWork = FALSE)
dir_input <- file.path(dir_base, "Input")
dir_output <- file.path(dir_base, "Output")

# INPUTS
dir_ocorrencias <- file.path(dir_input, "new_occ_10km")
dir_buffers <- file.path(dir_input, "new_buffers")
dir_variaveis <- file.path(dir_input, "Clima")

# OUTPUTS
dir_modelagem <- file.path(dir_output, "Modelagem_10km")
dir_checkpoint <- file.path(dir_output, "Checkpoints", "Modelagem")
dir_temp <- file.path(dir_output, "temp_raster")
dir_relatorios <- file.path(dir_modelagem, "relatorios")
dir_avaliacoes <- file.path(dir_relatorios, "avaliacoes_individuais")


# Criar pastas
for (pasta in c(dir_modelagem, dir_checkpoint, dir_temp, dir_relatorios, dir_avaliacoes)) {
  if (!dir.exists(pasta)) dir.create(pasta, recursive = TRUE)
}
