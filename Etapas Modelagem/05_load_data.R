# -----------------------------------------------------------------------------
# 5. CARREGAR DADOS
# -----------------------------------------------------------------------------

cat("📦 Carregando variáveis climáticas...\n")
bioclimaticas <- rast(file.path(dir_variaveis, "wc2.1_country/BRA_wc2.1_30s_bio.tif"))

# Listar espécies
arquivos_ocorrencias <- list.files(dir_ocorrencias, pattern = "\\.csv$", full.names = TRUE)
nomes_arquivos <- basename(arquivos_ocorrencias)

extrair_nome_especie <- function(nome_arquivo) {
  nome <- gsub("_rarefeito\\.csv$|_bruto\\.csv$|\\.csv$", "", nome_arquivo)
  return(nome)
}

especies_df <- data.frame(
  arquivo = arquivos_ocorrencias,
  nome_arquivo = nomes_arquivos,
  especie = sapply(nomes_arquivos, extrair_nome_especie),
  stringsAsFactors = FALSE
)

especies_df <- especies_df[order(especies_df$especie), ]

# Identificar espécies já processadas
arquivos_processados <- list.files(dir_modelagem, pattern = "_ensemble\\.tif$", full.names = FALSE)
especies_processadas <- gsub("_ensemble\\.tif$", "", arquivos_processados)

cat("\n📊 Total de espécies:", nrow(especies_df), "\n")
cat("📊 Já processadas:", length(especies_processadas), "\n")

# Encontrar posição da espécie de partida
posicao_partida <- which(especies_df$especie == especie_partida)
if (length(posicao_partida) == 0) posicao_partida <- 1

# Espécies pendentes
especies_pendentes <- especies_df[posicao_partida:nrow(especies_df), ]
especies_pendentes <- especies_pendentes[!especies_pendentes$especie %in% especies_processadas, ]

cat("🎯 Espécie de partida:", especie_partida, "\n")
cat("📊 Espécies pendentes:", nrow(especies_pendentes), "\n")
