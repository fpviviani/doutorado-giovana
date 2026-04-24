# =============================================================================
# Script otimizado para download, limpeza e rarefação das espécies
# Objetivo: Baixar apenas coordenadas e nome científico, limpar e rarefazer 10 km
# Autor: Giovana D. Cabral
# Data: 2026-13-08
# =============================================================================

# 1. Carregar pacotes
library(rgbif)
library(dplyr)
library(CoordinateCleaner)
library(spThin)

# 2. Definir diretórios e parâmetros
dir_lista <- "C:/Users/giova/modelagem-15km-codigo/Input"
dir_saida <- "C:/Users/giova/modelagem-15km-codigo/Input/new_occ_10km"
arquivo_lista <- file.path(dir_lista, "lista_completa_spp.csv")

# Criar diretório de saída se não existir
if(!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

# 3. Ler lista de espécies
lista_spp <- read.csv(arquivo_lista, stringsAsFactors = FALSE)
if(!"species" %in% names(lista_spp)) {
  # Se não encontrar, usa a primeira coluna
  names(lista_spp)[1] <- "species"
}
especies <- lista_spp$species

# 4. Loop principal
for(i in seq_along(especies)) {
  
  especie_atual <- especies[i]
  cat("\n========================================\n")
  cat(sprintf("Processando [%d/%d]: %s\n", i, length(especies), especie_atual))
  cat("========================================\n")
  
  # ---- 4.1 Download ----
  ocorrencias_raw <- tryCatch({
    occ_search(
      scientificName = especie_atual,
      country = 'BR',
      year = '2005,2025',
      hasCoordinate = TRUE,
      limit = 50000,
      fields = c('scientificName', 'decimalLatitude', 'decimalLongitude')
    )$data
  }, error = function(e) {
    cat("ERRO no download:", e$message, "\n")
    return(NULL)
  })
  
  # Se não houver dados, pular para próxima
  if(is.null(ocorrencias_raw) || nrow(ocorrencias_raw) == 0) {
    cat("Nenhum registro encontrado para", especie_atual, "\n")
    next
  }
  
  cat("Registros baixados:", nrow(ocorrencias_raw), "\n")
  
  # Padronizar nome científico (usar o nome da lista)
  ocorrencias_raw$scientificName <- especie_atual
  
  # ---- 4.2 Limpeza com CoordinateCleaner ----
  ocorrencias_clean <- tryCatch({
    clean_coordinates(
      x = ocorrencias_raw,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      species = "scientificName",
      tests = c("capitals", "centroids", "equal", "gbif", "institutions", "zeros")
    ) %>%
      filter(.summary == TRUE) %>%
      select(-starts_with(".")) %>%
      distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) %>%
      as.data.frame()
  }, error = function(e) {
    cat("ERRO na limpeza:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(ocorrencias_clean) || nrow(ocorrencias_clean) == 0) {
    cat("Após limpeza, nenhum registro restante para", especie_atual, "\n")
    next
  }
  
  cat("Registros após limpeza:", nrow(ocorrencias_clean), "\n")
  
  # Se houver poucos registros (< 2), não faz thinning
  if(nrow(ocorrencias_clean) < 2) {
    cat("Pontos insuficientes para thinning (<2). Salvando dados limpos.\n")
    arquivo_saida <- file.path(dir_saida, paste0(gsub(" ", "_", especie_atual), "_cleaned.csv"))
    write.csv(ocorrencias_clean, arquivo_saida, row.names = FALSE)
    next
  }
  
  # ---- 4.3 Rarefação com spThin ----
  resultado_thin <- tryCatch({
    thin(
      ocorrencias_clean,
      lat.col = "decimalLatitude",
      long.col = "decimalLongitude",
      spec.col = "scientificName",
      thin.par = 10,
      reps = 50,
      locs.thinned.list.return = TRUE,
      write.files = FALSE,
      out.dir = tempdir(),
      verbose = FALSE  # deixar FALSE para não poluir o console
    )
  }, error = function(e) {
    cat("ERRO no thinning:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(resultado_thin)) {
    next
  }
  
  # Escolher a repetição com maior retenção de pontos
  n_pontos <- sapply(resultado_thin, nrow)
  melhor_idx <- which.max(n_pontos)
  ocorrencias_thin <- resultado_thin[[melhor_idx]]
  
  cat("Melhor repetição:", melhor_idx, "com", n_pontos[melhor_idx], "pontos\n")
  
  # ---- 4.4 Salvar arquivo final ----
  nome_arquivo <- paste0(gsub(" ", "_", especie_atual), "_thin_10km.csv")
  caminho_completo <- file.path(dir_saida, nome_arquivo)
  write.csv(ocorrencias_thin, caminho_completo, row.names = FALSE)
  cat("Salvo em:", caminho_completo, "\n")
  
  # ---- 4.5 Limpeza de memória (importante!) ----
  rm(ocorrencias_raw, ocorrencias_clean, resultado_thin, ocorrencias_thin)
  gc()  # força garbage collection
}

cat("\n========================================\n")
cat("Processamento concluído para todas as espécies!\n")
cat("Arquivos salvos em:", dir_saida, "\n")