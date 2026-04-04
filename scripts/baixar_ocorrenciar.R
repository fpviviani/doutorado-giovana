# =============================================================================
# SCRIPT COMPLETO - Download e rarefação de múltiplas espécies
# Rarefação fixa: 10 km
# Saída: CSV com apenas latitude e longitude
# =============================================================================

# Pacotes
library(rgbif)
library(dplyr)
library(tidyr)
library(spThin)
library(CoordinateCleaner)
library(progress)

# -----------------------------------------------------------------------------
# 1. CONFIGURAÇÕES
# -----------------------------------------------------------------------------

dir_saida <- "C:/Users/giova/OneDrive/Documentos/Dados-doutorado/REAVALIANDO_OCORRENCIAS_10KM"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

# CSV SEM CABEÇALHO
especies <- read.csv(
  "C:/Users/giova/OneDrive/Documentos/Dados-doutorado/REAVALIANDO_OCORRENCIAS_10KM/lista_completa_spp.csv",
  stringsAsFactors = FALSE,
  header = FALSE
)

nomes_especies <- especies[[1]]
nomes_especies <- trimws(nomes_especies)
nomes_especies <- nomes_especies[nomes_especies != "" & !is.na(nomes_especies)]

cat("Total de espécies:", length(nomes_especies), "\n")

ano_inicio <- 2005
ano_fim <- 2025

# -----------------------------------------------------------------------------
# 2. FUNÇÃO PRINCIPAL
# -----------------------------------------------------------------------------

processar_especie <- function(nome_cientifico, dir_saida) {
  
  cat("\n============================\n")
  cat("Processando:", nome_cientifico, "\n")
  cat("============================\n")
  
  resultado <- data.frame(
    especie = nome_cientifico,
    status = "falha",
    n_bruto = 0,
    n_limpo = 0,
    n_final = 0,
    rarefacao = "nenhuma",
    observacao = "",
    stringsAsFactors = FALSE
  )
  
  tryCatch({
    
    # -------------------------------------------------------------------------
    # DOWNLOAD
    # -------------------------------------------------------------------------
    
    ocorrencias <- occ_search(
      scientificName = nome_cientifico,
      country = 'BR',
      year = paste0(ano_inicio, ',', ano_fim),
      hasCoordinate = TRUE,
      limit = 50000
    )$data %>%
      select(scientificName, decimalLatitude, decimalLongitude)
    
    if (is.null(ocorrencias) || nrow(ocorrencias) == 0) {
      resultado$observacao <- "Sem dados"
      return(resultado)
    }
    
    resultado$n_bruto <- nrow(ocorrencias)
    
    if (resultado$n_bruto == 50000) {
      resultado$observacao <- "Possível truncamento GBIF"
    }
    
    # -------------------------------------------------------------------------
    # LIMPEZA
    # -------------------------------------------------------------------------
    
    ocorrencias <- ocorrencias %>%
      drop_na(decimalLatitude, decimalLongitude)
    
    ocorrencias_clean <- clean_coordinates(
      x = ocorrencias,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      species = "scientificName",
      tests = c("capitals", "centroids", "equal", "gbif", "institutions", "zeros"),
      verbose = FALSE
    ) %>%
      filter(.summary == TRUE) %>%
      select(decimalLatitude, decimalLongitude, scientificName) %>%
      distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)
    
    resultado$n_limpo <- nrow(ocorrencias_clean)
    
    if (resultado$n_limpo < 3) {
      resultado$observacao <- "Poucos pontos"
      return(resultado)
    }
    
    # -------------------------------------------------------------------------
    # RAREFAÇÃO (APENAS 10 km)
    # -------------------------------------------------------------------------
    
    cat("Tentando rarefação: 10 km\n")
    
    resultado_thin <- tryCatch({
      thin(
        loc.data = ocorrencias_clean,
        lat.col = "decimalLatitude",
        long.col = "decimalLongitude",
        spec.col = "scientificName",
        thin.par = 10,
        reps = 50,
        write.files = FALSE,
        verbose = FALSE
      )
    }, error = function(e) NULL)
    
    dados_finais <- NULL
    
    if (!is.null(resultado_thin) && length(resultado_thin) > 0) {
      tamanhos <- sapply(resultado_thin, nrow)
      
      if (max(tamanhos) > 0) {
        dados_finais <- resultado_thin[[which.max(tamanhos)]]
      }
    }
    
    # -------------------------------------------------------------------------
    # DECISÃO FINAL
    # -------------------------------------------------------------------------
    
    if (!is.null(dados_finais) && nrow(dados_finais) > 0) {
      
      resultado$status <- "sucesso"
      resultado$n_final <- nrow(dados_finais)
      resultado$rarefacao <- "spThin_10km"
      
      coordenadas <- dados_finais %>%
        select(decimalLatitude, decimalLongitude)
      
    } else {
      
      warning(paste("⚠️ Rarefação 10 km não retornou pontos para:", nome_cientifico))
      
      resultado$status <- "sem_rarefacao"
      resultado$n_final <- resultado$n_limpo
      resultado$rarefacao <- "falhou_10km"
      resultado$observacao <- "Rarefação 10 km retornou 0 pontos"
      
      coordenadas <- ocorrencias_clean %>%
        select(decimalLatitude, decimalLongitude)
    }
    
    # -------------------------------------------------------------------------
    # SALVAR (APENAS LAT/LON)
    # -------------------------------------------------------------------------
    
    nome_simples <- gsub("[^a-zA-Z0-9]", "_", nome_cientifico)
    nome_arquivo <- file.path(dir_saida, paste0(nome_simples, ".csv"))
    
    write.csv(coordenadas, nome_arquivo, row.names = FALSE)
    
    return(resultado)
    
  }, error = function(e) {
    resultado$observacao <- e$message
    return(resultado)
  })
}

# -----------------------------------------------------------------------------
# 3. LOOP PRINCIPAL
# -----------------------------------------------------------------------------

pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent)",
  total = length(nomes_especies)
)

lista_resultados <- vector("list", length(nomes_especies))

for (i in seq_along(nomes_especies)) {
  
  lista_resultados[[i]] <- processar_especie(nomes_especies[i], dir_saida)
  
  pb$tick()
  Sys.sleep(1)
}

resultados <- bind_rows(lista_resultados)

# -----------------------------------------------------------------------------
# 4. RELATÓRIO FINAL
# -----------------------------------------------------------------------------

write.csv(resultados, file.path(dir_saida, "log_processamento.csv"), row.names = FALSE)

cat("\nFinalizado!\n")