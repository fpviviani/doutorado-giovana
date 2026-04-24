  # =============================================================================
  # Script para download, limpeza e rarefação de UMA espécie
  # Autor: Giovana D. Cabral
  # Data: 2026-04-14
  # =============================================================================
  
  # 0. Configurações iniciais
  library(rgbif)
  library(dplyr)
  library(CoordinateCleaner)
  library(spThin)
  
  # Parâmetros da espécie
  especie_nome <- "Pteroglossus bitorquatus"      
  especie_busca <- "Pteroglossus bitorquatus"     
  thin_distancia_km <- 10
  reps <- 50
  dir_saida <- "C:/Users/giova/modelagem-15km-codigo/Input/new_occ_10km"
  
  # Criar diretório se não existir
  if(!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)
  
  # -----------------------------------------------------------------------------
  # 1. Download com retry e tratamento de erro
  # -----------------------------------------------------------------------------
  baixar_com_retry <- function(nome_busca, tentativas = 3, delay = 2) {
    for(tent in 1:tentativas) {
      cat(sprintf("\nTentativa %d de %d...\n", tent, tentativas))
      resultado <- tryCatch({
        occ_search(
          scientificName = nome_busca,
          country = 'BR',
          year = '2005,2025',
          hasCoordinate = TRUE,
          limit = 15000,
          fields = c('scientificName', 'decimalLatitude', 'decimalLongitude')
        )$data
      }, error = function(e) {
        if(grepl("503|Service Unavailable|timeout", e$message, ignore.case = TRUE)) {
          cat("Erro transitório:", e$message, "\n")
          return(NULL)
        } else {
          stop(e)
        }
      })
      
      if(!is.null(resultado) && nrow(resultado) > 0) {
        cat("Download bem-sucedido! Registros brutos:", nrow(resultado), "\n")
        return(resultado)
      }
      
      if(tent < tentativas) {
        cat(sprintf("Aguardando %d segundos antes de nova tentativa...\n", delay))
        Sys.sleep(delay)
        delay <- delay * 1.5
      }
    }
    cat("Falha no download após todas as tentativas.\n")
    return(NULL)
  }
  
  # Executar download
  ocorrencias_raw <- baixar_com_retry(especie_busca)
  
  if(is.null(ocorrencias_raw) || nrow(ocorrencias_raw) == 0) {
    stop("Sem dados para a espécie. Verifique o nome ou a conectividade.")
  }
  
  # -----------------------------------------------------------------------------
  # 2. Limpeza coordenadas (Coordinate Cleaner) - Usando base R
  # -----------------------------------------------------------------------------
  cat("\nIniciando limpeza de coordenadas...\n")
  
  # Executar limpeza
  resultado_clean <- clean_coordinates(
    x = ocorrencias_raw,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    tests = c("capitals", "centroids", "equal", "gbif", "institutions", "zeros")
  )
  
  # Converter para data.frame padrão
  resultado_clean <- as.data.frame(resultado_clean)
  
  # Filtrar apenas registros válidos (usando base R)
  ocorrencias_validas <- resultado_clean[resultado_clean$.summary == TRUE, ]
  
  # Remover colunas temporárias que começam com ponto (usando base R)
  colunas_remover <- grep("^\\.", names(ocorrencias_validas), value = TRUE)
  if(length(colunas_remover) > 0) {
    ocorrencias_validas <- ocorrencias_validas[, !names(ocorrencias_validas) %in% colunas_remover]
  }
  
  # Remover duplicatas (mesma lat/long)
  ocorrencias_validas <- ocorrencias_validas[!duplicated(ocorrencias_validas[, c("decimalLatitude", "decimalLongitude")]), ]
  
  cat("Registros após limpeza:", nrow(ocorrencias_validas), "\n")
  
  # -----------------------------------------------------------------------------
  # 3. Manter apenas longitude e latitude
  # -----------------------------------------------------------------------------
  # Extrair apenas as colunas de coordenadas
  ocorrencias_coords <- ocorrencias_validas[, c("decimalLongitude", "decimalLatitude")]
  names(ocorrencias_coords) <- c("Longitude", "Latitude")
  
  cat("Registros para rarefação:", nrow(ocorrencias_coords), "\n")
  
  # -----------------------------------------------------------------------------
  # 4. Rarefação com spThin
  # -----------------------------------------------------------------------------
  if(nrow(ocorrencias_coords) > 0) {
    cat("\nIniciando rarefação...\n")
    
    # spThin exige coluna de espécie; adiciona coluna auxiliar
    ocorrencias_coords$species <- especie_nome

    # Executar thinning
    thin_result <- thin(
      loc.data = ocorrencias_coords,
      lat.col = "Latitude",
      long.col = "Longitude",
      spec.col = "species",
      thin.par = thin_distancia_km,
      reps = reps,
      locs.thinned.list.return = TRUE,
      write.files = FALSE,
      write.log.file = FALSE
    )
    
    # Encontrar a replicata com mais pontos
    n_points <- sapply(thin_result, nrow)
    best_rep <- which.max(n_points)
    ocorrencias_rarefeitas <- thin_result[[best_rep]]
    
    cat("\nResultados da rarefação:\n")
    cat("  - Pontos antes da rarefação:", nrow(ocorrencias_coords), "\n")
    cat("  - Pontos após rarefação (", thin_distancia_km, "km):", nrow(ocorrencias_rarefeitas), "\n")
    cat("  - Replicata mais rica:", best_rep, "com", max(n_points), "pontos\n")
    
    # -------------------------------------------------------------------------
    # 5. Salvar resultado final (apenas longitude e latitude)
    # -------------------------------------------------------------------------
    # Renomear colunas para o formato final
    names(ocorrencias_rarefeitas) <- c("Longitude", "Latitude")
    
    # Salvar em CSV
    arquivo_saida <- file.path(dir_saida, paste0(gsub(" ", "_", especie_nome), "_thin.csv"))
    write.csv(ocorrencias_rarefeitas, arquivo_saida, row.names = FALSE)
    
    cat("\n✓ Arquivo salvo em:", arquivo_saida, "\n")
    cat("✓ Total de registros finais:", nrow(ocorrencias_rarefeitas), "\n")
    
    # Mostrar primeiras linhas
    cat("\nPrimeiras linhas do arquivo final:\n")
    print(head(ocorrencias_rarefeitas))
    
  } else {
    cat("\n✗ Nenhum registro restante após limpeza. Verifique os dados.\n")
  }
  
  # -----------------------------------------------------------------------------
  # 6. Resumo final
  # -----------------------------------------------------------------------------
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("RESUMO DO PROCESSAMENTO\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("Espécie:", especie_nome, "\n")
  cat("Download inicial:", nrow(ocorrencias_raw), "registros\n")
  cat("Após limpeza:", nrow(ocorrencias_validas), "registros\n")
  cat("Após rarefação:", ifelse(exists("ocorrencias_rarefeitas"), nrow(ocorrencias_rarefeitas), 0), "registros\n")
  cat("Arquivo de saída:", file.path(dir_saida, paste0(gsub(" ", "_", especie_nome), "_thin_10km.csv")), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")