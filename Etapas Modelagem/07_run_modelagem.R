# -----------------------------------------------------------------------------
# 7. PROCESSAMENTO PRINCIPAL
# -----------------------------------------------------------------------------

if (nrow(especies_pendentes) == 0) {
  cat("\n✅ Todas as espécies já foram processadas!\n")
} else {
  
  cat("\n", paste(rep("🔥", 40), collapse = ""), "\n")
  cat("INICIANDO MODELAGEM\n")
  cat("Background adaptativo: proporção 1:1\n")
  cat("Espécies pendentes:", nrow(especies_pendentes), "\n")
  cat(paste(rep("🔥", 40), collapse = ""), "\n\n")
  # Snapshot de parâmetros usados nesta execução
  parametros_execucao <- data.frame(
    parametro = c(
      'lote_tamanho','pausa_minutos','max_tentativas','n_cores_inicial','n_cores','safe_mode','limiar_vif','n_replicacoes','test_percent',
      'background_min','background_max','metodos_modelagem','especie_partida'
    ),
    valor = c(
      lote_tamanho, pausa_minutos, max_tentativas, n_cores_inicial, n_cores, safe_mode, limiar_vif, n_replicacoes, test_percent,
      background_min, background_max, paste(metodos_modelagem, collapse = ','), especie_partida
    ),
    stringsAsFactors = FALSE
  )
  write.csv(parametros_execucao,
            file.path(dir_relatorios, 'parametros_execucao.csv'),
            row.names = FALSE)

  
  resultados_consolidados <- data.frame()

  # --- CHECKPOINT (retomar execução) ---
  checkpoint_path <- file.path(dir_checkpoint, "progresso.rds")
  if (file.exists(checkpoint_path)) {
    resultados_consolidados <- readRDS(checkpoint_path)
    if (!is.data.frame(resultados_consolidados)) resultados_consolidados <- data.frame()

    if (nrow(resultados_consolidados) > 0 && "especie" %in% names(resultados_consolidados) && "status" %in% names(resultados_consolidados)) {
      especies_concluidas <- unique(resultados_consolidados$especie[resultados_consolidados$status == "sucesso"])
      especies_pendentes <- especies_pendentes[!especies_pendentes$especie %in% especies_concluidas, ]
    }
  }

  # --- ARQUIVO DE ERROS (último erro por espécie) ---
  erros_path <- file.path(dir_relatorios, "erros_por_especie.csv")
  erros_por_especie <- data.frame(
    especie = character(),
    tentativa = integer(),
    erro = character(),
    timestamp = character(),
    stringsAsFactors = FALSE
  )
  if (file.exists(erros_path)) {
    erros_por_especie <- tryCatch(read.csv(erros_path, stringsAsFactors = FALSE), error = function(e) erros_por_especie)
  }

  # --- SAFE MODE AUTOMÁTICO (reduzir n_cores ao detectar erro de memória) ---
  n_cores_inicial <- n_cores
  n_cores_atual <- n_cores
  safe_mode_auto <- FALSE

  is_memory_error <- function(msg) {
    if (is.null(msg) || is.na(msg)) return(FALSE)
    m <- tolower(as.character(msg))
    patterns <- c(
      "cannot allocate",
      "cannot allocate memory",
      "vector memory exhausted",
      "out of memory",
      "std::bad_alloc",
      "bad_alloc",
      "memory",
      "memoria",
      "insufficient memory"
    )
    any(vapply(patterns, function(p) grepl(p, m, fixed = TRUE), logical(1)))
  }

  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", 
                         total = nrow(especies_pendentes), clear = FALSE)
  
  n_lotes <- ceiling(nrow(especies_pendentes) / lote_tamanho)
  
  for (i in 1:n_lotes) {
    
    inicio <- (i-1) * lote_tamanho + 1
    fim <- min(i * lote_tamanho, nrow(especies_pendentes))
    idx_lote <- inicio:fim
    
    cat("\n", paste(rep("★", 60), collapse = ""), "\n")
    cat("LOTE", i, "DE", n_lotes, "\n")
    cat(paste(rep("★", 60), collapse = ""), "\n")
    
    for (j in idx_lote) {
      especie_info <- especies_pendentes[j, ]
      
      sucesso <- FALSE
      tentativa_final <- NA
      for (tentativa in 1:max_tentativas) {
        tentativa_final <- tentativa

        # Aplicar n_cores dinâmico (pode ser reduzido pelo safe mode automático)
        n_cores <- n_cores_atual

        resultado <- processar_especie(especie_info, bioclimaticas, tentativa)
        
        if (resultado$status == "sucesso") {
          sucesso <- TRUE
          resultados_consolidados <- rbind(resultados_consolidados, resultado)
          break
        } else if (tentativa < max_tentativas) {
          cat("\n⏳ Aguardando 2 minutos...\n")
          Sys.sleep(120)
        }
      }
      
      # Safe mode automático: se falhou por memória, reduzir n_cores para a PRÓXIMA espécie
      if (!is.null(resultado) && resultado$status != "sucesso" && is_memory_error(resultado$erro)) {
        safe_mode_auto <- TRUE
        if (n_cores_atual > 3) {
          cat("
⚠️ Safe mode automático ativado (erro de memória). Reduzindo n_cores para 3 nas próximas espécies.
")
          n_cores_atual <- 3
        } else if (n_cores_atual == 3) {
          cat("
⚠️ Novo erro de memória com n_cores=3. Reduzindo n_cores para 1 nas próximas espécies.
")
          n_cores_atual <- 1
        }
      }

      # Registrar último erro por espécie (se falhou)
      if (!is.null(resultado) && resultado$status != "sucesso") {
        nova_linha <- data.frame(
          especie = especie_info$especie,
          tentativa = tentativa_final,
          erro = as.character(resultado$erro),
          timestamp = as.character(Sys.time()),
          stringsAsFactors = FALSE
        )
        if (nrow(erros_por_especie) > 0 && "especie" %in% names(erros_por_especie) && any(erros_por_especie$especie == especie_info$especie)) {
          erros_por_especie[erros_por_especie$especie == especie_info$especie, ] <- nova_linha
        } else {
          erros_por_especie <- rbind(erros_por_especie, nova_linha)
        }
        write.csv(erros_por_especie, erros_path, row.names = FALSE)
      }

      pb$tick()
      
      # Salvar checkpoint
      saveRDS(resultados_consolidados, 
              file.path(dir_checkpoint, "progresso.rds"))
      
      if (j < fim) {
        cat("\n⏳ Pausa de 30 segundos...\n")
        Sys.sleep(30)
      }
    }
    
    # Salvar lote
    write.csv(resultados_consolidados, 
              file.path(dir_relatorios, paste0("lote_", i, ".csv")), 
              row.names = FALSE)
    
    if (i < n_lotes) {
      cat("\n⏸️ PAUSA DE", pausa_minutos, "MINUTOS...\n")
      Sys.sleep(pausa_minutos * 60)
    }
  }
  
  # Resultados finais
  write.csv(resultados_consolidados, 
            file.path(dir_relatorios, "resultados_finais.csv"), 
            row.names = FALSE)
  
  sucessos <- resultados_consolidados[resultados_consolidados$status == "sucesso", ]
  
  cat("\n", paste(rep("📊", 40), collapse = ""), "\n")
  cat("RESUMO FINAL\n")
  cat(paste(rep("📊", 40), collapse = ""), "\n\n")
  
  cat("📈 ESTATÍSTICAS GERAIS:\n")
  cat("  ✅ Sucessos:", nrow(sucessos), "\n")
  cat("  ❌ Falhas:", sum(resultados_consolidados$status == "falha"), "\n")
  
  if (nrow(sucessos) > 0) {
    cat("\n📊 MÉDIAS DOS SUCESSOS:\n")
    cat("  TSS médio:", round(mean(sucessos$tss_media, na.rm = TRUE), 4), "\n")
    cat("  AUC médio:", round(mean(sucessos$auc_media, na.rm = TRUE), 4), "\n")
    cat("  Background médio:", round(mean(sucessos$n_background_usado, na.rm = TRUE), 0), "\n")
    cat("  Ocorrências média:", round(mean(sucessos$n_ocorrencias, na.rm = TRUE), 1), "\n")
  }
}

cat("\n🏁 Script concluído!\n")
