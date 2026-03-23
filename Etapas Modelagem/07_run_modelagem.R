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
  
  resultados_consolidados <- data.frame()
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
      for (tentativa in 1:max_tentativas) {
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
