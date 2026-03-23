# -----------------------------------------------------------------------------
# 4. LIMPEZA INICIAL
# -----------------------------------------------------------------------------

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("INICIANDO MODELAGEM\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("🧹 LIMPEZA INICIAL...\n")

temp_windows <- "C:/Users/giova/AppData/Local/Temp"
if (dir.exists(temp_windows)) {
  arquivos <- list.files(temp_windows, full.names = TRUE, recursive = FALSE)
  if (length(arquivos) > 0) {
    info <- file.info(arquivos)
    apenas_arquivos <- rownames(info[!info$isdir, ])
    if (length(apenas_arquivos) > 0) {
      cat("   🗑️ Removendo", length(apenas_arquivos), "arquivos\n")
      unlink(apenas_arquivos, force = TRUE)
    }
  }
}

if (dir.exists(dir_temp)) {
  arquivos_terra <- list.files(dir_temp, full.names = TRUE)
  if (length(arquivos_terra) > 0) {
    unlink(arquivos_terra, force = TRUE)
  }
}

gc()
cat("✅ LIMPEZA CONCLUÍDA!\n\n")
