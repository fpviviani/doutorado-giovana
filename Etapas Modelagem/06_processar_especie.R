# -----------------------------------------------------------------------------
# 6. FUNÇÃO PRINCIPAL DE PROCESSAMENTO
# -----------------------------------------------------------------------------

processar_especie <- function(especie_info, bioclimaticas, tentativa = 1) {
  
  especie <- especie_info$especie

  # --- LOG POR ESPÉCIE ---
  dir_logs_especies <- file.path(dir_relatorios, "logs_especies")
  if (!dir.exists(dir_logs_especies)) dir.create(dir_logs_especies, recursive = TRUE)
  log_path <- file.path(dir_logs_especies, paste0(especie, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))

  con <- file(log_path, open = "at", encoding = "UTF-8")
  sink(con, append = TRUE, split = TRUE)
  sink(con, append = TRUE, type = "message")
  on.exit({
    try(sink(type = "message"), silent = TRUE)
    try(sink(), silent = TRUE)
    try(close(con), silent = TRUE)
  }, add = TRUE)

  cat("\n===== LOG INICIADO: ", as.character(Sys.time()), " | ", especie, " =====\n")

  cat("\n", paste(rep("=", 70), collapse = ""), "\n")
  cat("🔷 PROCESSANDO:", especie, "\n")
  cat("🔄 Tentativa:", tentativa, "/", max_tentativas, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  
  resultado <- data.frame(
    especie = especie,
    status = "falha",
    n_ocorrencias = 0,
    n_background_usado = 0,
    n_variaveis_selecionadas = 0,
    metodos_solicitados = paste(metodos_modelagem, collapse = ','),
    metodos_rodados = NA_character_,
    metodos_faltando = NA_character_,
    auc_media = NA,
    tss_media = NA,
    tempo_min = NA,
    erro = NA_character_
  )
  
  tempo_inicio <- Sys.time()
  
  tryCatch({

    # Validações rápidas
    if (!inherits(bioclimaticas, "SpatRaster")) stop("bioclimaticas inválido (não é SpatRaster)")
    if (nlyr(bioclimaticas) < 1) stop("bioclimaticas inválido (sem camadas)")

    # 1. Carregar ocorrências
    cat("\n1️⃣ Carregando ocorrências...\n")
    sp <- read.csv(especie_info$arquivo)
    
    if ("decimalLongitude" %in% names(sp)) {
      names(sp)[names(sp) == "decimalLongitude"] <- "longitude"
      names(sp)[names(sp) == "decimalLatitude"] <- "latitude"
    }
    
    sp <- sp[complete.cases(sp[, c("longitude", "latitude")]), ]
    resultado$n_ocorrencias <- nrow(sp)
    
    if (nrow(sp) < 5) stop("Menos de 5 ocorrências")
    cat("   ✅", nrow(sp), "ocorrências\n")
    
    # 2. Calcular background
    n_background <- calcular_background(nrow(sp))
    resultado$n_background_usado <- n_background
    
    # 3. Carregar buffer
    cat("\n2️⃣ Carregando buffer...\n")
    buffer_shp <- file.path(dir_buffers, paste0(especie, "_MCP_2graus.shp"))
    if (!file.exists(buffer_shp)) {
      buffer_shp <- file.path(dir_buffers, paste0(especie, ".shp"))
    }
    if (!file.exists(buffer_shp)) {
      buffer_shp <- file.path(dir_buffers, paste0(especie, "buffer.shp"))
    }
    if (!file.exists(buffer_shp)) stop("Buffer não encontrado")
    
    buffer <- vect(buffer_shp)

    # --- DIAGNÓSTICO (buffer / CRS / arquivos) ---
    cat("   🔎 buffer_shp:", buffer_shp, "\n")
    cat("   🔎 buffer_shp exists:", file.exists(buffer_shp), "\n")
    buffer_prj <- sub("\\.shp$", ".prj", buffer_shp, ignore.case = TRUE)
    cat("   🔎 buffer_prj:", buffer_prj, " exists:", file.exists(buffer_prj), "\n")
    cat("   🔎 crs(buffer):", as.character(terra::crs(buffer)), "\n")
    cat("   🔎 crs(bioclimaticas):", as.character(terra::crs(bioclimaticas)), "\n")

    # 4. Recortar variáveis
    cat("\n3️⃣ Recortando variáveis...\n")
    vars_buffer <- crop(bioclimaticas, buffer)
    vars_buffer <- mask(vars_buffer, buffer)
    cat("   📊", nlyr(vars_buffer), "camadas\n")
    cat("   🔎 ext(vars_buffer):", paste(as.vector(terra::ext(vars_buffer)), collapse = ", "), "\n")
    if (nlyr(vars_buffer) < 1) stop("Recorte de variáveis resultou em 0 camadas")

    # 5. Extrair valores
    sp_vect <- vect(sp[, c("longitude", "latitude")], 
                    geom = c("longitude", "latitude"), 
                    crs = "epsg:4326")
    cat("   🔎 crs(sp_vect):", as.character(terra::crs(sp_vect)), "\n")

    # quick check: how many points fall inside raster extent (in the CURRENT CRS of sp_vect)
    lon <- sp$longitude
    lat <- sp$latitude
    e <- terra::ext(vars_buffer)
    inside_extent <- sum(lon >= e[1] & lon <= e[2] & lat >= e[3] & lat <= e[4], na.rm = TRUE)
    cat("   🔎 ocorrências dentro do extent do raster (check simples lon/lat):", inside_extent, "/", nrow(sp), "\n")

    sp_extract <- terra::extract(vars_buffer, sp_vect)
    sp_extract <- sp_extract[, -1, drop = FALSE]
    cat("   🔎 extract: nrow=", nrow(sp_extract), " ncol=", ncol(sp_extract), "\n")
    if (nrow(sp_extract) > 0) {
      na_per_col <- colSums(is.na(sp_extract))
      na_show <- paste(names(na_per_col), as.integer(na_per_col), sep = "=", collapse = ", ")
      cat("   🔎 NAs por coluna:", na_show, "\n")
    }

    # Não punir NA: se a única NA for em cobertura_arborea, não descartamos a ocorrência.
    # Estratégia parametrizável (02_params.R):
    # - na_cobertura_strategy = "zero" (default) ou "median"
    if ("cobertura_arborea" %in% names(sp_extract)) {
      na_cov <- sum(is.na(sp_extract$cobertura_arborea))
      if (na_cov > 0) {
        strat <- "zero"
        if (exists("na_cobertura_strategy") && !is.null(na_cobertura_strategy)) {
          strat <- tolower(trimws(as.character(na_cobertura_strategy)))
        }

        if (strat == "median") {
          med <- suppressWarnings(median(sp_extract$cobertura_arborea, na.rm = TRUE))
          if (is.finite(med)) {
            sp_extract$cobertura_arborea[is.na(sp_extract$cobertura_arborea)] <- med
            cat("   ⚠️ cobertura_arborea tinha", na_cov, "NA(s); imputado com mediana=", round(med, 4), "\n")
          } else {
            cat("   ⚠️ cobertura_arborea está toda NA; removendo esta variável.\n")
            sp_extract$cobertura_arborea <- NULL
          }
        } else {
          # default: zero
          sp_extract$cobertura_arborea[is.na(sp_extract$cobertura_arborea)] <- 0
          cat("   ⚠️ cobertura_arborea tinha", na_cov, "NA(s); imputado com zero.\n")
        }
      }
    }

    # Agora sim: manter apenas linhas completas para as variáveis restantes
    sp_extract <- sp_extract[complete.cases(sp_extract), ]
    if (nrow(sp_extract) < 1) stop("Extração das variáveis retornou 0 linhas (ocorrências fora do raster/buffer?)")
    if (ncol(sp_extract) < 1) stop("Extração das variáveis retornou 0 colunas")
    
    # 6. VIF
    cat("\n4️⃣ Analisando VIF...\n")
    vif_resultado <- tryCatch({
      vifstep(sp_extract, th = limiar_vif)
    }, error = function(e) NULL)
    
    if (!is.null(vif_resultado)) {
      vars_selecionadas <- exclude(vars_buffer, vif_resultado)
      resultado$n_variaveis_selecionadas <- nlyr(vars_selecionadas)
      cat("   ✅", resultado$n_variaveis_selecionadas, "variáveis selecionadas\n")
    } else {
      vars_selecionadas <- vars_buffer
      resultado$n_variaveis_selecionadas <- nlyr(vars_selecionadas)
      cat("   ⚠️ Usando todas as", resultado$n_variaveis_selecionadas, "variáveis\n")
    }
    
    # 7. Converter para stack
    cat("\n5️⃣ Convertendo para formato raster...\n")
    raster_temp <- file.path(dir_temp, paste0(especie, "_vars.tif"))
    terra::writeRaster(vars_selecionadas, raster_temp, overwrite = TRUE, gdal = c("COMPRESS=NONE"))
    if (!file.exists(raster_temp)) stop("Falha ao criar raster temporário")
    vars_stack <- raster::stack(raster_temp)
    
    # 8. Preparar dados
    cat("\n6️⃣ Preparando dados com", n_background, "backgrounds...\n")
    
    sp_sf <- st_as_sf(sp, coords = c("longitude", "latitude"), crs = 4326)
    sp_sf$presenca <- 1
    sp_sp <- as(sp_sf, "Spatial")
    
    mdata <- sdmData(
      formula = presenca ~ .,
      train = sp_sp,
      predictors = vars_stack,
      bg = list(n = n_background, method = "gRandom", remove = TRUE)
    )
    
    # 9. Calibrar modelos
    cat("\n7️⃣ Calibrando modelos...\n")
    
    modelo <- sdm(
      formula = presenca ~ .,
      data = mdata,
      methods = metodos_modelagem,
      replication = "sub",
      n = n_replicacoes,
      test.percent = test_percent,
      parallelSettings = list(ncore = n_cores, method = "parallel")
    )

    # Registrar quais métodos realmente foram calibrados
    info_modelos <- tryCatch(getModelInfo(modelo), error = function(e) NULL)
    if (!is.null(info_modelos) && is.data.frame(info_modelos)) {
      col_metodo <- if ("method" %in% names(info_modelos)) "method" else if ("model" %in% names(info_modelos)) "model" else NA
      if (!is.na(col_metodo)) {
        metodos_presentes <- unique(as.character(info_modelos[[col_metodo]]))
        metodos_presentes <- metodos_presentes[!is.na(metodos_presentes) & metodos_presentes != ""]
        resultado$metodos_rodados <- paste(metodos_presentes, collapse = ',')
        metodos_faltando <- setdiff(metodos_modelagem, metodos_presentes)
        resultado$metodos_faltando <- paste(metodos_faltando, collapse = ',')
      }
    }
    
    # 10. Avaliar (SIMPLIFICADO)
    cat("\n8️⃣ Avaliando modelos...\n")
    
    eval_stats <- tryCatch({
      getEvaluation(modelo, stat = c("AUC", "TSS"), opt = 2)
    }, error = function(e) {
      cat("   ⚠️ Erro na avaliação:", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(eval_stats)) {
      stop("Falha ao obter avaliação")
    }
    
    # Calcular médias
    resultado$auc_media <- round(mean(eval_stats$AUC, na.rm = TRUE), 4)
    resultado$tss_media <- round(mean(eval_stats$TSS, na.rm = TRUE), 4)
    
    cat("   📈 AUC:", resultado$auc_media, "\n")
    cat("   📈 TSS:", resultado$tss_media, "\n")
    
    # 11. Ensemble
    cat("\n9️⃣ Gerando mapa ensemble (em chunks) ...\n")

    arquivo_mapa <- file.path(dir_modelagem, paste0(especie, "_ensemble.tif"))
    pred_tmp <- file.path(dir_temp, paste0(especie, "_pred_tmp.tif"))

    # Observação: o sdm::ensemble() chama predict() internamente quando newdata é raster
    # e suporta escrever em arquivo (filename/pFilename), o que força processamento em blocos
    # via terra (evita segurar o raster inteiro em memória).
    ensemble_res <- tryCatch({
      ensemble(
        modelo,
        newdata = vars_stack,
        filename = arquivo_mapa,
        overwrite = TRUE,
        pFilename = pred_tmp,
        setting = list(method = "weighted", stat = "TSS"),
        wopt = list(gdal = c("COMPRESS=LZW"))
      )
    }, error = function(e) {
      cat("   ⚠️ Weighted falhou: ", e$message, "\n", sep = "")
      cat("   ⚠️ Usando método média (em chunks)\n")
      ensemble(
        modelo,
        newdata = vars_stack,
        filename = arquivo_mapa,
        overwrite = TRUE,
        pFilename = pred_tmp,
        setting = list(method = "mean"),
        wopt = list(gdal = c("COMPRESS=LZW"))
      )
    })

    # Limpar arquivo temporário de predição (se foi gerado)
    if (file.exists(pred_tmp)) file.remove(pred_tmp)

    # 12. Salvar avaliação
    cat("\n🔟 Salvando resultados...\n")
    
    eval_completo <- as.data.frame(eval_stats)
    eval_completo$especie <- especie
    eval_completo$n_background_usado <- n_background
    eval_completo$metodos_solicitados <- paste(metodos_modelagem, collapse = ',')
    eval_completo$metodos_rodados <- resultado$metodos_rodados
    eval_completo$metodos_faltando <- resultado$metodos_faltando
    write.csv(eval_completo, 
              file.path(dir_avaliacoes, paste0(especie, "_avaliacao.csv")), 
              row.names = FALSE)
    
    tempo_fim <- Sys.time()
    resultado$tempo_min <- round(difftime(tempo_fim, tempo_inicio, units = "mins"), 1)
    resultado$status <- "sucesso"
    
    cat("\n✅ CONCLUÍDO em", resultado$tempo_min, "minutos\n")
    
    # Limpeza
    if (file.exists(raster_temp)) file.remove(raster_temp)
    
  }, error = function(e) {
    cat("\n❌ ERRO:", e$message, "\n")
    resultado$erro <- as.character(e$message)
  })
  
  gc()
  
  return(resultado)
}
