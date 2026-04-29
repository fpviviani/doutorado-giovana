  # =============================================================================
  # SCRIPT AUTOMATIZADO - GERAR BUFFERS PARA TODAS AS ESPÉCIES
  # =============================================================================
  
  # Carregar pacotes
  library(sf)
  library(adehabitatHR)
  library(dplyr)
  library(progress)
  
  # -----------------------------------------------------------------------------
  # 1. CONFIGURAÇÕES INICIAIS
  # -----------------------------------------------------------------------------
  
  # Pastas de entrada e saída
  pasta_ocorrencias <- "C:/Users/giova/modelagem-10km-codigo/Input/new_occ_10km"
  pasta_buffers <- "C:/Users/giova/modelagem-10km-codigo/Input/new_buffers_98"
  
  # Criar pasta de buffers se não existir
  if (!dir.exists(pasta_buffers)) {
    dir.create(pasta_buffers, recursive = TRUE)
    cat("📁 Pasta criada:", pasta_buffers, "\n")
  }
  
  # Listar todos os arquivos CSV
  arquivos <- list.files(pasta_ocorrencias, pattern = "\\.csv$", full.names = TRUE)
  cat("📊 Encontrados", length(arquivos), "arquivos CSV\n")

# Margem em metros (2 graus ≈ 222 km)
margem_metros <- 2 * 111000  # 222,000 metros

# -----------------------------------------------------------------------------
# 2. FUNÇÃO PARA PROCESSAR UMA ESPÉCIE
# -----------------------------------------------------------------------------

processar_buffer <- function(arquivo, pasta_saida, margem_metros) {
  
  # Extrair nome da espécie do nome do arquivo
  nome_arquivo <- basename(arquivo)
  nome_especie <- gsub("_rarefeito\\.csv$|\\.csv$", "", nome_arquivo)
  nome_especie <- gsub("_bruto$", "", nome_especie)
  
  cat("\n", paste(rep("-", 50), collapse = ""), "\n")
  cat("Processando:", nome_especie, "\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  tryCatch({
    
    # -------------------------------------------------------------------------
    # 2.1 CARREGAR DADOS
    # -------------------------------------------------------------------------
    
    sp <- read.csv(arquivo)
    
    # Verificar se há dados
    if (nrow(sp) == 0) {
      cat("❌ Arquivo vazio\n")
      return(NULL)
    }
    
    # Identificar colunas de coordenadas
    possiveis_long <- c("Longitude", "long", "x", "decimalLongitude", "lon", "X", "decimalLongitude")
    possiveis_lat <- c("Latitude", "lat", "y", "decimalLatitude", "Y", "decimalLatitude")
    
    # Encontrar colunas presentes no arquivo
    nomes_colunas <- names(sp)
    long_col <- intersect(possiveis_long, nomes_colunas)[1]
    lat_col <- intersect(possiveis_lat, nomes_colunas)[1]
    
    if (is.na(long_col) || is.na(lat_col)) {
      cat("❌ Colunas de coordenadas não encontradas\n")
      cat("Colunas disponíveis:", paste(nomes_colunas, collapse = ", "), "\n")
      return(NULL)
    }
    
    cat("📍 Longitude:", long_col, "\n")
    cat("📍 Latitude:", lat_col, "\n")
    
    # -------------------------------------------------------------------------
    # 2.2 CRIAR OBJETO SPATIAL
    # -------------------------------------------------------------------------
    
    # Criar cópia com coordenadas
    sp_coords <- sp[, c(long_col, lat_col)]
    sp_coords <- sp_coords[complete.cases(sp_coords), ]
    if (nrow(sp_coords) < 3) {
      cat("❌ Menos de 3 pontos válidos\n")
      return(NULL)
    }
    
    # Converter para SpatialPoints
    coordinates(sp_coords) <- c(long_col, lat_col)
    proj4string(sp_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

    # -------------------------------------------------------------------------
    # 2.3 CALCULAR MCP (Mínimo Polígono Convexo)
    # -------------------------------------------------------------------------
    
    cat("🔄 Calculando MCP...\n")
    
    mcp_original <- mcp(sp_coords, percent = 98)
    mcp_sf <- st_as_sf(mcp_original)
    
    # -------------------------------------------------------------------------
    # 2.4 ENCONTRAR ZONA UTM
    # -------------------------------------------------------------------------
    
    centro <- st_centroid(mcp_sf)
    coords_centro <- st_coordinates(centro)
    longitude_centro <- coords_centro[1]
    
    zona_utm <- floor((longitude_centro + 180) / 6) + 1
    
    # Definir se é hemisfério sul
    hemisferio <- ifelse(coords_centro[2] < 0, "+south", "")
    crs_utm <- paste0("+proj=utm +zone=", zona_utm, " ", hemisferio, " +datum=WGS84 +units=m +no_defs")
    
    cat("🌍 Zona UTM:", zona_utm, "\n")
    
    # -------------------------------------------------------------------------
    # 2.5 CONVERTER PARA UTM E ADICIONAR MARGEM
    # -------------------------------------------------------------------------
    
    mcp_utm <- st_transform(mcp_sf, crs = crs_utm)
    
    # Calcular área original
    area_original <- as.numeric(st_area(mcp_utm))
    
    # Adicionar margem
    mcp_utm_com_margem <- st_buffer(mcp_utm, dist = margem_metros)
    
    # Calcular nova área
    area_com_margem <- as.numeric(st_area(mcp_utm_com_margem))
    
    # Converter de volta para WGS84
    mcp_com_margem <- st_transform(mcp_utm_com_margem, crs = 4326)
    
    # -------------------------------------------------------------------------
    # 2.6 SALVAR RESULTADO
    # -------------------------------------------------------------------------
    
    # Nome do arquivo de saída
    nome_saida <- file.path(pasta_saida, paste0(nome_especie, "_buffer.shp"))
    
    # Salvar shapefile
    st_write(mcp_com_margem, nome_saida, delete_dsn = TRUE, quiet = TRUE)
    
    # -------------------------------------------------------------------------
    # 2.7 RESUMO
    # -------------------------------------------------------------------------
    
    cat("✅ Buffer salvo:", basename(nome_saida), "\n")
    cat("📊 Pontos utilizados:", nrow(sp_coords), "\n")
    cat("📐 Área original:", round(area_original / 1e6, 2), "km²\n")
    cat("📏 Área com margem:", round(area_com_margem / 1e6, 2), "km²\n")
    cat("📈 Expansão:", round(area_com_margem / area_original, 1), "x\n")
    
    return(data.frame(
      especie = nome_especie,
      pontos = length(sp_coords),
      area_original_km2 = round(area_original / 1e6, 2),
      area_buffer_km2 = round(area_com_margem / 1e6, 2),
      expansao = round(area_com_margem / area_original, 1),
      zona_utm = zona_utm,
      status = "sucesso"
    ))
    
  }, error = function(e) {
    cat("❌ ERRO:", e$message, "\n") 
    cat("ERRO: ", deparse(e$call), "\n")
    cat("especie =", nome_especie, "\n")
    cat("pontos =", nrow(sp_coords), "\n")
    cat("area_original_km2 =", round(area_original / 1e6, 2), "\n")
    cat("area_buffer_km2 =", round(area_com_margem / 1e6, 2), "\n")
    cat("expansao =", round(area_com_margem / area_original, 1), "\n")
    cat("zona_utm =", zona_utm, "\n")
    return(data.frame(
      especie = nome_especie,
      pontos = NA,
      area_original_km2 = NA,
      area_buffer_km2 = NA,
      expansao = NA,
      zona_utm = NA,
      status = "erro",
      erro = e$message
    ))
  })
}

# -----------------------------------------------------------------------------
# 3. PROCESSAR TODAS AS ESPÉCIES
# -----------------------------------------------------------------------------

cat("\n", paste(rep("#", 60), collapse = ""), "\n")
cat("INICIANDO PROCESSAMENTO DE BUFFERS\n")
cat("Margem:", margem_metros / 1000, "km (", margem_metros/111000, "graus)\n")
cat(paste(rep("#", 60), collapse = ""), "\n\n")

# Data.frame para resultados
resultados_buffers <- data.frame()

# Barra de progresso
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) | ETA: :eta",
  total = length(arquivos),
  clear = FALSE
)

# Loop sobre todos os arquivos
for (i in seq_along(arquivos)) {
  
  # Processar espécie
  resultado <- processar_buffer(
    arquivo = arquivos[i],
    pasta_saida = pasta_buffers,
    margem_metros = margem_metros
  )
  
  # Acumular resultados
  if (!is.null(resultado)) {
    resultados_buffers <- rbind(resultados_buffers, resultado)
  }
  
  # Atualizar progresso
  pb$tick()
}

# -----------------------------------------------------------------------------
# 4. RELATÓRIO FINAL
# -----------------------------------------------------------------------------

cat("\n", paste(rep("#", 60), collapse = ""), "\n")
cat("PROCESSAMENTO DE BUFFERS CONCLUÍDO!\n")
cat(paste(rep("#", 60), collapse = ""), "\n\n")

# Resumo
cat("📊 RESUMO:\n")
cat("Total de espécies processadas:", nrow(resultados_buffers), "\n")
cat("Sucessos:", sum(resultados_buffers$status == "sucesso"), "\n")
cat("Erros:", sum(resultados_buffers$status == "erro"), "\n\n")

# Estatísticas
if (sum(resultados_buffers$status == "sucesso") > 0) {
  sucessos <- resultados_buffers[resultados_buffers$status == "sucesso", ]
  
  cat("📈 Médias:\n")
  cat("  Pontos por espécie:", round(mean(sucessos$pontos, na.rm = TRUE), 1), "\n")
  cat("  Área original (km²):", round(mean(sucessos$area_original_km2, na.rm = TRUE), 1), "\n")
  cat("  Área com buffer (km²):", round(mean(sucessos$area_buffer_km2, na.rm = TRUE), 1), "\n")
  cat("  Expansão média:", round(mean(sucessos$expansao, na.rm = TRUE), 1), "x\n")
}

# Mostrar erros se houver
if (sum(resultados_buffers$status == "erro") > 0) {
  cat("\n❌ Espécies com erro:\n")
  print(resultados_buffers[resultados_buffers$status == "erro", 
                           c("especie", "erro")])
}

# Salvar log dos buffers
log_buffers <- file.path(pasta_buffers, "log_buffers.csv")
write.csv(resultados_buffers, log_buffers, row.names = FALSE)
cat("\n✅ Log dos buffers salvo em:", log_buffers, "\n")

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 5. FIM DO PROCESSAMENTO
# -----------------------------------------------------------------------------

cat("\n✅ Todos os buffers foram gerados!\n")
cat("📁 Pasta de saída:", pasta_buffers, "\n")
cat("📊 Log salvo em:", file.path(pasta_buffers, "log_buffers.csv"), "\n")