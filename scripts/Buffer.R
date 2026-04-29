# =============================================================================
# SCRIPT AUTOMATIZADO - GERAR BUFFERS PARA TODAS AS ESPÉCIES
# + Exportar CSV filtrado com apenas ocorrências dentro do buffer final
# =============================================================================

# Carregar pacotes
library(sf)
library(adehabitatHR)
library(dplyr)
library(progress)
library(sp)

# -----------------------------------------------------------------------------
# 1. CONFIGURAÇÕES INICIAIS
# -----------------------------------------------------------------------------

# Pastas de entrada e saída
pasta_ocorrencias <- "C:/Users/giova/modelagem-10km-codigo/Input/new_occ_10km"
pasta_buffers <- "C:/Users/giova/modelagem-10km-codigo/Input/new_buffers_98"

# NOVO: pasta para salvar ocorrências filtradas pelo buffer final
pasta_occ_filtradas <- "C:/Users/giova/modelagem-10km-codigo/Input/occ_10km"

# Criar pastas se não existirem
if (!dir.exists(pasta_buffers)) {
  dir.create(pasta_buffers, recursive = TRUE)
  cat("📁 Pasta criada:", pasta_buffers, "\n")
}

if (!dir.exists(pasta_occ_filtradas)) {
  dir.create(pasta_occ_filtradas, recursive = TRUE)
  cat("📁 Pasta criada:", pasta_occ_filtradas, "\n")
}

# Listar todos os arquivos CSV
arquivos <- list.files(pasta_ocorrencias, pattern = "\\.csv$", full.names = TRUE)
cat("📊 Encontrados", length(arquivos), "arquivos CSV\n")

# Margem em metros (2 graus ≈ 222 km)
margem_metros <- 2 * 111000  # 222,000 metros

# -----------------------------------------------------------------------------
# 2. FUNÇÃO PARA PROCESSAR UMA ESPÉCIE
# -----------------------------------------------------------------------------

processar_buffer <- function(arquivo, pasta_saida, pasta_occ_filtradas, margem_metros) {

  # Extrair nome base do arquivo
  nome_arquivo <- basename(arquivo)
  nome_base <- sub("\\.csv$", "", nome_arquivo) # mantém thin_10km etc.

  # Nome limpo (mantém padrão anterior para shapefile)
  nome_especie <- gsub("_rarefeito$", "", nome_base)
  nome_especie <- gsub("_bruto$", "", nome_especie)

  cat("\n", paste(rep("-", 50), collapse = ""), "\n")
  cat("Processando:", nome_base, "\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  tryCatch({

    # -------------------------------------------------------------------------
    # 2.1 CARREGAR DADOS
    # -------------------------------------------------------------------------

    sp_df <- read.csv(arquivo)

    if (nrow(sp_df) == 0) {
      cat("❌ Arquivo vazio\n")
      return(NULL)
    }

    # Identificar colunas de coordenadas
    possiveis_long <- c("Longitude", "long", "x", "decimalLongitude", "lon", "X")
    possiveis_lat <- c("Latitude", "lat", "y", "decimalLatitude", "Y")

    nomes_colunas <- names(sp_df)
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
    # 2.2 PREPARAR PONTOS (remover NAs nas coords)
    # -------------------------------------------------------------------------

    sp_df_coords <- sp_df[complete.cases(sp_df[, c(long_col, lat_col)]), , drop = FALSE]

    if (nrow(sp_df_coords) < 3) {
      cat("❌ Menos de 3 pontos válidos\n")
      return(NULL)
    }

    # Objeto SpatialPoints para MCP
    sp_coords <- sp_df_coords[, c(long_col, lat_col), drop = FALSE]
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

    hemisferio <- ifelse(coords_centro[2] < 0, "+south", "")
    crs_utm <- paste0(
      "+proj=utm +zone=", zona_utm, " ", hemisferio,
      " +datum=WGS84 +units=m +no_defs"
    )

    cat("🌍 Zona UTM:", zona_utm, "\n")

    # -------------------------------------------------------------------------
    # 2.5 CONVERTER PARA UTM E ADICIONAR MARGEM
    # -------------------------------------------------------------------------

    mcp_utm <- st_transform(mcp_sf, crs = crs_utm)

    area_original <- as.numeric(st_area(mcp_utm))

    mcp_utm_com_margem <- st_buffer(mcp_utm, dist = margem_metros)

    area_com_margem <- as.numeric(st_area(mcp_utm_com_margem))

    mcp_com_margem <- st_transform(mcp_utm_com_margem, crs = 4326)
    mcp_com_margem <- st_make_valid(mcp_com_margem)

    # -------------------------------------------------------------------------
    # 2.6 SALVAR SHAPEFILE DO BUFFER
    # -------------------------------------------------------------------------

    nome_saida <- file.path(pasta_saida, paste0(nome_especie, "_buffer.shp"))
    st_write(mcp_com_margem, nome_saida, delete_dsn = TRUE, quiet = TRUE)

    # -------------------------------------------------------------------------
    # 2.7 FILTRAR E SALVAR CSV COM OCORRÊNCIAS DENTRO DO BUFFER FINAL
    # -------------------------------------------------------------------------

    # Criar sf com todos os pontos válidos (mantendo colunas originais)
    pts_sf <- st_as_sf(
      sp_df_coords,
      coords = c(long_col, lat_col),
      crs = 4326,
      remove = FALSE
    )

    inside <- lengths(st_within(pts_sf, mcp_com_margem)) > 0
    sp_filtrado <- sp_df_coords[inside, , drop = FALSE]

    cat("   🧹 Pontos dentro do buffer:", nrow(sp_filtrado), "/", nrow(sp_df_coords), "\n")

    nome_csv_saida <- file.path(pasta_occ_filtradas, paste0(nome_base, ".csv"))
    write.csv(sp_filtrado, nome_csv_saida, row.names = FALSE)
    cat("✅ CSV filtrado salvo:", basename(nome_csv_saida), "\n")

    # -------------------------------------------------------------------------
    # 2.8 RESUMO
    # -------------------------------------------------------------------------

    cat("✅ Buffer salvo:", basename(nome_saida), "\n")
    cat("📊 Pontos (originais válidos):", nrow(sp_df_coords), "\n")
    cat("📊 Pontos (filtrados):", nrow(sp_filtrado), "\n")
    cat("📐 Área original:", round(area_original / 1e6, 2), "km²\n")
    cat("📏 Área com margem:", round(area_com_margem / 1e6, 2), "km²\n")
    cat("📈 Expansão:", round(area_com_margem / area_original, 1), "x\n")

    return(data.frame(
      especie = nome_especie,
      arquivo_base = nome_base,
      pontos_originais_validos = nrow(sp_df_coords),
      pontos_filtrados = nrow(sp_filtrado),
      area_original_km2 = round(area_original / 1e6, 2),
      area_buffer_km2 = round(area_com_margem / 1e6, 2),
      expansao = round(area_com_margem / area_original, 1),
      zona_utm = zona_utm,
      status = "sucesso"
    ))

  }, error = function(e) {
    cat("❌ ERRO:", e$message, "\n")
    cat("ERRO:", deparse(e$call), "\n")
    return(data.frame(
      especie = nome_especie,
      arquivo_base = nome_base,
      pontos_originais_validos = NA,
      pontos_filtrados = NA,
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
cat("Margem:", margem_metros / 1000, "km (", margem_metros / 111000, "graus)\n")
cat(paste(rep("#", 60), collapse = ""), "\n\n")

resultados_buffers <- data.frame()

pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) | ETA: :eta",
  total = length(arquivos),
  clear = FALSE
)

for (i in seq_along(arquivos)) {

  resultado <- processar_buffer(
    arquivo = arquivos[i],
    pasta_saida = pasta_buffers,
    pasta_occ_filtradas = pasta_occ_filtradas,
    margem_metros = margem_metros
  )

  if (!is.null(resultado)) {
    resultados_buffers <- rbind(resultados_buffers, resultado)
  }

  pb$tick()
}

# -----------------------------------------------------------------------------
# 4. RELATÓRIO FINAL
# -----------------------------------------------------------------------------

cat("\n", paste(rep("#", 60), collapse = ""), "\n")
cat("PROCESSAMENTO DE BUFFERS CONCLUÍDO!\n")
cat(paste(rep("#", 60), collapse = ""), "\n\n")

cat("📊 RESUMO:\n")
cat("Total de espécies processadas:", nrow(resultados_buffers), "\n")
cat("Sucessos:", sum(resultados_buffers$status == "sucesso"), "\n")
cat("Erros:", sum(resultados_buffers$status == "erro"), "\n\n")

if (sum(resultados_buffers$status == "sucesso") > 0) {
  sucessos <- resultados_buffers[resultados_buffers$status == "sucesso", ]

  cat("📈 Médias:\n")
  cat("  Pontos originais válidos por espécie:", round(mean(sucessos$pontos_originais_validos, na.rm = TRUE), 1), "\n")
  cat("  Pontos filtrados por espécie:", round(mean(sucessos$pontos_filtrados, na.rm = TRUE), 1), "\n")
  cat("  Área original (km²):", round(mean(sucessos$area_original_km2, na.rm = TRUE), 1), "\n")
  cat("  Área com buffer (km²):", round(mean(sucessos$area_buffer_km2, na.rm = TRUE), 1), "\n")
  cat("  Expansão média:", round(mean(sucessos$expansao, na.rm = TRUE), 1), "x\n")
}

if (sum(resultados_buffers$status == "erro") > 0) {
  cat("\n❌ Espécies com erro:\n")
  print(resultados_buffers[resultados_buffers$status == "erro", c("arquivo_base", "erro")])
}

# Salvar log dos buffers
log_buffers <- file.path(pasta_buffers, "log_buffers.csv")
write.csv(resultados_buffers, log_buffers, row.names = FALSE)
cat("\n✅ Log dos buffers salvo em:", log_buffers, "\n")

cat("\n✅ Todos os buffers foram gerados!\n")
cat("📁 Pasta de saída buffers:", pasta_buffers, "\n")
cat("📁 Pasta de saída ocorrências filtradas:", pasta_occ_filtradas, "\n")
