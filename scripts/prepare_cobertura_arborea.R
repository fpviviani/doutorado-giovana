#!/usr/bin/env Rscript

# Prepare predictors so biobrasil (larger extent) is cropped/aligned to cobertura (smaller Brazil-only extent).
# This avoids expanding cobertura to South America and keeps both layers aligned for stacking.

suppressPackageStartupMessages({
  library(terra)
})

# Try to set working directory to project root
args <- commandArgs(trailingOnly = TRUE)
project_root <- if (length(args) >= 1 && nzchar(args[1])) args[1] else getwd()
setwd(project_root)

# Load directory config (defines dir_variaveis)
source("Etapas Modelagem/01_config_dirs.R")

# Keep paths consistent with the main pipeline (Etapa 05)
bio_path <- file.path(dir_variaveis, "bio_brasil_30s.tif")
# cobertura is expected in dir_variaveis root
cov_path <- file.path(dir_variaveis, "cobertura_arborea_ambdata.tif")
# Write only the cropped/aligned bioclim (same geometry as cobertura)
out_bio_path <- file.path(dir_variaveis, "bio_brasil_30s_alinhado_cobertura_arborea.tif")

cat("📌 Project root:", normalizePath(getwd(), winslash = "/", mustWork = FALSE), "\n")
cat("📦 Bioclim:", bio_path, "\n")
cat("🌳 Cobertura:", cov_path, "\n")
cat("💾 Output bioclim recortado/alinhado:", out_bio_path, "\n\n")

if (!file.exists(bio_path)) stop("Bioclim not found: ", bio_path)
if (!file.exists(cov_path)) stop("Cobertura not found: ", cov_path)

bio <- rast(bio_path)
# Use first layer for geometry checks
bio_tpl <- bio[[1]]

cov <- rast(cov_path)

cat("🔎 Bioclim ext (orig): ", paste(as.vector(ext(bio_tpl)), collapse = ", "), "\n", sep = "")
cat("🔎 Bioclim res (orig): ", paste(res(bio_tpl), collapse = ", "), "\n", sep = "")
cat("🔎 Bioclim crs (orig): ", as.character(crs(bio_tpl)), "\n", sep = "")

cat("🔎 Cobertura ext (orig): ", paste(as.vector(ext(cov)), collapse = ", "), "\n", sep = "")
cat("🔎 Cobertura res (orig): ", paste(res(cov)), "\n", sep = "")
cat("🔎 Cobertura crs (orig): ", as.character(crs(cov)), "\n", sep = "")

# Use a trimmed cobertura as reference geometry (removes NA-only borders)
cat("✂️ Trimming cobertura (remove bordas só-NA) ...\n")
cov_ref <- trim(cov)
cat("🔎 Cobertura ext (trim): ", paste(as.vector(ext(cov_ref)), collapse = ", "), "\n", sep = "")
cat("🔎 Cobertura res (trim): ", paste(res(cov_ref)), "\n\n", sep = "")

# 1) Ensure CRS matches (use cobertura as reference)
if (!same.crs(cov_ref, bio_tpl)) {
  cat("🔁 Reprojecting bioclim to cobertura CRS...\n")
  bio <- project(bio, crs(cov_ref))
  bio_tpl <- bio[[1]]
}

# 2) Crop bioclim to cobertura TRIM extent (Brazil-only)
cat("✂️ Cropping bioclim to cobertura extent (trim)...\n")
bio_crop <- crop(bio, cov_ref)

# 3) Resample bioclim to cobertura grid (forces res/extent/origin alignment)
# (bioclim is continuous, so bilinear is OK)
cat("🧩 Resampling bioclim to cobertura grid (trim)...\n")
bio_aligned <- resample(bio_crop, cov_ref, method = "bilinear")

# 4) Ensure cobertura is also aligned to itself grid (no-op) but keeps naming consistent
# Sanity checks (geometry must match between bio_aligned[[1]] and cobertura)
stopifnot(same.crs(bio_aligned, cov_ref))
stopifnot(all(res(bio_aligned) == res(cov_ref)))
stopifnot(all(ext(bio_aligned) == ext(cov_ref)))
stopifnot(all(origin(bio_aligned) == origin(cov_ref)))

cat("✅ Aligned: bioclim cropped/resampled to cobertura geometry\n")

# Write output (compressed)
cat("💾 Writing output...\n")
writeRaster(bio_aligned, out_bio_path, overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW")))

cat("✅ Done. Output written to:\n- ", out_bio_path, "\n", sep = "")
