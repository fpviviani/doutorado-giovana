library(mapview)
library(viridisLite)
library(raster)
library(sf)

# Carregar raster
raster_adequabilidade <- raster("C:/Users/giova/modelagem-10km-codigo/Output/Modelagem_10km/Alipiopsitta_xanthops_thin_10km_ensemble.tif")

# Corrigir CRS do raster (marcado como "unknown", mas é WGS84)
raster::crs(raster_adequabilidade) <- sp::CRS(SRS_string = "EPSG:4326")

# Carregar pontos de ocorrência
occ <- read.csv("C:/Users/giova/modelagem-10km-codigo/Input/new_occ_10km/Alipiopsitta_xanthops_thin_10km.csv")
occ_sf <- st_as_sf(occ, coords = c("Longitude", "Latitude"), crs = 4326)

# Plotar mapa com pontos de ocorrência
mapview(raster_adequabilidade,
        col.regions = viridisLite::turbo,
        na.color    = "transparent",
        layer.name  = "Adequabilidade") +
  mapview(occ_sf,
          layer.name = "Ocorrências",
          col.regions = "white",
          cex = 3)
