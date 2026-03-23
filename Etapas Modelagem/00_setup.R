# =============================================================================
# MODELAGEM - VERSÃO SIMPLIFICADA E FUNCIONAL
# =============================================================================

# Configurar ambiente
Sys.unsetenv("JAVA_HOME")

# Carregar pacotes
library(terra)
library(sf)
library(tidyverse)
library(sdm)
library(usdm)
library(progress)
library(parallel)

# Configurar terra
terraOptions(memfrac = 0.4, progress = 10)
sf_use_s2(FALSE)
