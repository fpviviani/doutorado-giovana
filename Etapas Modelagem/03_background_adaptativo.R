# -----------------------------------------------------------------------------
# 3. FUNÇÃO DE BACKGROUND ADAPTATIVO
# -----------------------------------------------------------------------------

calcular_background <- function(n_ocorrencias) {
  bg <- n_ocorrencias * 1  # proporção 1:1
  bg <- max(background_min, min(background_max, bg))
  cat("   📊 Background:", bg, "(", round(bg/n_ocorrencias, 1), "× ocorrências)\n")
  return(bg)
}
