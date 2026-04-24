# -----------------------------------------------------------------------------
# 3. FUNÇÃO DE BACKGROUND ADAPTATIVO
# Baseado em Barbet-Massin (2012) e Rausell-Moreno (2025)
# -----------------------------------------------------------------------------

calcular_background <- function(n_ocorrencias) {
  if (n_ocorrencias < 100) {
    bg <- 5000
    cat("   🟡 MENOS DE 50 OCORRÊNCIAS (", n_ocorrencias, 
        ") → Background: 5.000 (fixo)\n")
  } else {
    bg <- 10000
    cat("   🔵 50 OU MAIS OCORRÊNCIAS (", n_ocorrencias, 
        ") → Background: 10.000 (fixo)\n")
  }
  return(bg)
}