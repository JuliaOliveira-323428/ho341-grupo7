# ---------------------------------------------------------------------------
# HO-341 - Grupo 7 - pacotes do projeto. Rode UMA VEZ em cada computador.
# (Rodar de novo nao estraga nada: so instala o que falta.)
# ---------------------------------------------------------------------------

pacotes <- c(
  "tidyverse",  # leitura, manipulacao e graficos
  "sidrar",     # tabelas agregadas do SIDRA/IBGE
  "scales"      # formatacao de eixos nos graficos
)

faltantes <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltantes) > 0) install.packages(faltantes)

# O sidrar precisa ser 0.5.1 ou mais recente, por dois motivos:
#  1. o argumento value_type = "character", que devolve os sinais do IBGE
#     ("-" = zero, "X" = sigilo) em vez de transformar todos em NA;
#  2. o desvio automatico para a API de agregados do IBGE quando o servidor do
#     SIDRA responde com o desafio do Cloudflare.
# Se o CRAN ainda tiver versao antiga, instala do GitHub do autor (pacote so
# em R, nao precisa de Rtools).
versao_minima <- "0.5.1"
if (packageVersion("sidrar") < versao_minima) {
  try(install.packages("sidrar"), silent = TRUE)
}
if (packageVersion("sidrar") < versao_minima) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("rpradosiqueira/sidrar", upgrade = "never")
}

message("sidrar versao ", packageVersion("sidrar"),
        ". Se o sidrar ja estava carregado, use Session > Restart R antes de seguir.")
