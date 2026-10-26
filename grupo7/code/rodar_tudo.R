# ---------------------------------------------------------------------------
# HO-341 - Grupo 7 - roda o projeto inteiro, de cima a baixo.
# Com o projeto grupo7.Rproj aberto: source("code/rodar_tudo.R")
# ---------------------------------------------------------------------------
if (!file.exists("grupo7.Rproj") && file.exists("../grupo7.Rproj")) setwd("..")

source("code/00_instalar_pacotes.R")
source("code/01_coleta.R")              # so baixa o que ainda nao esta em data/bruto/
source("code/02_limpeza.R")
source("code/03_descritiva_bivariada.R")
source("code/04_metadados.R")