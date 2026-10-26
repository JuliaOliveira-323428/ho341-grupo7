# ---------------------------------------------------------------------------
# HO-341 - Metodos Quantitativos Aplicados a Economia - 2o sem. 2026
# Trabalho final - Grupo 7: Heterogeneidade da agricultura familiar
#
# config.R: definicoes comuns aos scripts (pacotes, pastas, especificacao das
# consultas ao SIDRA e tabela de UFs). Nao precisa ser rodado sozinho: cada
# script o chama com source("code/config.R").
# ---------------------------------------------------------------------------

# Exige R >= 4.2 (UTF-8 nativo no Windows, para os acentos dos rotulos)
if (getRversion() < "4.2.0") {
  stop("Este projeto exige R 4.2 ou mais recente. Atualize o R em cran.r-project.org.")
}

# O projeto precisa estar aberto pelo grupo7.Rproj: todos os caminhos partem
# da pasta que contem esse arquivo, em qualquer computador.
if (!file.exists("grupo7.Rproj")) {
  stop("Abra o projeto pelo arquivo grupo7.Rproj (duplo clique) e rode de novo.")
}

suppressPackageStartupMessages(library(tidyverse))

# Pastas do projeto -----------------------------------------------------------
pastas <- c(bruto   = "data/bruto",      # o que vem do SIDRA, sem mexer
            dados   = "data",            # base limpa e consolidada
            tabelas = "output/tabelas",  # tabelas para o texto
            figs    = "figs")            # graficos
walk(pastas, dir.create, recursive = TRUE, showWarnings = FALSE)

# Recorte obrigatorio do roteiro (Secao 10.7) ----------------------------------
ANO <- 2017
TIPOLOGIA_AF <- "/c829/46304"   # c829 (Tipologia): "Agricultura familiar - sim"

# Uma linha por variavel do conjunto de dados principal (Tabela 9 do roteiro).
# classif: classificacao e categoria alem da Tipologia; as nao indicadas voltam
# na categoria Total.
especificacao <- tribble(
  ~nome,            ~tabela, ~variavel, ~classif,          ~descricao,
  "n_af",           6778,    183,       "",                "Numero de estabelecimentos da agricultura familiar",
  "energia",        6778,    183,       "/c309/3011",      "Estabelecimentos com energia eletrica (Tinham)",
  "proprietario",   6778,    183,       "/c218/46503",     "Estabelecimentos de produtor proprietario(a)",
  "cooperativa",    6846,    183,       "/c12598/113455",  "Estabelecimentos com produtor associado a cooperativa",
  "orientacao",     6779,    183,       "/c12567/113111",  "Estabelecimentos que recebem orientacao tecnica",
  "irrigacao",      6857,    2372,      "",                "Estabelecimentos com uso de irrigacao",
  "financiamento",  6895,    1990,      "",                "Estabelecimentos que obtiveram financiamento",
  "valor_producao", 6897,    1999,      "",                "Valor da producao"
)

# Grandes Regioes: o primeiro digito do codigo do IBGE. Ficam no script, e nao
# numa tabela baixada, porque sao rotulos administrativos estaveis.
REGIOES <- c("1" = "Norte", "2" = "Nordeste", "3" = "Sudeste",
             "4" = "Sul", "5" = "Centro-Oeste")

# UFs: os dois primeiros digitos do codigo da microrregiao
UFS <- tribble(
  ~cod_uf, ~uf,
  "11", "RO", "12", "AC", "13", "AM", "14", "RR", "15", "PA", "16", "AP", "17", "TO",
  "21", "MA", "22", "PI", "23", "CE", "24", "RN", "25", "PB", "26", "PE", "27", "AL",
  "28", "SE", "29", "BA",
  "31", "MG", "32", "ES", "33", "RJ", "35", "SP",
  "41", "PR", "42", "SC", "43", "RS",
  "50", "MS", "51", "MT", "52", "GO", "53", "DF"
)

# Rotulos das variaveis de analise, para tabelas e graficos
ROTULOS <- c(
  p_energia       = "Energia elétrica (%)",
  p_proprietario  = "Produtor proprietário (%)",
  p_cooperativa   = "Associado a cooperativa (%)",
  p_orientacao    = "Recebe orientação técnica (%)",
  p_irrigacao     = "Usa irrigação (%)",
  p_financiamento = "Obteve financiamento (%)",
  vp_estab        = "Valor da produção por estab. (mil R$)"
)
VARS_ANALISE <- names(ROTULOS)

theme_set(theme_minimal(base_size = 11))

# Grava um grafico em figs/ com tamanho fixo (o mesmo em qualquer computador)
salvar_fig <- function(grafico, nome, largura = 9, altura = 5.5) {
  ggsave(file.path(pastas["figs"], paste0(nome, ".png")), grafico,
         width = largura, height = altura, dpi = 300, bg = "white")
  invisible(grafico)
}

# Grava uma tabela em output/tabelas/ (CSV UTF-8, ponto decimal)
salvar_tab <- function(tabela, nome) {
  write_csv(tabela, file.path(pastas["tabelas"], paste0(nome, ".csv")))
  invisible(tabela)
}
