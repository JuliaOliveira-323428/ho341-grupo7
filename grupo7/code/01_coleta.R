# ---------------------------------------------------------------------------
# HO-341 - Grupo 7: Heterogeneidade da agricultura familiar
# 01_coleta.R - coleta das 8 variaveis do Censo Agropecuario 2017 no SIDRA

# ---------------------------------------------------------------------------
if (!file.exists("grupo7.Rproj") && file.exists("../grupo7.Rproj")) setwd("..")
source("code/config.R")

ATUALIZAR <- FALSE   #precisa deixar como "FALSE" pra não ficar baixando toda vez que rodar
### COLOQUE TRUE PRA GERAR NOVAMENTE 
# Tempo maximo de cada tentativa e numero de tentativas do sidrar. As tabelas
# do Censo Agropecuario sao pesadas; por isso tambem consultamos por Grande
# Regiao (n2) e empilhamos, como pede a Secao 8 do roteiro.
options(sidrar.timeout = 300, sidrar.retries = 4)

# Antes de baixar, da para conferir variaveis, classificacoes e categorias de
# cada tabela (se der erro de certificado SSL, veja o lab02 do professor; a
# mesma informacao esta no site: sidra.ibge.gov.br/tabela/6778):
# sidrar::info_sidra(6778)

# Monta a URL da consulta: microrregioes (n9) de uma Grande Regiao (n2) --------
montar_api <- function(tabela, variavel, classif, regiao) {
  paste0("/t/", tabela,
         "/n9/in%20n2%20", regiao,
         "/v/", variavel,
         "/p/", ANO,
         TIPOLOGIA_AF, classif)
}

# Uma consulta, com ate 3 tentativas extras caso o servidor caia no meio -------
consultar <- function(api, tentativas = 3) {
  for (i in seq_len(tentativas)) {
    resultado <- tryCatch(
      suppressMessages(sidrar::get_sidra(api = api, value_type = "character")),
      error = function(e) e
    )
    if (!inherits(resultado, "error")) return(resultado)
    message("   falhou (", conditionMessage(resultado), "); nova tentativa em 15 s")
    Sys.sleep(15)
  }
  stop("A consulta ", api, " falhou ", tentativas, " vezes. ",
       "Verifique a internet ou tente mais tarde.")
}

# Coleta uma variavel: as 5 Grandes Regioes, empilhadas ------------------------
coletar_variavel <- function(nome, tabela, variavel, classif, ...) {
  arquivo <- file.path(pastas["bruto"], paste0(nome, ".csv"))
  if (file.exists(arquivo) && !ATUALIZAR) {
    message("[", nome, "] ja existe em ", arquivo, "; nao baixa de novo.")
    return(NULL)
  }

  partes <- list()
  registro <- list()
  for (regiao in names(REGIOES)) {
    api <- montar_api(tabela, variavel, classif, regiao)
    message("[", nome, "] ", REGIOES[regiao], ": ", api)
    inicio <- Sys.time()
    bruto <- consultar(api)
    segundos <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 1)

    partes[[regiao]] <- bruto |>
      as_tibble() |>
      mutate(across(everything(), as.character))   # tudo texto: nada se perde
    registro[[regiao]] <- tibble(
      nome, tabela, variavel, classificacao = paste0(TIPOLOGIA_AF, classif),
      regiao = REGIOES[regiao], api, linhas = nrow(bruto), segundos,
      consultado_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      versao_sidrar = as.character(packageVersion("sidrar"))
    )
  }

  write_csv(bind_rows(partes), arquivo)
  bind_rows(registro)
}

# Roda a coleta ---------------------------------------------------------------
registro <- especificacao |>
  select(nome, tabela, variavel, classif) |>
  pmap(coletar_variavel) |>
  bind_rows()

# Procedencia: acrescenta as consultas novas ao registro que ja existir
arquivo_registro <- file.path(pastas["bruto"], "extracao.csv")
if (nrow(registro) > 0) {
  if (file.exists(arquivo_registro)) {
    anterior <- read_csv(arquivo_registro, col_types = cols(.default = "c")) |>
      filter(!nome %in% registro$nome)
    registro <- bind_rows(anterior, mutate(registro, across(everything(), as.character)))
  }
  write_csv(registro, arquivo_registro)
}

# Resumo do que esta em data/bruto/
if (file.exists(arquivo_registro)) {
  read_csv(arquivo_registro, show_col_types = FALSE) |>
    group_by(nome, tabela) |>
    summarise(linhas = sum(linhas), segundos = sum(segundos),
              consultado_em = max(consultado_em), .groups = "drop") |>
    print()
}
message("Coleta concluida. Proximo passo do código é abrir o: code/02_limpeza.R")
