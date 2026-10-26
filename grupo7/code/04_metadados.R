# ---------------------------------------------------------------------------
# HO-341 - Grupo 7: Heterogeneidade da agricultura familiar
# 04_metadados.R - metadados das 8 tabelas, direto da API de agregados do IBGE
# ---------------------------------------------------------------------------
if (!file.exists("grupo7.Rproj") && file.exists("../grupo7.Rproj")) setwd("..")
source("code/config.R")

# Se der erro de certificado SSL (o servidor servicodados.ibge.gov.br as vezes
# falha nisso, veja o lab02), abra a mesma URL no navegador: o resultado e o
# mesmo, em formato de texto (JSON).
url_metadados <- function(tabela) {
  paste0("https://servicodados.ibge.gov.br/api/v3/agregados/", tabela, "/metadados")
}

# Categorias pedidas em cada linha da especificacao (Tipologia + a da variavel)
categorias_pedidas <- function(classif) {
  codigos <- str_match_all(paste0(TIPOLOGIA_AF, classif), "/c(\\d+)/(\\d+)")[[1]]
  tibble(classificacao = codigos[, 2], categoria = codigos[, 3])
}

ler_metadados <- function(nome, tabela, variavel, classif, ...) {
  message("[", nome, "] ", url_metadados(tabela))
  m <- jsonlite::fromJSON(url_metadados(tabela))

  var <- m$variaveis[m$variaveis$id == variavel, ]

  # Nome de cada classificacao e categoria filtrada
  cats <- categorias_pedidas(classif) |>
    pmap_chr(function(classificacao, categoria) {
      cl <- m$classificacoes[m$classificacoes$id == classificacao, ]
      if (nrow(cl) == 0) return(paste0("c", classificacao, ": NAO ENCONTRADA"))
      ct <- cl$categorias[[1]]
      nome_cat <- ct$nome[ct$id == categoria]
      if (length(nome_cat) == 0) nome_cat <- "CATEGORIA NAO ENCONTRADA"
      paste0(cl$nome, " = ", nome_cat)
    })

  tibble(
    nome, tabela,
    titulo_tabela   = m$nome,
    pesquisa        = m$pesquisa,
    assunto         = m$assunto,
    periodicidade   = paste(m$periodicidade$frequencia, m$periodicidade$inicio,
                            m$periodicidade$fim, sep = " / "),
    niveis_territ   = paste(unlist(m$nivelTerritorial), collapse = ","),
    variavel_nome   = if (nrow(var) == 1) var$nome else "VARIAVEL NAO ENCONTRADA",
    variavel_unid   = if (nrow(var) == 1) var$unidade else NA_character_,
    recorte         = paste(cats, collapse = "; "),
    url             = paste0("https://sidra.ibge.gov.br/tabela/", tabela)
  )
}

metadados <- especificacao |>
  select(nome, tabela, variavel, classif) |>
  pmap(ler_metadados) |>
  list_rbind()

print(metadados, width = Inf)
salvar_tab(metadados, "metadados_tabelas")
# O que conferir: se cada recorte traz o nome esperado (por exemplo,
# "Tipologia = Agricultura familiar - sim") e se N9 (microrregiao) esta entre
# os niveis territoriais. As notas de cada tabela ("Vide Notas" na pagina do
# SIDRA, link na coluna url) trazem as definicoes das variaveis.
