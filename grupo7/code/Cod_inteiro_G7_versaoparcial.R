# ---------------------------------------------------------------------------
# HO-341 - Grupo 7 - pacotes do projeto. Rode UMA VEZ em cada computador.
# Caso queira que não fique baixando os dados mais de uma vez, troque TRUE por FALSE
# em "ATUALIZAR"
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


############COLETA DOS DADOS UTILIZADOS NO TRABALHO#######################
###### SÃO 8 variaveis do Censo Agropecuario 2017 no SIDRA

# ---------------------------------------------------------------------------
if (!file.exists("grupo7.Rproj") && file.exists("../grupo7.Rproj")) setwd("..")
source("code/config.R")

ATUALIZAR <- FALSE   #precisa deixar como "FALSE" pra não ficar baixando toda vez que rodar
### COLOQUE TRUE PRA GERAR NOVAMENTE 

options(sidrar.timeout = 300, sidrar.retries = 4)

# Antes de baixar, da para conferir variaveis!!! NA AULA LAB02 AJUDA A ARRUMAR ERRO SE PRECISAR
# informacao esta no site: sidra.ibge.gov.br/tabela/6778
# sidrar::info_sidra(6778)

# Monta a URL da consulta: microrregioes (n9) de uma Grande Regiao (n2) --------
montar_api <- function(tabela, variavel, classif, regiao) {
  paste0("/t/", tabela,
         "/n9/in%20n2%20", regiao,
         "/v/", variavel,
         "/p/", ANO,
         TIPOLOGIA_AF, classif)
}

# Uma consulta, com ate 3 tentativas extras (SUGESTÃO CASO CAIA O SIST.) -------
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
      mutate(across(everything(), as.character))   
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

# Roda a coleta ---------------
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
## OLHEM PRA CONFERIR AS VARIAVEIS 
if (file.exists(arquivo_registro)) {
  read_csv(arquivo_registro, show_col_types = FALSE) |>
    group_by(nome, tabela) |>
    summarise(linhas = sum(linhas), segundos = sum(segundos),
              consultado_em = max(consultado_em), .groups = "drop") |>
    print()
}

#################LIMPEZA DOS DADOS COLETADOS 

if (!file.exists("grupo7.Rproj") && file.exists("../grupo7.Rproj")) setwd("..")
source("code/config.R")

# =============================================================================
# 1. LEITURA DOS DADOS PRA FAZER A LIMPEZA 
# =============================================================================
# Tudo como texto: o "-" (zero) e o "X" (sigilo) sao informacao, e ler como
# numero os transformaria, sem aviso, no mesmo NA.
ler_bruto <- function(nome) {
  arquivo <- file.path(pastas["bruto"], paste0(nome, ".csv"))
  if (!file.exists(arquivo)) stop("Falta ", arquivo, ". Rode antes code/01_coleta.R.")
  read_csv(arquivo, col_types = cols(.default = col_character()))
}
brutos <- map(set_names(especificacao$nome), ler_bruto)

# As colunas do SIDRA tem acento e espaco; localizamos cada uma pelo comeco e
# pelo fim do nome, o que funciona com qualquer codificacao do sistema.
achar_coluna <- function(df, inicio, fim = "", sem_codigo = FALSE) {
  n <- names(df)
  termina <- if (fim == "") TRUE else str_ends(n, fixed(fim))
  col <- n[str_starts(n, fixed(inicio)) & termina &
             !(sem_codigo & str_ends(n, fixed("digo)")))]
  if (length(col) != 1) {
    stop("Coluna '", inicio, "...", fim, "' nao encontrada ou repetida. ",
         "Colunas do arquivo: ", paste(n, collapse = " | "))
  }
  col
}

# Formato longo padronizad,fiz uma linha por variável 
padronizar <- function(df, nome) {
  tibble(
    variavel     = nome,
    cod_micro    = df[[achar_coluna(df, "Microrregi", "digo)")]],
    nome_micro   = df[[achar_coluna(df, "Microrregi", sem_codigo = TRUE)]],
    cod_variavel = df[[achar_coluna(df, "Vari", "digo)")]],
    ano          = df[[achar_coluna(df, "Ano", "digo)")]],
    unidade      = df[["Unidade de Medida"]],
    valor_txt    = str_trim(df[["Valor"]])
  )
}
longo <- imap(brutos, padronizar) |> list_rbind()

# Conferencia da leitura: a variavel e o ano sao os pedidos?
conf_leitura <- longo |>
  group_by(variavel) |>
  summarise(linhas = n(),
            microrregioes = n_distinct(cod_micro),
            cod_variavel = paste(unique(cod_variavel), collapse = ","),
            ano = paste(unique(ano), collapse = ","),
            .groups = "drop") |>
  left_join(select(especificacao, variavel = nome, tabela, esperado = variavel),
            by = "variavel")
print(conf_leitura)
stopifnot("Codigo de variavel diferente do pedido" =
            all(conf_leitura$cod_variavel == as.character(conf_leitura$esperado)),
          "Ano diferente de 2017" = all(conf_leitura$ano == as.character(ANO)))
salvar_tab(conf_leitura, "limpeza_1_leitura")

# =============================================================================
# 2. CLASSES: de texto para numero, respeitando os sinais do IBGE
#   "-"          zero absoluto           -> 0
#   "X"          omitido por sigilo      -> NA
#   ".." e "..." nao se aplica / nao disponivel -> NA
SINAIS_AUSENCIA <- c("X", "..", "...", "")

sinais <- longo |>
  mutate(sinal = case_when(valor_txt == "-" ~ "- (zero)",
                           valor_txt == "X" ~ "X (sigilo)",
                           valor_txt %in% c("..", "...") ~ "... (nao disponivel)",
                           is.na(valor_txt) | valor_txt == "" ~ "vazio",
                           TRUE ~ "numero")) |>
  count(variavel, sinal) |>
  pivot_wider(names_from = sinal, values_from = n, values_fill = 0)
print(sinais)
salvar_tab(sinais, "limpeza_2_sinais_ibge")

longo <- longo |>
  mutate(valor = case_when(
    valor_txt == "-" ~ 0,
    is.na(valor_txt) | valor_txt %in% SINAIS_AUSENCIA ~ NA_real_,
    TRUE ~ suppressWarnings(as.numeric(valor_txt))   # a API usa ponto decimal
  ))

# Algum texto que nao e sinal do IBGE e nao virou numero? Entao a leitura
# esta errada, e e melhor parar aqui do que seguir com NA 
nao_convertidos <- longo |>
  filter(is.na(valor), !is.na(valor_txt), !valor_txt %in% SINAIS_AUSENCIA)
if (nrow(nao_convertidos) > 0) {
  print(nao_convertidos)
  stop("Ha valores que nao sao numero nem sinal do IBGE (acima).")
}

# =============================================================================
# 3. DUPLICATAS
repetidos <- longo |> count(variavel, cod_micro) |> filter(n > 1)
print(repetidos)   # esperado: nenhuma linha
if (nrow(repetidos) > 0) {
  # Linhas identicas saem; codigo repetido com valores diferentes, nao.
  longo <- distinct(longo)
  conflito <- longo |> count(variavel, cod_micro) |> filter(n > 1)
  if (nrow(conflito) > 0) {
    print(conflito)
    stop("Microrregiao repetida com valores diferentes: decidir e justificar.")
  }
}

# =============================================================================
# 4. UNIDADES
unidades <- longo |> count(variavel, unidade)
print(unidades)
salvar_tab(unidades, "limpeza_4_unidades")

# Contagens precisam estar em "Unidades". O valor da producao e convertido para
# mil reais seja qual for a unidade publicada (Reais ou Mil Reais).
stopifnot("Contagem fora de 'Unidades'" =
            all(filter(longo, variavel != "valor_producao")$unidade == "Unidades"))
un_vp <- unique(filter(longo, variavel == "valor_producao")$unidade)
fator_mil <- case_when(un_vp == "Mil Reais" ~ 1,
                       un_vp == "Reais"     ~ 1 / 1000,
                       TRUE                 ~ NA_real_)
if (length(un_vp) != 1 || is.na(fator_mil)) {
  stop("Unidade inesperada no valor da producao: ", paste(un_vp, collapse = ", "))
}
longo <- longo |>
  mutate(valor = if_else(variavel == "valor_producao", valor * fator_mil, valor))

# =============================================================================
# 5. IDENTIFICADOR
# O codigo da microrregiao do IBGE tem 5 digitos: 2 da UF + 3 da micro. Fica
# como texto (codigo nao e quantidade). O nome NAO serve para cruzar dados
stopifnot("Codigo de microrregiao fora do padrao de 5 digitos" =
            all(str_detect(longo$cod_micro, "^[0-9]{5}$")))

# Cada codigo tem o mesmo nome em todas as tabelas?
nomes_divergentes <- longo |>
  distinct(cod_micro, nome_micro) |>
  count(cod_micro) |>
  filter(n > 1)
print(nomes_divergentes)   # esperado: nenhuma linha

# A base parte do total de estabelecimentos da AF (o denominador); as outras
# variaveis entram por left_join pelo codigo.
base <- longo |>
  filter(variavel == "n_af") |>
  transmute(cod_micro, nome_micro, n_af = valor)

cobertura <- map(set_names(setdiff(especificacao$nome, "n_af")), function(v) {
  outra <- filter(longo, variavel == v)
  tibble(variavel = v,
         sobra_na_tabela = nrow(anti_join(outra, base, by = "cod_micro")),
         falta_na_tabela = nrow(anti_join(base, outra, by = "cod_micro")))
}) |> list_rbind()
print(cobertura)   # esperado: zeros
salvar_tab(cobertura, "limpeza_5_cobertura")

micro <- longo |>
  filter(variavel != "n_af") |>
  select(cod_micro, variavel, valor) |>
  pivot_wider(names_from = variavel, values_from = valor) |>
  right_join(base, by = "cod_micro") |>
  mutate(cod_uf = str_sub(cod_micro, 1, 2),
         regiao = factor(REGIOES[str_sub(cod_micro, 1, 1)], levels = REGIOES),
         nome_micro = str_remove(nome_micro, " - [A-Z]{2}$")) |>
  left_join(UFS, by = "cod_uf") |>
  relocate(cod_micro, nome_micro, uf, regiao, n_af) |>
  select(-cod_uf) |>
  arrange(cod_micro)

# Microrregioes por Grande Regiao. Pela divisao de 1989/1990 sao 558:
# Norte 64, Nordeste 188, Sudeste 160, Sul 94, Centro-Oeste 52.
print(count(micro, regiao))
if (nrow(micro) != 558) warning("Sao ", nrow(micro), " microrregioes, e nao 558: conferir.")

# =============================================================================
# 6. CONSTRUCAO DAS PROPORCOES E DA MEDIDA POR ESTABELECIMENTO
# Denominador: total de estabelecimentos da AF na microrregiao (roteiro, 10.7).
# Proporcoes em % (0 a 100). Valor da producao em mil R$ por estabelecimento.
micro <- micro |>
  mutate(n_af = if_else(n_af == 0, NA_real_, n_af),   # evita divisao por zero
         p_energia       = 100 * energia       / n_af,
         p_proprietario  = 100 * proprietario  / n_af,
         p_cooperativa   = 100 * cooperativa   / n_af,
         p_orientacao    = 100 * orientacao    / n_af,
         p_irrigacao     = 100 * irrigacao     / n_af,
         p_financiamento = 100 * financiamento / n_af,
         vp_estab        = valor_producao      / n_af)

# Uma proporcao acima de 100% indicaria numerador e denominador de universos
# diferentes (por exemplo, a Tipologia nao aplicada em uma das consultas).
acima_100 <- micro |>
  pivot_longer(starts_with("p_"), names_to = "indicador", values_to = "pct") |>
  filter(pct > 100)
print(acima_100)   # esperado: nenhuma linha
if (nrow(acima_100) > 0) stop("Proporcao acima de 100%: conferir as consultas.")

# =============================================================================
# 7. AUSENTES
# =============================================================================
ausentes <- tibble(coluna = names(micro), ausentes = colSums(is.na(micro))) |>
  mutate(em_pct = round(100 * ausentes / nrow(micro), 1))
print(ausentes)
salvar_tab(ausentes, "limpeza_7_ausentes")

linhas_com_ausente <- micro |>
  filter(if_any(all_of(VARS_ANALISE), is.na)) |>
  select(cod_micro, nome_micro, uf, n_af, all_of(VARS_ANALISE))
print(linhas_com_ausente)
salvar_tab(linhas_com_ausente, "limpeza_7_microrregioes_com_ausente")

# DECISAO (a declarar na entrega parcial): os ausentes por sigilo ("X") ficam
# como NA, sem imputacao, e cada analise informa quantas microrregioes usou.
# O "-" do IBGE ja virou zero na etapa de limpeza. e zero estrutural, nao ausente.

# =============================================================================
# 8. GRAVACAO tabs

write_csv(micro, file.path(pastas["dados"], "microrregioes_af_2017.csv"))
saveRDS(micro, file.path(pastas["dados"], "microrregioes_af_2017.rds"))
# Versao para abrir no Excel em portugues (ponto e virgula, virgula decimal)
write_excel_csv2(micro, file.path(pastas["dados"], "microrregioes_af_2017_excel.csv"))

glimpse(micro)

##############PARTE DA ANÁL. DESCRITIVA E BIVARIADA ############
if (!file.exists("grupo7.Rproj") && file.exists("../grupo7.Rproj")) setwd("..")
source("code/config.R")

arquivo_base <- file.path(pastas["dados"], "microrregioes_af_2017.rds")
if (!file.exists(arquivo_base)) stop("Falta a base limpa. Rode antes code/02_limpeza.R.")
micro <- readRDS(arquivo_base)

# Formato longo das variaveis de analise, usado em varios graficos
longo <- micro |>
  pivot_longer(all_of(VARS_ANALISE), names_to = "var", values_to = "valor") |>
  mutate(var = factor(var, levels = VARS_ANALISE, labels = ROTULOS))

# =============================================================================
# A. CONTEXTO: onde esta a agricultura familiar

contagens <- c("energia", "proprietario", "cooperativa", "orientacao",
               "irrigacao", "financiamento")

# Proporcao AGREGADA: soma dos numeradores / soma dos denominadores. E a
# proporcao do conjunto de estabelecimentos, e nao a media das microrregioes.
agregado <- function(dados) {
  dados |>
    summarise(microrregioes = n(),
              estab_af = sum(n_af, na.rm = TRUE),
              across(all_of(contagens),
                     ~ 100 * sum(.x, na.rm = TRUE) / sum(n_af[!is.na(.x)], na.rm = TRUE),
                     .names = "p_{.col}"),
              vp_estab = sum(valor_producao, na.rm = TRUE) /
                sum(n_af[!is.na(valor_producao)], na.rm = TRUE))
}
contexto <- bind_rows(
  micro |> group_by(regiao) |> agregado() |> mutate(regiao = as.character(regiao)),
  micro |> agregado() |> mutate(regiao = "Brasil")
) |>
  mutate(pct_estab_af = round(100 * estab_af / sum(estab_af[regiao != "Brasil"]), 1),
         .after = estab_af)
print(contexto, width = Inf)
salvar_tab(contexto, "A_contexto_por_regiao")

# Media das proporcoes das microrregioes x proporcao agregada do Brasil
media_x_agregado <- tibble(
  var = VARS_ANALISE,
  media_microrregioes = map_dbl(VARS_ANALISE, ~ mean(micro[[.x]], na.rm = TRUE)),
  agregado_brasil     = unlist(filter(contexto, regiao == "Brasil")[VARS_ANALISE])
) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(media_x_agregado)
salvar_tab(media_x_agregado, "A_media_x_agregado")
# O que precisamos olhar: a media das microrregioes da o mesmo peso a uma micro com 300
# estabelecimentos e a outra com 30 mil. Quando as duas colunas diferem muito,
# as microrregioes com muitos estabelecimentos tem perfil diferente das demais.

# =============================================================================
# B. ESTATISTICAS DESCRITIVAS (unidade: microrregiao)

assimetria <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^3) / sd(x)^3
}
descritivas <- longo |>
  group_by(var) |>
  summarise(n          = sum(!is.na(valor)),
            ausentes   = sum(is.na(valor)),
            media      = mean(valor, na.rm = TRUE),
            dp         = sd(valor, na.rm = TRUE),
            cv_pct     = 100 * dp / media,
            minimo     = min(valor, na.rm = TRUE),
            q1         = quantile(valor, 0.25, na.rm = TRUE),
            mediana    = median(valor, na.rm = TRUE),
            q3         = quantile(valor, 0.75, na.rm = TRUE),
            maximo     = max(valor, na.rm = TRUE),
            assimetria = assimetria(valor),
            .groups = "drop") |>
  mutate(across(where(is.numeric) & !c(n, ausentes), ~ round(.x, 2)))
print(descritivas, width = Inf)
salvar_tab(descritivas, "B_estatisticas_descritivas")
# O que precisamos olhar: o coeficiente de variacao compara a dispersao entre variaveis de
# niveis diferentes; a assimetria e a distancia media-mediana dizem se poucas
# microrregioes puxam a media (tipico de irrigacao e valor da producao).

# Mesmas medidas por Grande Regiao (mediana e intervalo interquartil)
por_regiao <- longo |>
  group_by(var, regiao) |>
  summarise(mediana = median(valor, na.rm = TRUE),
            iiq = IQR(valor, na.rm = TRUE),
            .groups = "drop") |>
  mutate(across(c(mediana, iiq), ~ round(.x, 1)))
salvar_tab(por_regiao, "B_mediana_por_regiao")
print(pivot_wider(select(por_regiao, -iiq), names_from = regiao, values_from = mediana))

# Distribuicoes: histograma, com tracos sob os valores atipicos (mais de 1,5
# intervalo interquartil alem dos quartis), como no laboratorio de limpeza
longo_at <- longo |>
  group_by(var) |>
  mutate(atipico = valor < quantile(valor, 0.25, na.rm = TRUE) - 1.5 * IQR(valor, na.rm = TRUE) |
           valor > quantile(valor, 0.75, na.rm = TRUE) + 1.5 * IQR(valor, na.rm = TRUE)) |>
  ungroup()

graf_hist <- ggplot(filter(longo_at, !is.na(valor)), aes(valor)) +
  geom_histogram(bins = 30, fill = "grey55", colour = "white") +
  geom_rug(data = filter(longo_at, atipico), length = unit(0.05, "npc")) +
  facet_wrap(vars(var), scales = "free", ncol = 4) +
  labs(x = NULL, y = "Microrregiões",
       caption = "Fonte: IBGE, Censo Agropecuário 2017. Traços: valores atípicos (regra 1,5 IIQ).")
salvar_fig(graf_hist, "B1_histogramas", largura = 11, altura = 6)

# O valor da producao e muito assimetrico: em escala log a forma aparece
graf_vp <- ggplot(filter(micro, !is.na(vp_estab)), aes(vp_estab)) +
  geom_histogram(bins = 30, fill = "grey55", colour = "white") +
  scale_x_log10(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(x = "Valor da produção por estabelecimento da AF (mil R$, escala log)",
       y = "Microrregiões", caption = "Fonte: IBGE, Censo Agropecuário 2017.")
salvar_fig(graf_vp, "B2_valor_producao_log", altura = 4.5)

# Diagramas de caixa por Grande Regiao
graf_box <- ggplot(filter(longo, !is.na(valor)), aes(regiao, valor)) +
  geom_boxplot(outlier.size = 0.8) +
  facet_wrap(vars(var), scales = "free_y", ncol = 4) +
  labs(x = NULL, y = NULL, caption = "Fonte: IBGE, Censo Agropecuário 2017.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
salvar_fig(graf_box, "B3_caixas_por_regiao", largura = 11, altura = 7)
# O que precisamos olhar: se a diferenca entre regioes e maior que a diferenca dentro de
# cada regiao. A heterogeneidade da AF e regional ou atravessa as regioes?

# Microrregioes nos extremos do valor da producao por estabelecimento
extremos <- bind_rows(
  slice_max(micro, vp_estab, n = 10) |> mutate(grupo = "10 maiores"),
  slice_min(micro, vp_estab, n = 10) |> mutate(grupo = "10 menores")
) |>
  select(grupo, nome_micro, uf, n_af, all_of(VARS_ANALISE)) |>
  mutate(across(all_of(VARS_ANALISE), ~ round(.x, 1)))
print(extremos, width = Inf)
salvar_tab(extremos, "B_extremos_valor_producao")

# =============================================================================
# C. ANALISE BIVARIADA
# C1. Matrizes de correlacao. Pearson mede associacao LINEAR e e sensivel a
# valores extremos; por isso o valor da producao entra em log. Spearman usa
# postos ele mede associacao monotona e nao muda com o log.
dados_cor <- micro |>
  select(all_of(VARS_ANALISE)) |>
  mutate(vp_estab = log(vp_estab))

cor_pearson  <- cor(dados_cor, use = "pairwise.complete.obs", method = "pearson")
cor_spearman <- cor(dados_cor, use = "pairwise.complete.obs", method = "spearman")
print(round(cor_pearson, 2))
print(round(cor_spearman, 2))

tab_cor <- function(m) {
  as_tibble(round(m, 3), rownames = "var") |>
    mutate(var = ROTULOS[var])
}
salvar_tab(tab_cor(cor_pearson),  "C1_correlacao_pearson")
salvar_tab(tab_cor(cor_spearman), "C1_correlacao_spearman")

# Mapa de calor da matriz de Spearman (triangulo inferior)
graf_cor <- as_tibble(cor_spearman, rownames = "v1") |>
  pivot_longer(-v1, names_to = "v2", values_to = "r") |>
  mutate(v1 = factor(v1, levels = VARS_ANALISE, labels = ROTULOS),
         v2 = factor(v2, levels = rev(VARS_ANALISE), labels = rev(ROTULOS))) |>
  filter(as.integer(v1) < length(VARS_ANALISE) + 1 - as.integer(v2)) |>
  ggplot(aes(v1, v2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = format(round(r, 2), decimal.mark = ",")), size = 3.5) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                       limits = c(-1, 1), name = "Spearman") +
  labs(x = NULL, y = NULL, caption = "Fonte: IBGE, Censo Agropecuário 2017.") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank())
salvar_fig(graf_cor, "C1_mapa_correlacao", largura = 8, altura = 6)
# O que olhar precisamos olhar: quais fatores andam juntos (eles medem uma mesma "capacitacao"?
# e o que a analise fatorial do trabalho final vai explorar) e quais andam em
# sentido oposto ou sao independentes dos demais.

# C2. Cada fator estrutural contra o valor da producao por estabelecimento
fatores <- setdiff(VARS_ANALISE, "vp_estab")
disp <- micro |>
  pivot_longer(all_of(fatores), names_to = "fator", values_to = "pct") |>
  mutate(fator = factor(fator, levels = fatores, labels = ROTULOS[fatores])) |>
  filter(!is.na(pct), !is.na(vp_estab))

graf_disp <- ggplot(disp, aes(pct, vp_estab)) +
  geom_point(aes(colour = regiao), alpha = 0.6, size = 1.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "black", linewidth = 0.6) +
  scale_y_log10(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  facet_wrap(vars(fator), scales = "free_x", ncol = 3) +
  labs(x = "% dos estabelecimentos da AF da microrregião",
       y = "Valor da produção por estab. (mil R$, log)", colour = NULL,
       caption = "Fonte: IBGE, Censo Agropecuário 2017. Reta: mínimos quadrados com o valor em log.") +
  theme(legend.position = "bottom")
salvar_fig(graf_disp, "C2_dispersao_fatores_valor", largura = 10, altura = 7)

# C3. A associacao vale dentro de cada regiao? (Spearman por Grande Regiao)
# Uma correlacao forte no Brasil que some dentro das regioes indica que ela
# vem das diferencas ENTRE regioes, e nao de uma relacao que vale em toda parte.
cor_regiao <- disp |>
  group_by(fator, regiao) |>
  summarise(rho = cor(pct, vp_estab, method = "spearman"), n = n(), .groups = "drop") |>
  bind_rows(disp |> group_by(fator) |>
              summarise(rho = cor(pct, vp_estab, method = "spearman"), n = n(),
                        .groups = "drop") |>
              mutate(regiao = "Brasil")) |>
  mutate(rho = round(rho, 2))
tab_cor_regiao <- cor_regiao |>
  select(-n) |>
  pivot_wider(names_from = regiao, values_from = rho)
print(tab_cor_regiao, width = Inf)
salvar_tab(tab_cor_regiao, "C3_spearman_por_regiao")

# Valor critico do Spearman a 5% (aproximacao pela t), por regiao. Usado na
# nota da Tabela 3: correlacoes abaixo desse valor nao diferem de zero.
r_critico <- function(n) { t <- qt(0.975, n - 2); t / sqrt(n - 2 + t^2) }
criticos <- cor_regiao |> distinct(regiao, n) |> mutate(r_critico = round(r_critico(n), 2))
print(criticos)
salvar_tab(criticos, "C3_valores_criticos")

# C4. Tabela de contingencia: quartis do valor da producao x Grande Regiao
contingencia <- micro |>
  filter(!is.na(vp_estab)) |>
  mutate(quartil_vp = cut(vp_estab, quantile(vp_estab, 0:4 / 4),
                          labels = c("Q1 (menor)", "Q2", "Q3", "Q4 (maior)"),
                          include.lowest = TRUE))
tab_cont <- table(contingencia$regiao, contingencia$quartil_vp)
print(tab_cont)
print(round(100 * prop.table(tab_cont, margin = 1), 1))   # % na linha
teste_qui2 <- chisq.test(tab_cont)
print(teste_qui2)
v_cramer <- sqrt(unname(teste_qui2$statistic) / (sum(tab_cont) * (min(dim(tab_cont)) - 1)))
message("V de Cramer: ", round(v_cramer, 3))
salvar_tab(as_tibble(as.data.frame.matrix(tab_cont), rownames = "regiao"),
           "C4_contingencia_regiao_quartil_vp")
# O que precisamos olhar: as microrregioes do quartil de maior valor se concentram em
# poucas regioes? Celulas com frequencia esperada abaixo de 5 enfraquecem o
# qui-quadrado

# Registro do ambiente, pra saber qual versão estamos trabalhando 
writeLines(capture.output(sessionInfo()), file.path("output", "sessao_R.txt"))
message("Pronto. Tabelas em output/tabelas/ e graficos em figs/.")


##########Dados adicionais só pra conseguir um contexto (metadados) puxado com API####

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




