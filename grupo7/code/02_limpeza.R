# ---------------------------------------------------------------------------
# HO-341 - Grupo 7: Heterogeneidade da agricultura familiar
# 02_limpeza.R - das tabelas brutas do SIDRA a base consolidada
#
# ---------------------------------------------------------------------------
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
# =============================================================================
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
# esta errada, e e melhor parar aqui do que seguir com NA silencioso.
nao_convertidos <- longo |>
  filter(is.na(valor), !is.na(valor_txt), !valor_txt %in% SINAIS_AUSENCIA)
if (nrow(nao_convertidos) > 0) {
  print(nao_convertidos)
  stop("Ha valores que nao sao numero nem sinal do IBGE (acima).")
}

# =============================================================================
# 3. DUPLICATAS
# =============================================================================
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
# =============================================================================
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
# =============================================================================
# O codigo da microrregiao do IBGE tem 5 digitos: 2 da UF + 3 da micro. Fica
# como texto (codigo nao e quantidade). O nome NAO serve para cruzar.
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
# =============================================================================
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
# O "-" do IBGE ja virou zero na etapa 2: e zero estrutural, nao ausente.

# =============================================================================
# 8. GRAVACAO
# =============================================================================
write_csv(micro, file.path(pastas["dados"], "microrregioes_af_2017.csv"))
saveRDS(micro, file.path(pastas["dados"], "microrregioes_af_2017.rds"))
# Versao para abrir no Excel em portugues (ponto e virgula, virgula decimal)
write_excel_csv2(micro, file.path(pastas["dados"], "microrregioes_af_2017_excel.csv"))

glimpse(micro)
message("Base consolidada: ", nrow(micro), " microrregioes. ",
        "Proximo passo: code/03_descritiva_bivariada.R")
