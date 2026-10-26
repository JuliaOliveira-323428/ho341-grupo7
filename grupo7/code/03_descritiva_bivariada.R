# ---------------------------------------------------------------------------
# HO-341 - Grupo 7: Heterogeneidade da agricultura familiar
# 03_descritiva_bivariada.R - estatisticas descritivas e analise bivariada
# ---------------------------------------------------------------------------
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
# =============================================================================
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
# O que olhar: a media das microrregioes da o mesmo peso a uma micro com 300
# estabelecimentos e a outra com 30 mil. Quando as duas colunas diferem muito,
# as microrregioes com muitos estabelecimentos tem perfil diferente das demais.

# =============================================================================
# B. ESTATISTICAS DESCRITIVAS (unidade: microrregiao)
# =============================================================================
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
# O que olhar: o coeficiente de variacao compara a dispersao entre variaveis de
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
# O que olhar: se a diferenca entre regioes e maior que a diferenca dentro de
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
# =============================================================================
# C1. Matrizes de correlacao. Pearson mede associacao LINEAR e e sensivel a
# valores extremos; por isso o valor da producao entra em log. Spearman usa
# postos: mede associacao monotona e nao muda com o log.
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
# O que olhar: quais fatores andam juntos (eles medem uma mesma "capacitacao"?
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
# O que olhar: se as microrregioes do quartil de maior valor se concentram em
# poucas regioes. Celulas com frequencia esperada abaixo de 5 enfraquecem o
# qui-quadrado (o R avisa).

# Registro do ambiente, para quem rodar de novo saber com que versoes rodou
writeLines(capture.output(sessionInfo()), file.path("output", "sessao_R.txt"))
message("Pronto. Tabelas em output/tabelas/ e graficos em figs/.")

