# HO-341 · Trabalho final · Grupo 7

**Heterogeneidade da agricultura familiar** — microrregiões brasileiras, Censo
Agropecuário 2017 (IBGE), restrito aos estabelecimentos da agricultura familiar
(Tipologia, categoria 46304).

## Como rodar (em qualquer computador)

1. Tenha o **R 4.2 ou mais recente** e o RStudio.
2. **Abra o projeto** com duplo clique em `grupo7.Rproj`, na pasta principal (e
   não os scripts soltos). Todos os caminhos partem dessa pasta.
3. Rode tudo, de cima a baixo, por **um** dos dois caminhos abaixo. Os dois
   executam o mesmo código e gravam os mesmos resultados.

   **a)Arquivo único:** abra `code/Cod_inteiro_G7_versaoparcial.R` e rode de cima
   a baixo. Ele reúne os scripts 00 a 04 num só arquivo, mas **continua
   dependendo** de `code/config.R` e da pasta do projeto: não funciona sozinho,
   fora da pasta. 

   **b)  Scripts separados (recomendado para conferir cada etapa):**

   ```r
   source("code/rodar_tudo.R")
   ```

   ou abra cada script de `code/` na ordem (00 a 04) e rode com `Ctrl+Enter`.

Os dados já baixados estão em `data/bruto/`, e a coleta **não baixa de novo**
(`ATUALIZAR <- FALSE`): todos do grupo, e quem for conferir o trabalho, usam os
mesmos números da extração registrada em `data/bruto/extracao.csv`. Para baixar
outra vez do SIDRA, mude para `ATUALIZAR <- TRUE` em `code/01_coleta.R` (ou no
arquivo único). Isso substitui os dados e a data de extração, que é citada no
texto.

**Compartilhe a pasta inteira, com `data/bruto/` dentro.** Assim quem receber
não depende da internet nem do servidor do IBGE para reproduzir os resultados.
Não inclua `.RData` nem `.Rhistory`.

## Os scripts

| Script | O que faz | O que grava |
|---|---|---|
| `00_instalar_pacotes.R` | instala `tidyverse`, `sidrar` (≥ 0.5.1) e `scales` | — |
| `config.R` | pastas, especificação das consultas, UFs e rótulos (chamado pelos outros) | — |
| `01_coleta.R` | consulta o SIDRA pelo `sidrar`, por Grande Região, tudo como texto | `data/bruto/*.csv`, `data/bruto/extracao.csv` |
| `02_limpeza.R` | verificações de leitura, sinais, duplicatas, unidades, identificador e ausentes; construção das proporções | `data/microrregioes_af_2017.*`, `output/tabelas/limpeza_*.csv` |
| `03_descritiva_bivariada.R` | contexto, descritivas e análise bivariada | `output/tabelas/A_*, B_*, C_*`, `figs/*.png`, `output/sessao_R.txt` |
| `04_metadados.R` | confere, na API de agregados do IBGE, o nome de cada tabela, variável, unidade e categoria pedida | `output/tabelas/metadados_tabelas.csv` |
| `rodar_tudo.R` | roda os scripts 00 a 04 em sequência | — |
| `Cod_inteiro_G7_versaoparcial.R` | os scripts 00 a 04 num único arquivo (mesmo código) | os mesmos arquivos |

## Resultados gravados

| Arquivo | Conteúdo | Onde aparece no texto |
|---|---|---|
| `A_contexto_por_regiao.csv` | proporções agregadas por Grande Região | Tabela 1 |
| `A_media_x_agregado.csv` | média das microrregiões × agregado nacional | Seção 4 |
| `B_estatisticas_descritivas.csv` | n, média, DP, CV, quartis, assimetria | Tabela 2 |
| `B_mediana_por_regiao.csv` | mediana e IIQ por região | Seção 4 |
| `B_extremos_valor_producao.csv` | 10 maiores e 10 menores valores da produção | Seção 4 |
| `C1_correlacao_spearman.csv`, `C1_correlacao_pearson.csv` | matrizes de correlação (Pearson com o valor da produção em log) | Figura 1, Seção 5 |
| `C3_spearman_por_regiao.csv` | Spearman de cada fator com o valor da produção, por região | Tabela 3 |
| `C3_valores_criticos.csv` | valor crítico do Spearman a 5%, por região | nota da Tabela 3 |
| `C4_contingencia_regiao_quartil_vp.csv` | quartis do valor da produção × região (qui-quadrado e V de Cramér no console) | — |
| `figs/B3_caixas_por_regiao.png` | diagramas de caixa por região | Apêndice B |
| `figs/C1_mapa_correlacao.png` | mapa de calor do Spearman | Figura 1 |
| `figs/B1_histogramas.png`, `B2_valor_producao_log.png`, `C2_dispersao_fatores_valor.png` | histogramas e dispersões | reservados para o trabalho final |
| `output/sessao_R.txt` | versões do R e dos pacotes usados | referências (R e sidrar) |

## Variáveis (Tabela 9 do roteiro)

| Nome na base | Tabela | Parâmetros (além de c829/46304) |
|---|---|---|
| `n_af` | 6778 | v183 |
| `energia` | 6778 | v183; c309/3011 (Tinham) |
| `proprietario` | 6778 | v183; c218/46503 (Proprietário(a)) |
| `cooperativa` | 6846 | v183; c12598/113455 (Cooperativa) |
| `orientacao` | 6779 | v183; c12567/113111 (Recebe) |
| `irrigacao` | 6857 | v2372 |
| `financiamento` | 6895 | v1990 |
| `valor_producao` | 6897 | v1999 |

Na base limpa, `p_*` são porcentagens sobre `n_af` (0 a 100) e `vp_estab` é o
valor da produção por estabelecimento da agricultura familiar, em mil reais.

## Decisões de limpeza (declaradas na entrega parcial)

- **Sinais do IBGE:** `-` vira 0 (zero absoluto); `X` (sigilo), `..` e `...`
  viram `NA`. Por isso a coleta pede `value_type = "character"`: lido como
  número, o `-` viraria `NA` sem aviso.
- **Ausentes:** ficam como `NA`, sem imputação; cada análise informa quantas
  microrregiões usou (`output/tabelas/limpeza_7_*.csv`).
- **Identificador:** código da microrregião do IBGE, 5 dígitos, como texto. O
  nome não é usado para cruzar.
- **Unidades:** contagens em "Unidades"; o valor da produção é convertido para
  mil reais conforme a unidade publicada.
- **Proporção agregada × média das microrregiões:** são coisas diferentes; as
  duas estão em `A_media_x_agregado.csv`.

## Se algo der errado

- *"Abra o projeto pelo arquivo grupo7.Rproj"*: o RStudio não está na pasta do
  projeto. Feche e abra pelo `grupo7.Rproj` da pasta principal.
- *Objeto não encontrado*: `Session > Restart R` e rode de cima a baixo.
- *A coleta falha* (só acontece com `ATUALIZAR <- TRUE`): o servidor do SIDRA
  pode estar lento. O script tenta de novo sozinho; se persistir, tente mais
  tarde.
- *Erro de certificado SSL no `04_metadados.R`*: abra no navegador a URL que o
  script mostra no console; o conteúdo é o mesmo.
