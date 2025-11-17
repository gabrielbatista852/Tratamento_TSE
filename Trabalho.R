#Rodando os pacotes necessários

#install.packages("electionsBR")
library(electionsBR)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("showtext")
library(showtext)
#install.packages("gt")
library(gt)
#install.packages("janitor")
library(janitor)
#install.packages("margins")
library(margins)

#Importando a base de dados para seção de votação
df <- elections_tse(year = 2024,
                    type = "vote_section",
                    uf = "MG",
                    br_archive = TRUE)

#Filtrando a base para o cargo de vereador e para o município de Aimorés/MG
df_selecao <- df %>%
  select(CD_MUNICIPIO, NM_MUNICIPIO, DS_CARGO, NM_VOTAVEL, QT_VOTOS, 
         NM_LOCAL_VOTACAO, DS_LOCAL_VOTACAO_ENDERECO, ) %>%
  filter(NM_MUNICIPIO == "AIMORÉS",
         DS_CARGO == "Vereador")
head(df_selecao)

#Descobrindo os vereadores mais votados
df_maiores <- df_selecao %>%
  group_by(NM_VOTAVEL)%>%
  summarise(quantidade_votos = sum(QT_VOTOS, na.rm = TRUE)) %>%
              arrange(desc(quantidade_votos)) %>%
  mutate(participacao_votos = round((quantidade_votos / sum(quantidade_votos))*100,2))


#Monstrando os 8 candidatos com maiores números de votos
#Critério de seleção: Candidatos que conseguiram mais de 500 votos

#Transformando o Data Frame em tabela
df_maiores <- head(df_maiores, 8)
df_maiores %>%
  gt () %>%
  tab_header(title = md("**Ranking dos Votáveis em Aimorés-MG (2024)**"), subtitle = "Candidatos a Vereador") %>%
  cols_label(NM_VOTAVEL = "Nome", quantidade_votos = "Votos", participacao_votos = "(%)") %>%
  tab_source_note(
    source_note = md("**Fonte**: TSE (via ElectionsBR) | **Elaboração** : Gabriel Batista")
  )

#Criando o vetor na ordem dos mais votados
candidatos <- c("ANALDO GOMES DA SILVA", "GUSTAVO CALVAO CASER", "ELIAS PARENTE","WAGNER FERREIRA DE OLIVEIRA KNOBLAUCH", "MILTON SANTOS SIRES DE OLIVEIRA", "BENILDE MADEIRA","LUCIANO AFONSO CEZAR","ALVARO MIGUEL DE SOUZA")

#Criando uma outra coluna que identifica os mais votados
df_selecao <- df_selecao %>%
  mutate(MAIOR = ifelse(NM_VOTAVEL %in% candidatos, NM_VOTAVEL, "OUTROS"))

#Tranformando a coluna em fator
df_selecao$MAIOR <- factor(df_selecao$MAIOR,
                           levels = c(candidatos, "OUTROS"),
                                                 ordered = TRUE)
df_selecao <- df_selecao %>%
  group_by(NM_LOCAL_VOTACAO, MAIOR) %>%
  summarise(Votos = sum(QT_VOTOS), .groups = "drop")

showtext_auto()
font_add_google("Ubuntu","Ubuntu")

#Para fins estéticos, iremos adicionar a variável NM_LOCAL_VOTACAO em factor 
#ordenada pelas escolas com maiores percentuais de votos em "OUTROS"
df_tst <- df_selecao %>%
  group_by(NM_LOCAL_VOTACAO) %>%
  mutate(prop = Votos / sum(Votos)) %>%   
  filter(MAIOR == "OUTROS") %>%           
  arrange(desc(prop)) %>%                 
  pull(NM_LOCAL_VOTACAO)

#Transformando em Factor
df_selecao$NM_LOCAL_VOTACAO <- factor(df_selecao$NM_LOCAL_VOTACAO,
                                                levels = df_tst,
                                                ordered = TRUE)

#Gráfico que mostra a participação dos 8 candidatos com maior votos para cada
#escola
df_selecao %>%
  ggplot(aes(x = NM_LOCAL_VOTACAO, y = Votos, fill = MAIOR)) + 
  geom_bar(stat = "identity", position = "fill") + 
  theme_bw() + 
  geom_hline(yintercept = .5, linetype = "dashed", size = .5) +
  theme(legend.title = element_blank(),
        legend.text = element_text(family = "ubuntu", face="bold", color = "#000000", size = 10),
        plot.title = element_text(family = "ubuntu", face = "bold", size = 20),
        plot.subtitle = element_text(family = "ubuntu", face = "italic", size = 15),
        plot.caption = element_text(family = "ubuntu", face = "italic"),
        axis.title = element_text(size = 8),
        plot.margin = margin(t = 20, r = 8, b = 7, l = 5),
        legend.position = "right",
        panel.grid = element_blank()) +
  labs(x= '', y = "Proporção de votos",
       title = 'Distribuição Percentual de Votos dos Candidatos Mais Votados',
       subtitle = "Eleições Municipais de 2024 - Aimorés/MG",
       caption = "Elaboração: Gabriel Batista") + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "ANALDO GOMES DA SILVA" = "#1b9e77",
      "GUSTAVO CALVAO CASER" = "#d95f02",
      "ELIAS PARENTE" = "#7570b3",
      "WAGNER FERREIRA DE OLIVEIRA KNOBLAUCH" = "#e7298a",
      "MILTON SANTOS SIRES DE OLIVEIRA" = "#66a61e",
      "BENILDE MADEIRA" = "#e6ab02",
      "LUCIANO AFONSO CEZAR" = "#a6761d",
      "ALVARO MIGUEL DE SOUZA" = "#1f78b4",
      "OUTROS" = "grey70"
    )
  )

#Importando a base de dados para candidato
df_candidato <- elections_tse(year = 2024,
                              type = "candidate",
                              uf = "MG")
#Filtrando para o cargo de vereador e para a cidade de Aimorés/MG
df_candidato_selecao <- df_candidato %>%
  select(NM_UE, DS_CARGO, NM_CANDIDATO,
         SG_PARTIDO, NM_PARTIDO, DT_NASCIMENTO,
         DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL,
         DS_COR_RACA, DS_OCUPACAO,
         DS_SIT_TOT_TURNO, NM_COLIGACAO, DS_COMPOSICAO_COLIGACAO) %>%
  filter(NM_UE == "AIMORÉS",
         DS_CARGO == "VEREADOR")

#Vendo os resultados para a variável de grau de instrução
unique(df_candidato_selecao$DS_GRAU_INSTRUCAO)

#Tranformando a variável para grau de instrução em factor
df_candidato_selecao$DS_GRAU_INSTRUCAO <- factor(df_candidato_selecao$DS_GRAU_INSTRUCAO,
                   levels = c("LÊ E ESCREVE", "ENSINO FUNDAMENTAL INCOMPLETO", "ENSINO FUNDAMENTAL COMPLETO", "ENSINO MÉDIO INCOMPLETO",
                              "ENSINO MÉDIO COMPLETO", "SUPERIOR INCOMPLETO", "SUPERIOR COMPLETO"),
                   ordered = TRUE)

#Agrupando as observações em função das variáveis de genêro, grau de instrução,
#e cor
df_agrupado <- df_candidato_selecao %>%
  group_by(DS_GENERO, DS_GRAU_INSTRUCAO, DS_COR_RACA) %>%
  summarise(quantidade = n())%>%
  ungroup()

#Gráfico que mostra a relação entre as 3 variáveis supracitadas
df_agrupado %>% 
ggplot(aes(x = DS_GRAU_INSTRUCAO, y = quantidade, fill = DS_COR_RACA)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~DS_GENERO) +
  theme_bw() +
  scale_fill_manual(values = c("#FF6347", "#468284", "#6A5ACD")) + 
  theme(
    text = element_text(family = "Ubuntu"),
    plot.title = element_text(family = "ubuntu", face = "bold", size = 16),
    plot.subtitle = element_text(family = "ubuntu", face = "italic", size = 11),
    plot.caption = element_text(family = "ubuntu", face = "italic"),    
    axis.title = element_text(family = "Ubuntu", face = "bold", size = 12),
    axis.text = element_text(family = "Ubuntu", size = 10),
    legend.title = element_blank(),
    legend.text = element_text(family = "Ubuntu", size = 9),
    strip.text = element_text(family = "Ubuntu", face = "italic", size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(x = '', y = "Quantidade de Votáveis",
       title = "Quantidade de Votáveis por Sexo e Raça em Aimorés-MG (2024)",
       subtitle = "Candidatos a Vereador",
    caption = "Elaboração: Gabriel Batista"
  ) +
  coord_flip()

#Agrupando as observações em função das variáveis de genêro, nome do partido,
#e cor
df_partido <- df_candidato_selecao %>%
  group_by(DS_GENERO, NM_PARTIDO, DS_COR_RACA) %>%
  summarise(quantidade = n())%>%
  ungroup()

#Criando o gráfico que mostra a relação entre as 3 variáveis supracitadas
df_partido %>% 
  ggplot(aes(x = NM_PARTIDO, y = quantidade, fill = DS_COR_RACA)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~DS_GENERO) +
  theme_bw() +
  scale_fill_manual(values = c("#FF6347", "#468284", "#6A5ACD")) + # Cores personalizadas
  theme(
    text = element_text(family = "Ubuntu"),
    plot.title = element_text(family = "ubuntu", face = "bold", size = 16),
    plot.subtitle = element_text(family = "ubuntu", face = "italic", size = 11),
    plot.caption = element_text(family = "ubuntu", face = "italic"),    
    axis.title = element_text(family = "Ubuntu", face = "bold", size = 12),
    axis.text = element_text(family = "Ubuntu", size = 10),
    legend.title = element_blank(),
    legend.text = element_text(family = "Ubuntu", size = 9),
    strip.text = element_text(family = "Ubuntu", face = "italic", size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(x = '', y = "Quantidade de Votáveis",
       title = "Quantidade de Votáveis por Sexo e Raça em Aimorés-MG (2024)",
       subtitle = "Candidatos a Vereador",
       caption = "Elaboração: Gabriel Batista"
  ) +
  coord_flip()

#Transformando a variável de data de nascimento para o formato Date Time
df_candidato_selecao$DT_NASCIMENTO <- as.Date(df_candidato_selecao$DT_NASCIMENTO, format = "%d/%m/%Y")

#Confirmando a transformação
str(df_candidato_selecao$DT_NASCIMENTO)

#Criando uma nova variável para idade do candidato
df_candidato_selecao <- df_candidato_selecao %>%
  mutate(
    idade = as.integer(floor((Sys.Date() - as.Date(DT_NASCIMENTO)) / 365.25))
  )

#Agrupando as idades
df_candidato_selecao <- df_candidato_selecao %>%
  mutate(faixa_idade = cut(
    idade,
    breaks = c(seq(20, 80, by = 10), Inf),
    labels = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
    right = FALSE
  ))

#Gráfico de distribuição das idades dos candidatos a vereador
df_candidato_selecao %>%
  ggplot() + 
  geom_bar(aes(x = faixa_idade)) + theme_bw() + 
  theme(
    plot.title = element_text(family = "ubuntu", face = "bold", size = 16),
    plot.subtitle = element_text(family = "ubuntu", face = "italic", size = 11),
    plot.caption = element_text(family = "ubuntu", face = "italic"),
    axis.title = element_text(size = 12),
    plot.margin = margin(t = 20, r = 8, b = 7, l = 5),
    panel.grid = element_blank()
  ) +
  labs(title = "Distribuição da Idade dos Votáveis em Aimorés-MG (2024)",
       subtitle = "Candidatos a Vereador", x = "Faixa de Idade",
       y = "Frequência Absoluta",
       caption = "Elaboração: Gabriel Batista")

#Boxplot de idade com facetamento para sexo
df_candidato_selecao %>%
  ggplot (aes(x = "", y=idade)) +
  geom_violin(fill = NA, col ='red', trim = FALSE) +
  geom_boxplot() +
  facet_wrap(~DS_GENERO) +
  theme_bw() +
  labs (
    title = "Distribuição da Idade Por Sexo em Aimorés-MG (2024)",
    subtitle = "Candidatos a Vereador",
    x = "",
    y = "Idade",
    caption = "Elaboração: Gabriel Batista"
  ) +
  theme (
    text = element_text(family = "Ubuntu"),
    plot.title = element_text(family = "ubuntu", face = "bold", size = 16),
    plot.subtitle = element_text(family = "ubuntu", face = "italic", size = 11),
    plot.caption = element_text(family = "ubuntu", face = "italic"),
    axis.text.x = element_blank(),
    axis.title = element_text(family = "ubuntu", size = 12),
    plot.margin = margin(t = 20, r = 8, b = 7, l = 5),
    strip.text = element_text(family = "ubuntu", face = "italic"),
    panel.grid = element_blank())

#Construção das tabelas de contingência
names(df_candidato_selecao)
tabela <- df_candidato_selecao %>%
  tabyl(SG_PARTIDO, DS_GRAU_INSTRUCAO) %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front")

#Criando uma estética para a tabela de Partido X Grau de Instrução
tabela %>%
  gt() %>%
  opt_table_font(
    font = google_font("Ubuntu")
  ) %>%
  tab_header(title = md("**Partido dos Candidatos Por Grau de Instrução em Aimorés-MG (2024)**"), subtitle = "Candidatos a Vereador") %>%
  cols_label(SG_PARTIDO = "") %>%
  cols_label("ENSINO FUNDAMENTAL INCOMPLETO" = "Incompleto",
             "ENSINO FUNDAMENTAL COMPLETO" = "Completo",
             "ENSINO MÉDIO INCOMPLETO" = "Incompleto",
             "ENSINO MÉDIO COMPLETO" = "Completo",
             "SUPERIOR INCOMPLETO" = "Incompleto",
             "SUPERIOR COMPLETO" = "Completo",
             "LÊ E ESCREVE" = "Lê e Escreve"
             ) %>%
  tab_source_note(
    source_note = md("Fonte: TSE (via ElectionsBR) | Elaboração: Gabriel Batista")) %>%
  tab_style(
    style = cell_text(align = "right", size = px(11)),
    locations = cells_source_notes()
  ) %>%
  tab_spanner(
        label = "Ensino Fundamental",
        columns = starts_with("ENSINO FUNDAMENTAL")
      ) %>%
  tab_spanner(
    label = "Ensino Médio",
    columns = starts_with("ENSINO MÉDIO")
  ) %>%
  tab_spanner(
    label = "Ensino Superior",
    columns = starts_with("SUPERIOR")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = SG_PARTIDO)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = "Ensino Fundamental")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = "Ensino Médio")
  )%>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = "Ensino Superior")
  ) 
#Analisando os resultados para estado civil
unique(df_candidato_selecao$DS_ESTADO_CIVIL)
#Tranformando a variável de estado civil em factor
df_candidato_selecao$DS_ESTADO_CIVIL <- factor(df_candidato_selecao$DS_ESTADO_CIVIL,
                                                 levels = c("SOLTEIRO(A)", "CASADO(A)", "DIVORCIADO(A)", "VIÚVO(A)"),
                                                 ordered = TRUE)

#Criando a tabela de contingência para Partido X Estado Civil
tabela <- df_candidato_selecao %>%
  tabyl(SG_PARTIDO, DS_ESTADO_CIVIL) %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front")

#Estilizando a tabela
tabela %>%
  gt() %>%
  opt_table_font(
    font = google_font("Ubuntu")
  ) %>%
  tab_header(title = md("**Partido dos Candidatos Por Estado Civil em Aimorés-MG (2024)**"), subtitle = "Candidatos a Vereador") %>%
  cols_label(SG_PARTIDO = "") %>%
  cols_label("CASADO(A)" = "Casado(a)",
             "DIVORCIADO(A)" = "Divorciado(a)",
             "SOLTEIRO(A)" = "Solteiro(a)",
             "VIÚVO(A)" = "Viúvo(a)"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = SG_PARTIDO)
  ) %>%
  tab_source_note(
    source_note = md("Fonte: TSE (via ElectionsBR) | Elaboração: Gabriel Batista")) %>%
  tab_style(
    style = cell_text(align = "right", size = px(11)),
    locations = cells_source_notes()
  )

#Criando a tabela de contingência para Partido X Cor
tabela <- df_candidato_selecao %>%
  tabyl(SG_PARTIDO, DS_COR_RACA) %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front")

#Visualiazando os resultados para cor
unique(df_candidato_selecao$DS_COR_RACA)

#Estilizando a tabela
tabela %>%
  gt() %>%
  opt_table_font(
    font = google_font("Ubuntu")
  ) %>%
  tab_header(title = md("**Partido dos Candidatos Por Cor em Aimorés-MG (2024)**"), subtitle = "Candidatos a Vereador") %>%
  cols_label(SG_PARTIDO = "") %>%
  cols_label("BRANCA" = "Branco(a)",
             "PARDA" = "Pardo(a)",
             "PRETA" = "Preto(a)"
             ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = SG_PARTIDO)
  ) %>%
  tab_source_note(
    source_note = md("Fonte: TSE (via ElectionsBR) | Elaboração: Gabriel Batista")) %>%
  tab_style(
    style = cell_text(align = "right", size = px(11)),
    locations = cells_source_notes()
  )

#Transformações para o modelo Logit/Probit

#Vendo os resultados para a variável para grau de instrução
unique(df_candidato_selecao$DS_GRAU_INSTRUCAO)
unique(df_candidato_selecao$DS_SIT_TOT_TURNO)
unique(df_candidato_selecao$NM_COLIGACAO)

#Criando uma dummy para a variável grau de instrução, genêro, estado civil e cor
#idade, coligação e eleito
probit <- df_candidato_selecao %>%
  mutate(instrucao = ifelse(DS_GRAU_INSTRUCAO == "SUPERIOR COMPLETO", 1, 0),
         genero = ifelse(DS_GENERO == "MASCULINO", 1, 0),
         civil = ifelse(DS_ESTADO_CIVIL == "CASADO(A)", 1, 0),
         cor = ifelse(DS_COR_RACA == "BRANCA",1,0),
         idade_dummy = ifelse(idade > 40 & idade < 54, 1, 0),
         eleito = ifelse(
           DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA" | DS_SIT_TOT_TURNO == "ELEITO POR QP", 1, 0),
         coligacao = ifelse(NM_COLIGACAO == "FEDERAÇÃO", 1, 0)
         )

#Rodando o modelo Probit
probit_mod <- glm(eleito ~ instrucao + genero + civil + cor + idade_dummy,
                 data = probit,
                 family = binomial(link = "probit"))

#Visualizando os resultados
summary(probit_mod)
margins(probit_mod)
