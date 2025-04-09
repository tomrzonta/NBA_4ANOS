library(readxl)
library(tidyverse)
library(ggplot2)
library(dlookr)
library(dplyr)
library(purrr)
library(knitr)
library(summarytools)
library(GGally)
library(ggpubr)
library(mice)
library(corrplot)


# renv::init()
# renv::snapshot()

jogos_2024 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogos_2024.xlsx")

head(jogos_2024)

# Jogador, Time, Idade, Jogos_Jogados, Vitorias, Derrotas, Minutos_jogados,
# Pontos, Arremesso_certeiros, Arremessos_feitos, Porcentagem_arremessos_convertidos, 3Pontos_feitos,
# Tentativa_3pontos, Porcentagem_acertos3, Arrem.livres_feitos, Arrem.livres_tentados, Porcent_arrem.livres,
# Rebotes_ataque, Rebotes_defesa, Total_rebotes, Assistenc., Perder_posse, Roubos, Bloqueios, Faltas, 
# Pontos_fantasia, Dois_duplos, Tres_duplos, plus-minus


jogadores_2024 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogadores_2024.xlsx")

head(jogadores_2020)


str(jogos_2024)

names(jogos_2024)

jogos_2024 <- jogos_2024 %>%
  separate(Result, into = c("Placar_Casa", "Placar_Fora"), sep= " - ", convert =  TRUE)

colnames(jogos_2024) <- c("Numero_Round", "Data", "Local", "Time_Casa", "Visitante", "Placar_Casa", "Placar_Fora")

jogos_2024 <- jogos_2024 %>%
  mutate(
    Placar_Casa = as.numeric(Placar_Casa),
    Placar_Fora = as.numeric(Placar_Fora)
  )

estatisticas_times <- jogos_2024 %>%
  filter(!is.na(Placar_Casa) & !is.na(Placar_Fora)) %>%
  group_by(Time_Casa) %>%
  summarise(
    Media_Pontos_Casa = mean(Placar_Casa, na.rm = TRUE),
    Media_Pontos_Sofridos_Casa = mean(Placar_Fora, na.rm = TRUE)
  ) %>%
  rename(Time = Time_Casa) %>%
  full_join(
    jogos_2024 %>%
      filter(!is.na(Placar_Casa) & !is.na(Placar_Fora)) %>%
      group_by(Visitante) %>%
      summarise(
        Media_Pontos_Fora = mean(Placar_Fora, na.rm = TRUE),
        Media_Pontos_Sofridos_Fora = mean(Placar_Casa, na.rm = TRUE)
      ) %>%
      rename(Time =  Visitante),
    by = "Time"
  )

jogos_futuros_tmp <- jogos_2024 %>%
  filter(is.na(Placar_Casa) & is.na(Placar_Fora)) %>%
  left_join(estatisticas_times, by = c("Time_Casa" = "Time"))

colnames(jogos_futuros_tmp)


jogos_futuros_tmp2 <- jogos_futuros_tmp %>%
  left_join(estatisticas_times, by = c("Visitante" = "Time"))

colnames(jogos_futuros_tmp2)

colnames(estatisticas_times)

jogos_futuros <- jogos_futuros_tmp2 %>%
  rename(
    Media_Pontos_Casa_C = Media_Pontos_Casa.x,
    Media_Pontos_Sofridos_C = Media_Pontos_Sofridos_Casa.x,
    Media_Pontos_Fora_V = Media_Pontos_Fora.y,
    Media_Pontos_Sofridos_V = Media_Pontos_Sofridos_Fora.y
  ) %>%
  select(-Media_Pontos_Casa.y, -Media_Pontos_Sofridos_Casa.y, -Media_Pontos_Fora.x, -Media_Pontos_Sofridos_Fora.x)


colnames(jogos_futuros)



estatisticas_times <- estatisticas_times %>%
  mutate(
    Forca_Ofensiva = (Media_Pontos_Casa + Media_Pontos_Fora) / 2,
    Forca_Defensiva = (Media_Pontos_Sofridos_Casa + Media_Pontos_Sofridos_Fora) / 2
  )


#

jogos_futuros <- jogos_futuros %>%
  mutate(
    Pontos_Esperados_Casa = (Media_Pontos_Casa_C + Media_Pontos_Sofridos_V) / 2,
    Pontos_Esperados_Fora = (Media_Pontos_Fora_V + Media_Pontos_Sofridos_C) / 2
  )


jogos_futuros <- jogos_futuros %>%
  mutate(
    Prob_Vitoria_Casa = Pontos_Esperados_Casa / (Pontos_Esperados_Casa + Pontos_Esperados_Fora),
    Prob_Vitoria_Visitante = 1 - Prob_Vitoria_Casa
  )



jogos_futuros %>%
  select(Time_Casa, Visitante, Prob_Vitoria_Casa, Prob_Vitoria_Visitante) %>%
  arrange(desc(Prob_Vitoria_Casa))



jogos_futuros <- jogos_futuros %>%
  mutate(
    Placar_Estimado_Casa = round(Media_Pontos_Casa_C),
    Placar_Estimado_Fora = round(Media_Pontos_Fora_V)
  )



tabela_resultados_previstos <- jogos_futuros %>%
  select(Time_Casa, Visitante, Prob_Vitoria_Casa, Prob_Vitoria_Visitante, Placar_Estimado_Casa, Placar_Estimado_Fora)




tabela_resultados_previstos

write.csv(tabela_resultados_previstos, "resultado_previstos_att.csv", row.names = FALSE)


jogos_futuros %>% tail()

################################################################################
#################INÍCIO DA ANÁLISE DE DADOS DOS JOGADORES#######################
################################################################################


colnames(jogadores_2024) <- c(
  "ID","Jogador", "Time", "Idade", "Jogos_Jogados", "Vitorias", "Derrotas", "Minutos_Jogados",
  "Pontos", "Arremessos_Certos", "Arremessos_Totais", "Porcentagem_Acerto_Arremesso",
  "Tres_Pontos_Certos", "Tres_Pontos_Tentados", "Porcentagem_Acerto_Tres",
  "Lances_Livres_Certos", "Lances_Livres_Tentados", "Porcentagem_Acerto_Lance_Livre",
  "Rebotes_Ofensivos", "Rebotes_Defensivos", "Rebotes_Totais", "Assistencias",
  "Perdas_Posse", "Roubos", "Bloqueios", "Faltas", "Pontos_Fantasia",
  "Duplos_Duplos", "Triplos_Duplos", "PlusMinus"
)


descr(jogadores_2024$Pontos)

ggpairs(jogadores_2024[, c("Pontos","Assistencias","Rebotes_Totais", "Perdas_Posse")])


ggplot(jogadores_2024, aes(x = Pontos)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Distribuição de Pontos")


ggqqplot(jogadores_2024$Pontos)



shapiro.test(jogadores_2024$Pontos)

diagnose(jogadores_2024)

imputacao <- mice(jogadores_2024, m= 1, method = "pmm", seed = 3010)

summary(imputacao)

jogadores_2024_imputado <- complete(imputacao)


colunas <- c("Pontos", "Assistencias", "Rebotes_Ofensivos", "Rebotes_Defensivos", "Idade")

for (n in colunas) {
  print(
    ggplot(jogadores_2024_imputado, aes_string(x=n))+
      geom_histogram(fill= "steelblue", color = "white", bins = 30) +
      labs(title = paste("Histograma de", n), x = n, y = "Frequência")
  )
}



ggqqplot(jogadores_2024_imputado$Pontos, title = "Q-Q Plot - Pontos")



shapiro.test(jogadores_2024_imputado$Pontos)
shapiro.test(jogadores_2024_imputado$Assistencias)


diagnose(jogadores_2024) %>%
  select(variables, missing_count, missing_percent)


outliers <- diagnose_outlier(jogadores_2024_imputado)

head(outliers)


outliers %>%
  filter(outliers_cnt > 0)


head(jogadores_2024)

correlacao <- jogadores_2024_imputado %>%
  select(Pontos, Assistencias, Rebotes_Totais, Roubos, Bloqueios, Faltas, Perdas_Posse)

cor_matrix <- cor(correlacao, use = "complete.obs")
corrplot(cor_matrix, method = "number" )


anova_times <- aov(Pontos ~ Time, data = jogadores_2024_imputado)
summary(anova_times)


