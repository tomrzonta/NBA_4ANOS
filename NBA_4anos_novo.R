library(readxl)
library(tidyverse)
library(ggplot2)
library(dlookr)
library(dplyr)
library(purrr)

jogos_2020 <- read.csv("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogos_2020.csv")
jogos_2021 <- read.csv("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogos_2021.csv")
jogos_2022 <- read.csv("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogos_2022.csv")
jogos_2023 <- read.csv("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogos_2023.csv")
jogos_2024 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogos_2024.xlsx")

head(jogos_2020)
head(jogos_2021)
head(jogos_2022)
head(jogos_2023)

# Jogador, Time, Idade, Jogos_Jogados, Vitorias, Derrotas, Minutos_jogados,
# Pontos, Arremesso_certeiros, Arremessos_feitos, Porcentagem_arremessos_convertidos, 3Pontos_feitos,
# Tentativa_3pontos, Porcentagem_acertos3, Arrem.livres_feitos, Arrem.livres_tentados, Porcent_arrem.livres,
# Rebotes_ataque, Rebotes_defesa, Total_rebotes, Assistenc., Perder_posse, Roubos, Bloqueios, Faltas, 
# Pontos_fantasia, Dois_duplos, Tres_duplos, plus-minus

jogadores_2020 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogadores_2020.xlsx")
jogadores_2021 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogadores_2021.xlsx")
jogadores_2022 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogadores_2022.xlsx")
jogadores_2023 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogadores_2023.xlsx")
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

#colnames(jogos_futuros_tmp2)

#colnames(estatisticas_times)

jogos_futuros <- jogos_futuros_tmp2 %>%
  rename(
    Media_Pontos_Casa_H = Media_Pontos_Casa.x,
    Media_Pontos_Sofridos_H = Media_Pontos_Sofridos_Casa.x,
    Media_Pontos_Fora_A = Media_Pontos_Fora.y,
    Media_Pontos_Sofridos_A = Media_Pontos_Sofridos_Fora.y
  ) %>%
  select(-Media_Pontos_Casa.y, -Media_Pontos_Sofridos_Casa.y, -Media_Pontos_Fora.x, -Media_Pontos_Sofridos_Fora.x)


estatisticas_times <- estatisticas_times %>%
  mutate(
    Forca_Ofensiva = (Media_Pontos_Casa + Media_Pontos_Fora) / 2,
    Forca_Defensiva = (Media_Pontos_Sofridos_Casa + Media_Pontos_Sofridos_Fora) / 2
  )


jogos_futuros <- jogos_futuros %>%
  mutate(
    Pontos_Esperados_Casa = (Media_Pontos_Casa_H + Media_Pontos_Sofridos_A) / 2,
    Pontos_Esperados_Fora = (Media_Pontos_Fora_A + Media_Pontos_Sofridos_H) / 2
  )


jogos_futuros <- jogos_futuros %>%
  mutate(
    Prob_Vitoria_Casa = Pontos_Esperados_Casa / (Pontos_Esperados_Casa + Pontos_Esperados_Fora),
    Prob_Vitoria_Visitante = 1 - Prob_Vitoria_Casa
  )



jogos_futuros %>%
  select(Time_Casa, Visitante, Prob_Vitoria_Casa, Prob_Vitoria_Visitante) %>%
  arrange(desc(Prob_Vitoria_Casa))
