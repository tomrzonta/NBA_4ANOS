library(readxl)
library(tidyverse)
library(ggplot2)
library(dlookr)
library(dplyr)

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
# Pontos, Arremesso_cesta, 

jogadores_2020 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogadores_2020.xlsx")
jogadores_2021 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogadores_2021.xlsx")
jogadores_2022 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogadores_2022.xlsx")
jogadores_2023 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogadores_2023.xlsx")
jogadores_2024 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogadores_2024.xlsx")

head(jogadores_2020)


str(jogos_2024)

names(jogos_2024)
