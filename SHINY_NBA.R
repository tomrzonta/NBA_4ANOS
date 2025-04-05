library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)

# Carregar os dados
jogadores_2024 <- read_excel("C:/Users/Pichau/Documents/RSTUDIO/NBA/jogadores_2024.xlsx")

# Renomear colunas
colnames(jogadores_2024) <- c(
  "ID","Jogador", "Time", "Idade", "Jogos_Jogados", "Vitorias", "Derrotas", "Minutos_Jogados",
  "Pontos", "Arremessos_Certos", "Arremessos_Totais", "Porcentagem_Acerto_Arremesso",
  "Tres_Pontos_Certos", "Tres_Pontos_Tentados", "Porcentagem_Acerto_Tres",
  "Lances_Livres_Certos", "Lances_Livres_Tentados", "Porcentagem_Acerto_Lance_Livre",
  "Rebotes_Ofensivos", "Rebotes_Defensivos", "Rebotes_Totais", "Assistencias",
  "Perdas_Posse", "Roubos", "Bloqueios", "Faltas", "Pontos_Fantasia",
  "Duplos_Duplos", "Triplos_Duplos", "PlusMinus"
)

# Interface do usuário
ui <- fluidPage(
  titlePanel("Dashboard - Estatísticas dos Jogadores NBA 2024"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("jogador", "Selecione um Jogador:",
                  choices = unique(jogadores_2024$Jogador),
                  selected = unique(jogadores_2024$Jogador)[1]),
      
      selectInput("variavel", "Selecione a Variável para o Gráfico:",
                  choices = c("Pontos", "Assistencias", "Rebotes_Totais", "Perdas_Posse")),
      
      sliderInput("idade", "Filtrar por idade:",
                  min = min(jogadores_2024$Idade, na.rm = TRUE),
                  max = max(jogadores_2024$Idade, na.rm = TRUE),
                  value = c(min(jogadores_2024$Idade, na.rm = TRUE), max(jogadores_2024$Idade, na.rm = TRUE)))
    ),
    
    mainPanel(
      plotOutput("grafico")
    )
  )
)

# Lógica do servidor
server <- function(input, output) {
  
  dados_filtrados <- reactive({
    jogadores_2024 %>%
      filter(Jogador == input$jogador,
             Idade >= input$idade[1],
             Idade <= input$idade[2])
  })
  
  output$grafico <- renderPlot({
    dados <- dados_filtrados()
    
    validate(
      need(nrow(dados) > 0, "Nenhum dado encontrado para os filtros selecionados.")
    )
    
    ggplot(dados, aes_string(x = "Jogador", y = input$variavel)) +
      geom_col(width = 0.4, fill = "#0073C2FF") +
      theme_minimal() +
      labs(
        title = paste("Estatística de", input$variavel, "para o jogador selecionado"),
        x = NULL, y = input$variavel
      ) +
      theme(axis.text.x = element_text(angle = 0))
  })
}

# Rodar o app
shinyApp(ui = ui, server = server)