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
  titlePanel("Dashboard - Estatísticas por Time - NBA 2024"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variavel", "Selecione a Variável para o Gráfico:",
                  choices = c("Pontos", "Assistencias", "Rebotes_Totais", "Perdas_Posse")),
      
      sliderInput("idade", "Filtrar por Idade:",
                  min = min(jogadores_2024$Idade, na.rm = TRUE),
                  max = max(jogadores_2024$Idade, na.rm = TRUE),
                  value = c(min(jogadores_2024$Idade, na.rm = TRUE), max(jogadores_2024$Idade, na.rm = TRUE))),
      
      selectInput("time_destaque", "Destacar Time (opcional):",
                  choices = c("Nenhum", sort(unique(jogadores_2024$Time))),
                  selected = "Nenhum")
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
      filter(Idade >= input$idade[1],
             Idade <= input$idade[2])
  })
  
  output$grafico <- renderPlot({
    dados <- dados_filtrados()
    
    validate(
      need(nrow(dados) > 0, "Nenhum dado encontrado para os filtros selecionados.")
    )
    
    # Agrupar por time e somar a variável selecionada
    dados_agrupados <- dados %>%
      group_by(Time) %>%
      summarise(Valor = sum(.data[[input$variavel]], na.rm = TRUE)) %>%
      mutate(Destaque = ifelse(Time == input$time_destaque, "Sim", "Não"))
    
    ggplot(dados_agrupados, aes(x = reorder(Time, Valor), y = Valor, fill = Destaque)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Sim" = "#E41A1C", "Não" = "#377EB8")) +
      theme_minimal() +
      labs(
        title = paste("Total de", input$variavel, "por Time"),
        x = "Time", y = input$variavel,
        fill = "Destaque"
      )
  })
}

# Rodar o app
shinyApp(ui = ui, server = server)