library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(readr)

# Interface do usuário
ui <- fluidPage(
  titlePanel("Análise de Imóveis - Curitiba e São José dos Pinhais"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("cidade", "Selecione a cidade:", 
                  choices = c("Todas", "Curitiba", "São José dos Pinhais"),
                  selected = "Todas"),
      sliderInput("preco", "Faixa de preço (R$):",
                  min = 0, max = 5000000, value = c(0, 5000000)),
      sliderInput("area", "Área (m²):",
                  min = 0, max = 500, value = c(0, 500)),
      selectInput("tipo", "Tipo de imóvel:",
                  choices = c("Todos", "apartamento", "casa", "terreno", "comercial"),
                  selected = "Todos"),
      sliderInput("quartos", "Número de quartos:",
                  min = 0, max = 10, value = c(0, 10)),
      sliderInput("banheiros", "Número de banheiros:",
                  min = 0, max = 10, value = c(0, 10)),
      sliderInput("garagens", "Número de vagas:",
                  min = 0, max = 10, value = c(0, 10)),
      checkboxInput("outliers", "Remover outliers de preço", value = TRUE),
      actionButton("atualizar", "Atualizar Dados")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráficos",
                 fluidRow(
                   column(6, plotlyOutput("grafico_preco_area")),
                   column(6, plotlyOutput("grafico_valor_m2"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("grafico_distribuicao_preco")),
                   column(6, plotlyOutput("grafico_tipo_quartos"))
                 ),
                 tabPanel("Mapa de Calor por Bairro",
                          plotlyOutput("mapa_calor_bairro")
                 ),
                 tabPanel("Tabela de Dados",
                          DTOutput("tabela_dados")
                 ),
                 tabPanel("Resumo Estatístico",
                          verbatimTextOutput("resumo_estatistico")
                 )
        )
      )
    )
  )
) 

