
# Servidor
server <- function(input, output, session) {
  
  # Carregar dados do arquivo
  carregar_dados <- reactive({
    # Substitua pelo caminho completo do seu arquivo
    file_path <- "C:/Users/fiasi/OneDrive/DAY/Imoveis/all_cleaned_properties.csv"
    
    # Verificar se o arquivo existe
    if(!file.exists(file_path)) {
      showNotification("Arquivo não encontrado! Verifique o caminho.", type = "error")
      return(NULL)
    }
    
    tryCatch({
      df <- read_csv(file_path)
      
      # Atualizar os sliders com base nos dados reais
      updateSliderInput(session, "preco", 
                        max = max(df$preço, na.rm = TRUE) * 1.1,
                        value = c(0, max(df$preço, na.rm = TRUE) * 1.1))
      
      updateSliderInput(session, "area",
                        max = max(df$área, na.rm = TRUE) * 1.1,
                        value = c(0, max(df$área, na.rm = TRUE) * 1.1))
      
      return(df)
    }, error = function(e) {
      showNotification(paste("Erro ao carregar dados:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Dados filtrados
  dados_filtrados <- eventReactive(input$atualizar, {
    df <- carregar_dados()
    if(is.null(df)) return(NULL)
    
    # Filtrar dados conforme seleções do usuário
    if (input$cidade != "Todas") {
      df <- df %>% filter(cidade == input$cidade)
    }
    
    df <- df %>% 
      filter(preço >= input$preco[1] & preço <= input$preco[2],
             área >= input$area[1] & área <= input$area[2],
             quartos >= input$quartos[1] & quartos <= input$quartos[2],
             banheiros >= input$banheiros[1] & banheiros <= input$banheiros[2],
             garagens >= input$garagens[1] & garagens <= input$garagens[2])
    
    if (input$tipo != "Todos") {
      df <- df %>% filter(tipo == input$tipo)
    }
    
    # Remover outliers se selecionado
    if (input$outliers && nrow(df) > 0) {
      q1 <- quantile(df$preço, 0.25, na.rm = TRUE)
      q3 <- quantile(df$preço, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      df <- df %>% filter(preço >= (q1 - 1.5 * iqr) & preço <= (q3 + 1.5 * iqr))
    }
    
    df
  }, ignoreNULL = FALSE)
  
  # Gráfico de preço vs área
  output$grafico_preco_area <- renderPlotly({
    df <- dados_filtrados()
    if(is.null(df) || nrow(df) == 0) return(NULL)
    
    ggplotly(
      ggplot(df, aes(x = área, y = preço, color = tipo, text = paste("Bairro:", bairro, "<br>Preço: R$", preço, "<br>Área:", área, "m²"))) +
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Relação entre Área e Preço",
             x = "Área (m²)",
             y = "Preço (R$)",
             color = "Tipo") +
        theme_minimal()
    )
  })
  
  # Gráfico de valor por m² por bairro
  output$grafico_valor_m2 <- renderPlotly({
    df <- dados_filtrados()
    if(is.null(df) || nrow(df) == 0) return(NULL)
    
    dados_agrupados <- df %>%
      group_by(bairro, cidade) %>%
      summarise(media_valor_m2 = mean(valor_m², na.rm = TRUE), .groups = "drop")
    
    ggplotly(
      ggplot(dados_agrupados, aes(x = reorder(bairro, media_valor_m2), y = media_valor_m2, fill = cidade)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Valor Médio por m² por Bairro",
             x = "Bairro",
             y = "Valor médio por m² (R$)",
             fill = "Cidade") +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma)
    )
  })
  
  # Distribuição de preços
  output$grafico_distribuicao_preco <- renderPlotly({
    df <- dados_filtrados()
    if(is.null(df) || nrow(df) == 0) return(NULL)
    
    ggplotly(
      ggplot(df, aes(x = preço, fill = tipo)) +
        geom_histogram(bins = 20) +
        scale_x_continuous(labels = scales::comma) +
        labs(title = "Distribuição de Preços",
             x = "Preço (R$)",
             y = "Contagem",
             fill = "Tipo") +
        theme_minimal()
    )
  })
  
  # Relação entre tipo e quartos
  output$grafico_tipo_quartos <- renderPlotly({
    df <- dados_filtrados()
    if(is.null(df) || nrow(df) == 0) return(NULL)
    
    ggplotly(
      ggplot(df, aes(x = factor(quartos), y = preço, fill = tipo)) +
        geom_boxplot() +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Preço por Número de Quartos e Tipo",
             x = "Número de Quartos",
             y = "Preço (R$)",
             fill = "Tipo") +
        theme_minimal()
    )
  })
  
  # Mapa de calor por bairro
  output$mapa_calor_bairro <- renderPlotly({
    df <- dados_filtrados()
    if(is.null(df) || nrow(df) == 0) return(NULL)
    
    dados_agrupados <- df %>%
      group_by(bairro, cidade) %>%
      summarise(media_preco = mean(preço, na.rm = TRUE),
                contagem = n(),
                .groups = "drop")
    
    ggplotly(
      ggplot(dados_agrupados, aes(x = cidade, y = bairro, fill = media_preco, size = contagem)) +
        geom_point(shape = 22) +
        scale_fill_gradient(low = "blue", high = "red") +
        labs(title = "Mapa de Calor: Preço Médio por Bairro",
             x = "Cidade",
             y = "Bairro",
             fill = "Preço Médio",
             size = "Nº de Imóveis") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  # Tabela de dados
  output$tabela_dados <- renderDT{
    df <- dados_filtrados()
    #if(is.null(df) return(NULL)
       
       datatable(
         df %>% select(-link, -título) %>% mutate(link = paste0('<a href="', df$link, '" target="_blank">Ver imóvel</a>')),
         escape = FALSE,
         options = list(
           pageLength = 10,
           autoWidth = TRUE,
           scrollX = TRUE
         )
       )
  }
    
    # Resumo estatístico
    output$resumo_estatistico <- renderPrint({
      df <- dados_filtrados()
      if(is.null(df)) return("Dados não carregados")
      
      summary(df %>% select(área, preço, quartos, banheiros, garagens, valor_m²))
    })
}
