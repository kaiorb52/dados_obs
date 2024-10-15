
library(shiny)
library(leaflet)
library(shinythemes)
library(tidyr)

source("dados.R")

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  navbarPage(
    title = "Observatorio das Metropoles",
    tabPanel(
      title = "Fiscal", 
      sidebarLayout(
        sidebarPanel(
          selectInput("variavel", "Escolher indicador", choices = c(choice_var)),
          conditionalPanel(
            condition = "input.graph_tabs_fiscal == 'Grafico'",
            selectInput("municipio_selected", "Municipio", choices = c(municipios)),
          ),
          conditionalPanel(
            condition = "input.graph_tabs_fiscal == 'Mapa'",
            sliderInput(
              label = "Ano",
              inputId = "ano_selected",
              value = median(anos),
              min = min(anos),
              max = max(anos)
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "graph_tabs_fiscal",
            tabPanel("Grafico",
              uiOutput("graph") 
            ),
            tabPanel("Mapa",
             leafletOutput("finbra_map")
            ),
            tabPanel("Tabela",
              DT::dataTableOutput("view_table")
            ),
          )
        )
      )
    ),
    tabPanel(
      title = "Demografia",
      sidebarLayout(
        sidebarPanel(
          selectInput("demo_var_selected", "Escolher indicador", choices = c(demo_vars)),
          selectInput("demo_ano_selected", "Ano", choices = c(2010, 2022)),
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Mapa",
              leafletOutput("demo_map")
            )
          )
        )
      )
    )
    # tabPanel(
    #   title = "Segurança pública",
    #   sidebarLayout(
    #     sidebarPanel(
    #     ),
    #     mainPanel(
    #       tabsetPanel(
    #         tabPanel("Mapa",
    #         )
    #       )
    #     )
    #   )
    # )
  )
)

server <- function(input, output, session) {
  
  output$graph <- renderUI({

    p <- dados_rj |>
      filter(municipio == input$municipio_selected) |> 
      bind_rows(mean_rj) |> 
      filter(!is.na(!!sym(input$variavel)), !!sym(input$variavel) != 0) |> 
      ggplot(aes(x = ano, y = !!sym(input$variavel), group = municipio, colour = municipio, linetype = municipio)) +
      geom_line(size = 1.5) +
      geom_point(size = 3.5) +
      labs(x = NULL, y = NULL) +
      ggthemes::theme_clean() +
      scale_color_manual(
        values = c(MetBrewer::met.brewer("Homer2", 2))
      )
    
    ggplotly(p) 
    
  })
  
  output$finbra_map <- renderLeaflet({
    dados_filtrados <- subset(geo_br_dados, ano == input$ano_selected)
    
    pal <- colorNumeric(palette = "RdYlGn", domain = dados_filtrados[[input$variavel]])
    
    leaflet(data = dados_filtrados) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) |>  # Mapa de fundo escuro
      addPolygons(
        fillColor = ~pal(dados_filtrados[[input$variavel]]), # Preenchimento baseado na variável
        fillOpacity = 1.0,
        weight = 0.2,
        smoothFactor = 0.2) %>%
      addLegend(
        pal = pal,
        values = dados_filtrados[[input$variavel]],
        position = "bottomright",
        title = input$variavel
      )  
  })
  
  output$view_table <- DT::renderDataTable({
    dados_rj |> 
      pivot_longer(cols = carga_tributaria:gasto_urbanos) |> 
      mutate(
        value = round(value, 2),
        value = ifelse(!is.na(value), yes = paste0(value, "%"), no = value)
      ) |> 
      pivot_wider(names_from = name, values_from = value)
    
  })
  
  output$demo_map <- renderLeaflet({
    
    censo_filtrado <- subset(dados_censo2, ano == input$demo_ano_selected)
    
    pal <- colorNumeric(palette = "RdYlGn", domain = censo_filtrado[[input$demo_var_selected]])
    
    leaflet(data = censo_filtrado) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) |>  # Mapa de fundo escuro
      addPolygons(
        fillColor = ~pal(censo_filtrado[[input$demo_var_selected]]), # Preenchimento baseado na variável
        fillOpacity = 1.0,
        weight = 0.2,
        smoothFactor = 0.2) %>%
      addLegend(
        pal = pal,
        values = censo_filtrado[[input$demo_var_selected]],
        position = "bottomright",
        title = input$demo_var_selected
    )
  })
  
}


shinyApp(ui = ui, server = server)
