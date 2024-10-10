library(shiny)
library(leaflet)
library(shinythemes)

setwd("/home/kaio/git/dados_obs")
source("src/dados.R")

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  
  navbarPage(
    title = "Observatorio das Metropoles",
    tabPanel(
      title = "Fiscais", 
      sidebarLayout(
        sidebarPanel(
          selectInput("variavel", "Escolher indicador", choices = c(choice_var)),
          selectInput("municipio_selected", "Municipio", choices = c(municipios)),
          sliderInput(
            label = "Ano",
            inputId = "ano_selected",
            value = median(anos),
            min = min(anos),
            max = max(anos)
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Grafico Longitudinal",
              uiOutput("graph")  # Corrigido para 'plotlyOutput'
            ),
            tabPanel("Mapa",
             leafletOutput("map")
            ),
            tabPanel("Tabela",
              DT::dataTableOutput("view_table")
            ),
          )
        )
      )
    ),
    tabPanel(
      title = "Demograficos"
    )
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
  
  output$map <- renderLeaflet({
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
  
}

library(tidyr)

shinyApp(ui = ui, server = server)
