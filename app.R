library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(leaflet)
library(dplyr)

source("processing.R")

memdata = reactiveValues(myship = NULL)

shipdata <- load_raw_ship_data()

dropdownUI <- function(id,values) {
    dropdown_input(id, values, 
                   value = values[1], type = "search selection")
}

dropdownServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    selected_type <- reactiveValues( s = input$simple_dropdown1)
    #a <- reactive({input$simple_dropdown1}) 
    return(selected_type$s)
    
  })
}
ui <- shinyUI(semanticPage(
    theme = "sandstone",
    header(title = "SHIP PROJECT", description = "Description", icon = "ship"),
    flowLayout(
      
      box(width = 10,
          color = "blue", ribbon = TRUE, 
          column(width = 10,
                 menu_header(icon("filter"), "SELECT A SHIP TYPE", is_item = FALSE),
                 dropdownUI("simple_dropdown1",unique(shipdata$ship_type)),
          )
      ),
      
      box(width = 10,
          color = "blue", ribbon = TRUE,
          column(width = 10,
                 menu_header(icon("search"), "SELECT A SHIP NAME", is_item = FALSE),
                 uiOutput("simple_dropdown2")
          )
      ),
      vertical_layout(
      textOutput("selected1"),
      theme_selector()
      ),
      cell_width = "250px",
      column_gap = "12px"
    ),
    
    sidebar_layout(
      
        sidebar_panel(
          message_box(class = "blue", header = "Note", content = "text"),
          ##SECOND ARGUMENT WILL BE THE PLOT   
          width = 4
          ),
        
        main_panel(
          leafletOutput("mymap")
        )
        
        ,mirrored = TRUE
    )
    #,theme = "solar"
    )
)

server <- shinyServer(function(input, output) {
    
    output$mymap <- renderLeaflet({
        req(memdata$myship)

        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            #setView(lng = shipdata$LON[1],lat = shipdata$LAT[1], zoom = 10) %>%
            #addMarkers(shipdata = cbind(shipdata$LAT[1], shipdata$LON[1]))
            addCircleMarkers(data = select(memdata$myship, LAT, LON))
    })
    
    output$simple_dropdown2 = renderUI({
        shipnames <- dplyr::filter(shipdata, ship_type==input$simple_dropdown1)  %>% 
            select(SHIPNAME)  %>%  pull() %>%  unique()
        dropdownUI("simple_dropdown2", shipnames)
    })
    
    df_filtered <- reactive({
        req(input$simple_dropdown2)
        cbind(data[data$SHIPNAME == input$simple_dropdown2, ]$LAT[1], 
              data[data$SHIPNAME == input$simple_dropdown2, ]$LON[1])
    })

    observe({
        req(input$simple_dropdown2)
        memdata$myship = dplyr::filter(shipdata, SHIPNAME==input$simple_dropdown2) %>% 
          slice_sample(n = 100)
    })
    observe({
        req(memdata$myship)
        leafletProxy(mapId = "mymap", data = memdata$myship) %>%
            clearMarkers() %>%   
            addCircleMarkers()
    })
    output$selected1 <- renderText(paste(as.character(dropdownServer("simple_dropdown1")),"selected"))
})

shinyApp(ui, server)