library(shiny)
library(shiny.semantic)
library(leaflet)
library(dplyr)

memdata = reactiveValues(myship = NULL)

if (!exists("shipdata")){
    if (!file.exists("ships.RData")) {
        shipdata <- read.csv("ships.csv")
        save(shipdata, file = "ships.RData")
    } else {
        load("ships.RData")
    }
}

dropdownUI <- function(id,values){
    dropdown_input(id, values, 
                   value = values[1], type = "search selection")
}

ui <- shinyUI(semanticPage(
    header(title = "SHIP PROJECT", description = "Description", icon = "ship"),
    sidebar_layout(
        sidebar_panel(
            menu_header(icon("filter"), "SELECT A SHIP TYPE", is_item = FALSE),
            dropdownUI("simple_dropdown1",unique(shipdata$ship_type)),
            menu_header(icon("search"), "SELECT A SHIP NAME", is_item = FALSE),
            uiOutput("simple_dropdown2"),
            message_box(class = "blue", header = "Note", content = "text"),
            width = 4),
        main_panel(
            leafletOutput("mymap"),
            width = 3
        ),mirrored = TRUE
    ),theme = "solar")
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
        dropdownUI("simple_dropdown2",shipnames)
    })
    
    df_filtered <- reactive({
        req(input$simple_dropdown2)
        cbind(data[data$SHIPNAME == input$simple_dropdown2, ]$LAT[1], 
              data[data$SHIPNAME == input$simple_dropdown2, ]$LON[1])
    })

    observe({
        req(input$simple_dropdown2)
        memdata$myship = dplyr::filter(shipdata, SHIPNAME==input$simple_dropdown2) %>% slice_sample(n = 25)
    })
    observe({
        req(memdata$myship)
        leafletProxy(mapId = "mymap", data = memdata$myship) %>%
            clearMarkers() %>%   
            addCircleMarkers()
    })

})
shinyApp(ui, server)