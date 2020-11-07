library(shiny)
library(shiny.semantic)
library(leaflet)
library(dplyr)

data <- read.csv("ships.csv", nrows = 50)
head(data)

ui <- shinyUI(semanticPage(
    header(title = "SHIP PROJECT", description = "Description", icon = "ship"),
    sidebar_layout(
        sidebar_panel(
            dropdown_input("simple_dropdown1", unique(data$ship_type), 
                           value = data$ship_type[1], type = "selection single"),
            uiOutput("simple_dropdown2"),
            width = 4),
        main_panel(
            leafletOutput("mymap"),
            width = 3
        ),mirrored = TRUE
    ),theme = "solar")
)
server <- shinyServer(function(input, output) {
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            )%>%
            #setView(lng = data$LON[1],lat = data$LAT[1], zoom = 10) %>%
            addMarkers(data = cbind(data$LAT[1],
                                    data$LON[1]))
    })
    output$simple_dropdown2 = renderUI({
        new <- filter(data, data$ship_type==input$simple_dropdown1) %>% as.data.frame()
        new <- new$SHIPNAME
        new <- unique(new)
        dropdown_input("simple_dropdown2", new, 
                       value = new[1], type = "selection single")
    })
    df_filtered <- reactive({
        cbind(data[data$SHIPNAME == input$simple_dropdown2, ]$LAT[1], 
              data[data$SHIPNAME == input$simple_dropdown2, ]$LON[1])
    })
    observe({
        
        leafletProxy(mapId = "mymap", data = df_filtered()) %>%
            clearMarkers() %>%   
            addMarkers()
    })

})
shinyApp(ui, server)
