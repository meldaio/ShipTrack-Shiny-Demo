library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(leaflet)
library(dplyr)

source("processing.R")

memdata = reactiveValues(myship = NULL,  shipdata_pr = NULL, mapcirclesize = 6, rowindex = NULL)

if (!exists("shipdata")){
    shipdata <- load_raw_ship_data()
    print("yo")
}
if (!exists("shipdata_pr")){
    memdata$shipdata_pr <- load_processed_ship_data()
    print("yo2")
}

dropdownUI <- function(id,values) {
    dropdown_input(id, values, 
                   value = values[1], type = "search selection")
}

ui <- shinyUI(
    semanticPage(
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
            uiOutput("message_box"),
            cell_width = "270px",
            column_gap = "20px"
        ),
        flowLayout(
            box(color = "blue", ribbon = FALSE,
                leafletOutput("mymap")
            ), cell_width = "850px"
        ),
        flowLayout(
            uiOutput("numeric_input"),
            #sliderInput("slid", label = "s", min = 1, max=199, value = 1),
            box(
                tags$div("yo world"),
                #slider_input("slider_ex", 5, 0, 20, 1, class = "Labeled"),
            ), cell_width = "850px"
        ),
        dataTableOutput('table')
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
            addCircleMarkers(data = memdata$myship %>% select(LAT, LON))
    })
    output$simple_dropdown2 = renderUI({
        shipnames <- dplyr::filter(shipdata, ship_type==input$simple_dropdown1)  %>% 
            select(SHIPNAME)  %>%  pull() %>%  unique()
        dropdownUI("simple_dropdown2", shipnames)
    })
    
    observe({
        req(input$simple_dropdown2)
        memdata$myship = dplyr::filter(shipdata, SHIPNAME==input$simple_dropdown2) #%>% 
            #slice_sample(n = 100)
    })
    observe({
        req(memdata$myship)
        
        highspeed <- memdata$shipdata_pr %>% 
            filter(SHIPNAME==input$simple_dropdown2)
#browser()
        leafletProxy(mapId = "mymap", data = memdata$myship) %>%
            clearMarkers() %>%   
            addCircleMarkers(radius = memdata$mapcirclesize,
                             stroke = FALSE,
                             fillOpacity = 0.2,
                             group = "Raw data") %>% 
            addCircleMarkers(data = highspeed[c("LON","LAT")],
                             group = "Highest Speed : End",
                             color = "#FF0000") %>% 
            addCircleMarkers(lng = highspeed$LON_prev, lat = highspeed$LAT_prev,
                             group = "Highest Speed : Start",
                             color = "#00FF00") %>% 
            addLayersControl(
                #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                overlayGroups = c("Raw data", "Highest Speed : Start", "Highest Speed : End"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    
    output$message_box = renderUI({
        message_box(class = "blue",
                    header = "Note",
                    content = paste("Your choices -->",
                                    "Ship Type:",input$simple_dropdown1,
                                    "Ship Name:",input$simple_dropdown2))
    })
    
    output$table <- renderDataTable({
        req(input$simple_dropdown2)
        memdata$shipdata_pr %>% filter(SHIPNAME==input$simple_dropdown2)
    })
#browser()
    output$numeric_input <- renderUI({
        req(input$simple_dropdown2)
        x <- memdata$shipdata_pr %>% filter(SHIPNAME == input$simple_dropdown2)
        memdata$rowindex <- which(shipdata$DATETIME == x$DATETIME & shipdata$SHIPNAME == x$SHIPNAME)
        #numeric_input("id","label", value = as.numeric(memdata$rowindex), min=1, max=length(shipdata$DATETIME))
        shiny::sliderInput("inputId", label="label",min=1, max=length(shipdata$DATETIME), value=as.numeric(memdata$rowindex))
    })
    exportTestValues(test_df = {shipdata})
})

shinyApp(ui, server)