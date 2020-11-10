library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(leaflet)
library(dplyr)
library(RSQLite)
library(dbplyr)

source("processing.R")

memdata = reactiveValues(shipmeta = NULL, myshipraw = NULL,  shipdata_pr = NULL, 
                         mapcirclesize = 6, rowindex = NULL)

print("Reading ship metadata off disk")
memdata$shipmeta <- load_shipmeta()
print("Reading processed ship data off disk")
memdata$shipdata_pr <- load_processed_ship_data()

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
                           uiOutput("simple_dropdown1")
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
            sliderInput("slid", label = "s", min = 1, max=199, value = 1),
            box(
                tags$div("yo world"),
            ), cell_width = "850px"
        ),
        dataTableOutput('table')
    )
)

server <- shinyServer(function(input, output) {
    
    output$mymap <- renderLeaflet({
        req(memdata$myshipraw)

        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(data = memdata$myshipraw %>% select(LAT, LON))
    })
    
    output$simple_dropdown1 <- renderUI({
        dropdownUI("simple_dropdown1", memdata$shipmeta$shiptypes_unique %>% pull())
    })
    
    output$simple_dropdown2 = renderUI({
        dropdownUI("simple_dropdown2", memdata$shipmeta$shipnames_unique %>% pull())
    })

    observe({
        req(input$simple_dropdown2)

        db <- getDBConnection()
        selected_shipname <- input$simple_dropdown2 
        memdata$myshipraw <- db %>% tbl("shipdataraw") %>% filter(SHIPNAME==selected_shipname) %>% as_tibble()
        #slice_sample(n = 100)
        dbDisconnect(db)
    })
    
    observe({
        req(memdata$myshipraw)

        highspeed <- memdata$shipdata_pr %>%
            filter(SHIPNAME==input$simple_dropdown2)

        leafletProxy(mapId = "mymap", data = memdata$myshipraw) %>%
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
        req(input$simple_dropdown2)

        message_box(class = "blue",
                    header = "Note",
                    content = paste("Your choices -->",
                                    "Ship Type:", isolate(input$simple_dropdown1),
                                    "Ship Name:", input$simple_dropdown2))
    })

    output$table <- renderDataTable({
        req(memdata$rowindex)
        #memdata$shipdata_pr %>% filter(SHIPNAME==input$simple_dropdown2)
        display_idx = seq(memdata$rowindex-2, length.out = 5)
        memdata$myshipraw[display_idx,] %>% select(LAT,LON,SPEED,COURSE,HEADING,DATETIME,is_parked)
    })

    output$numeric_input <- renderUI({
        req(input$simple_dropdown2)
        req(memdata$myshipraw$DATETIME)
        
        x <- memdata$shipdata_pr %>% filter(SHIPNAME == input$simple_dropdown2)
        memdata$rowindex <- which(memdata$myshipraw$DATETIME == x$DATETIME & memdata$myshipraw$SHIPNAME == x$SHIPNAME)
        shiny::sliderInput("inputId", label = "label", min = (memdata$myshipraw$DATETIME[1]),
                           max = tail((memdata$myshipraw$DATETIME),1),
                           value = (memdata$myshipraw$DATETIME[memdata$rowindex]))
    })
    exportTestValues(test_df = {shipdata},
                     test_processed_df = load_processed_ship_data())
})

shinyApp(ui, server)