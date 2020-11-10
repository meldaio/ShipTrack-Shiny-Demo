library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(leaflet)
library(dplyr)
library(DT)

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
            box(color = "blue", 
                       menu_header(icon("filter"), "SELECT A SHIP TYPE", is_item = FALSE),
                       dropdownUI("simple_dropdown1",unique(shipdata$ship_type)),
            ),
            box(color = "blue",
                       menu_header(icon("search"), "SELECT A SHIP NAME", is_item = FALSE),
                       uiOutput("simple_dropdown2")
            ),
            cell_width = "285px",
            column_gap = "20px"
        ),
        sidebar_layout(
            sidebar_panel(width = 3,
                          box(color = "blue",
                              menu_header(icon("edit"), "CHANGE THE OBSERVATION", is_item = FALSE),
                              uiOutput("numeric_input")
                          ),
                          box(color = "blue",
                          uiOutput("information"),
                          uiOutput("message_box"),
                ),
            ), 
            main_panel(width = 6,
                box(color = "blue", ribbon = FALSE,
                    leafletOutput("mymap")
                )
            )
        ),
        flowLayout(
            box(color = "blue",
                action_button("eval","Show Processed Dataset",icon("down arrow icon"),class="blue"),
                conditionalPanel("input.eval && !output.hide_panel", uiOutput("table_output")),
            ), cell_width = NULL
        )
    )
)

server <- shinyServer(function(input, output) {
    
    output$mymap <- renderLeaflet({
        req(memdata$myship)
        
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addCircleMarkers(data = memdata$myship %>% select(LAT, LON))
    })
    output$simple_dropdown2 = renderUI({
        shipnames <- dplyr::filter(shipdata, ship_type==input$simple_dropdown1)  %>% 
            select(SHIPNAME)  %>%  pull() %>%  unique()
        dropdownUI("simple_dropdown2", shipnames)
    })
    
    observe({
        req(input$simple_dropdown2)
        memdata$myship = dplyr::filter(shipdata, SHIPNAME==input$simple_dropdown2) 
    })
    observe({
        req(memdata$myship)
        
        highspeed <- memdata$shipdata_pr %>% 
            filter(SHIPNAME==input$simple_dropdown2)

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
                overlayGroups = c("Raw data", "Highest Speed : Start", "Highest Speed : End"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    
    observe({
        req(input$simple_dropdown2)
        x <- memdata$shipdata_pr %>% filter(SHIPNAME == input$simple_dropdown2)
        
        output$message_box = renderUI({
            message_box(class = "blue",
                        header = NULL,
                        content = paste("The longest distance between two consecutive observations",
                                        round(abs(as.numeric(x$DATETIME_diff)/60)),
                                        "minutes apart is",round(as.numeric(x$dist2prev)),"meters."))
        })
        
        
    })
    
    output$table <- DT::renderDataTable({
        req(input$simple_dropdown2)
        datatable(memdata$shipdata_pr %>% filter(SHIPNAME==input$simple_dropdown2)
                , options = list(dom = 't'))
    })

    output$numeric_input <- renderUI({
        req(input$simple_dropdown2)
        x <- memdata$shipdata_pr %>% filter(SHIPNAME == input$simple_dropdown2)
        memdata$rowindex <- which(shipdata$DATETIME == x$DATETIME & shipdata$SHIPNAME == x$SHIPNAME)
        shiny::sliderInput("inputId", label = NULL, min = 1, max = length(shipdata$DATETIME),
                           value = as.numeric(memdata$rowindex))
    })
    output$hide_panel <- eventReactive(input$num_input, TRUE, ignoreInit = TRUE)
    
    outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
    
    output$table_output <- renderUI({
        dataTableOutput('table')
    })
    
    observe({
        req(memdata$myship)
        req(memdata$rowindex)
        ship_length <- unique(memdata$myship$LENGTH)
        ship_width <- unique(memdata$myship$WIDTH)
        ship_port <- unique(memdata$myship$PORT[1])
        ship_destination <- unique(memdata$myship$PORT[2])
        ship_speed <- unique(shipdata$SPEED[memdata$rowindex])
        ship_flag <- unique(memdata$myship$FLAG)
        
        output$information <- renderUI(
        message_box(header = paste("Ship Name:",input$simple_dropdown2),
                    class = "list message",
                    content = c(paste("Ship Type:",input$simple_dropdown1),
                                paste("Length:",ship_length,"meter"),
                                paste("Width:",ship_width,"meter"),
                                paste ("Current Port:",ship_port),                                
                                paste("Destination:",ship_destination),
                                paste ("Speed:",ship_speed),                                
                                paste("Flag:",ship_flag)
                                )
                    )
        )
    })
    
    exportTestValues(test_df = {shipdata},
                     test_processed_df = load_processed_ship_data())
})

shinyApp(ui, server)