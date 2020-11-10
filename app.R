library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(leaflet)
library(dplyr)
library(RSQLite)
library(dbplyr)
library(DT)

source("processing.R")

memdata = reactiveValues(shipmeta = NULL, shipdetails = NULL, 
                         myshipraw = NULL,  shipdata_pr = NULL, 
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
        header(title = "SHIP PROJECT", description = NULL, icon = "ship"),
        flowLayout(
            box(color = "blue",
                menu_header(icon("filter"), "SELECT A SHIP TYPE", is_item = FALSE),
                uiOutput("simple_dropdown1")
            ),
            box(color = "blue",
                menu_header(icon("search"), "SELECT A SHIP NAME", is_item = FALSE),
                uiOutput("simple_dropdown2")
            ),
            cell_width = "48%",
            column_gap = "20px"
        ),
        
        sidebar_layout(
            sidebar_panel(width = 3,
                          box(color = "blue",
                          uiOutput("information"), tags$br(),
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
                conditionalPanel("input.eval && !output.hide_panel", 
                                 uiOutput("table_output")
                ),
            ), cell_width = NULL
        )
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
        isolate({
            shiptypes = unique(memdata$shipmeta$ship_type)
        })
        dropdownUI("simple_dropdown1", shiptypes)
    })
    
    output$simple_dropdown2 = renderUI({
        req(input$simple_dropdown1)

        isolate({
            shipnames = memdata$shipmeta %>% filter(ship_type==input$simple_dropdown1) %>% select(SHIPNAME) %>% pull()
        })
        dropdownUI("simple_dropdown2", shipnames)
    })

    observe({
        req(input$simple_dropdown2)
        
        db <- getDBConnection()
        selected_shipname <- input$simple_dropdown2 
        memdata$myshipraw <- db %>% tbl("shipdataraw") %>% filter(SHIPNAME==selected_shipname) %>% as_tibble()
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
                        content = paste("The longest distance between two consecutive observations is",
                                        round(as.numeric(x$dist2prev)),"meters. They are ",
                                        round(abs(as.numeric(x$DATETIME_diff)/60)),
                                        "minutes apart in time."
                        )
            )
        })
        
    })
    
    output$table_output <- renderUI({
        dataTableOutput('table')
    })

    output$table <- DT::renderDataTable({
        req(input$simple_dropdown2)
        
        row_idx = seq(memdata$rowindex-2, length.out = 5)
        inDF = memdata$myshipraw[row_idx,] %>% 
            select(-c(COURSE,ELAPSED,LENGTH,ROT,SHIPTYPE,SHIP_ID,WIDTH,L_FORE,W_LEFT,DWT,GT_SHIPTYPE,LEGEND))
        inDF$DATETIME = as.POSIXct(inDF$DATETIME, origin = "1970-01-01")
        datatable(inDF%>% mutate_if(is.numeric, round), options = list(dom = 't'))
    })

    observe({
        req(input$simple_dropdown2)
        x <- memdata$shipdata_pr %>% filter(SHIPNAME == input$simple_dropdown2)
        memdata$rowindex <- which(memdata$myshipraw$DATETIME == x$DATETIME & memdata$myshipraw$SHIPNAME == x$SHIPNAME)

        #take the most recent if there are two observations found
        isolate(memdata$rowindex <- tail(memdata$rowindex,1))
    })    

    output$hide_panel <- eventReactive(input$num_input, TRUE, ignoreInit = TRUE)
    
    outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
    
    observe({
        req(memdata$myshipraw)
        req(memdata$rowindex)

        memdata$shipdetails = list()
            isolate({
                memdata$shipdetails$ship_length <- unique(memdata$myshipraw$LENGTH)
                memdata$shipdetails$ship_width <- unique(memdata$myshipraw$WIDTH)
                memdata$shipdetails$ship_port <- unique(memdata$myshipraw$PORT[1])
                memdata$shipdetails$ship_destination <- unique(memdata$myshipraw$PORT[2])
                memdata$shipdetails$ship_speed <- unique(memdata$myshipraw$SPEED[memdata$rowindex])
                memdata$shipdetails$ship_flag <- unique(memdata$myshipraw$FLAG)
            })
    })
        
    output$information <- renderUI({
        req(memdata$shipdetails)
        req(input$simple_dropdown1)

        isolate(
            message_box(header = paste("Ship Name:",input$simple_dropdown2),
                        class = "list message",
                        content = c(paste("Ship Type:",input$simple_dropdown1),
                                    paste("Length:", memdata$shipdetails$ship_length[1], "meters"),
                                    paste("Width:", memdata$shipdetails$ship_width[1], "meters"),
                                    paste ("Current Port:", memdata$shipdetails$ship_port[1]),                                
                                    paste("Destination:", memdata$shipdetails$ship_destination[1]),
                                    paste ("Speed:", memdata$shipdetails$ship_speed[1]),
                                    paste("Flag:", memdata$shipdetails$ship_flag[1])
                        )
            )            
        )
    })
    
    exportTestValues(test_df = {shipdata},
                     test_processed_df = load_processed_ship_data())
})

shinyApp(ui, server)