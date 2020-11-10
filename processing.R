library(geosphere)
library(dplyr)
library(readr)

getDBConnection <- function() {
  db <- dbConnect(SQLite(), dbname = "./ship.sqlite")
}

load_raw_ship_data <- function(){
  
  db <- getDBConnection()
  
  if ((length(dbListTables(db)==0))) {
    print("Reading raw ship data from csv file and creating SQLite DB with it")
    start_time <- Sys.time()
    shipdata <- read_csv("ships.csv") #%>% mutate(DATETIME=as.POSIXct(as.character(DATETIME)))
    end_time <- Sys.time()
    print(difftime(end_time, start_time, units="mins"))
    
    dbWriteTable(db, "shipdataraw", shipdata %>% select(-port))
    #saveRDS(shipdata, file = "ships.rds")
  } 

  print("Reading raw data from SQLite")
  #shipdata <- readRDS("ships.rds")
  start_time <- Sys.time()
  shipdata <- db %>% tbl("shipdataraw") %>% as_tibble() 
  end_time <- Sys.time()
  print(difftime(end_time, start_time, units="mins"))
  
  shipdata
}

load_shipmeta <- function(){
  if (!file.exists("shipmeta.rds")) {
    df <- load_raw_ship_data(db)
    
    out = list()
    out$shipnames_unique <- 
      df %>% select(SHIPNAME, ship_type) %>% distinct(SHIPNAME)
    out$shiptypes_unique <- 
      df %>% select(SHIPNAME, ship_type) %>% distinct(ship_type)
  
    saveRDS(out, file = "shipmeta.rds")
  } else {
    out = readRDS("shipmeta.rds")
  }
  out
}

process_raw_ship_data <- function(shipdata){
  # my_ship = "KAROLI"
  # my_ship = "TORNADO"
  # my_ship = "AURA"
  # my_ship = c("TORNADO","RONDO")
  #my_ship = shipdata %>% group_by(SHIPNAME) %>% tally(sort = T) %>% select(SHIPNAME) %>% pull() %>% as.character() %>% head(8)
  
  start_time <- Sys.time()
  sd1 <- shipdata %>% 
    #filter(SHIPNAME %in% my_ship) %>%
    group_by(SHIPNAME) %>% 
    mutate(DATETIME_prev = lag(DATETIME), LON_prev=lag(LON), LAT_prev=lag(LAT)) %>% 
    select(SHIPNAME, SPEED, LON, LAT, LON_prev, LAT_prev, DATETIME, DATETIME_prev) %>% 
    mutate(DATETIME_diff=DATETIME-DATETIME_prev) %>% 
    filter(!is.na(LON_prev))
  
  sd2 <- sd1 %>% rowwise() %>% 
    mutate(dist2prev=distGeo(c(LON,LAT), c(LON_prev, LAT_prev))) %>% 
    relocate(dist2prev, .before=DATETIME) %>% 
    group_by(SHIPNAME) %>% 
    arrange(desc(dist2prev)) %>% 
    slice_head(1)
  
  end_time <- Sys.time()
  print(difftime(end_time, start_time, units="mins"))
  
  saveRDS(sd2, file = "ships_processed.rds")
  sd2
}

load_processed_ship_data <- function(){
  if (!file.exists("ships_processed.rds")) {
    #shipdataP <- read_csv("ships.csv")  #%>% mutate(DATETIME=as.POSIXct(as.character(DATETIME)))
    shipdata <- load_raw_ship_data()
    shipdataP <- process_raw_ship_data(shipdata)
    saveRDS(shipdataP, file = "ships_processed.rds")
  } else {
    shipdataP <- readRDS("ships_processed.rds")
  }
  shipdataP
}
