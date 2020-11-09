library(geosphere)
library(dplyr)

load_raw_ship_data <- function(){
  if (!file.exists("ships.rds")) {
    shipdata <- read.csv("ships.csv") %>% 
      mutate(DATETIME=as.POSIXct(as.character(DATETIME)))     
    saveRDS(shipdata, file = "ships.rds")
  } else {
    shipdata <- readRDS("ships.rds")
  }
  shipdata
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
    shipdata <- read.csv("ships.csv") %>% 
      mutate(DATETIME=as.POSIXct(as.character(DATETIME)))     
    saveRDS(shipdata, file = "ships_processed.rds")
  } else {
    shipdata <- readRDS("ships_processed.rds")
  }
  shipdata
}
