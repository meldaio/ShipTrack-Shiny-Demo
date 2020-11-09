library(geosphere)
library(dplyr)

load_raw_ship_data <- function(){
  if (!exists("shipdata")){
    if (!file.exists("ships.RData")) {
      shipdata <- read.csv("ships.csv") %>% 
        mutate(DATETIME=as.POSIXct(as.character(DATETIME)))     
      saveRDS(shipdata, file = "ships.rds")
    } else {
      shipdata <- readRDS("ships.rds")
    }
  }
  shipdata
}