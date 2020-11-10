library(shinytest)
library(testthat)
context("Test Shiny app")

app <- ShinyDriver$new("./", loadTimeout = 1e+05, seed = 4323)

test_that("data is matched", {
  exported_df <- app$getAllValues()$export$test_df
  expect_equal(exported_df, readRDS("ships.rds"))  
})

test_that("processing data is successful", {
  exported_processed_df <- app$getAllValues()$export$test_processed_df
  expect_equal(exported_processed_df, readRDS("ships_processed.rds"))
  expect_equal(names(exported_processed_df),c("SHIPNAME","SPEED","LON","LAT","LON_prev","LAT_prev", "dist2prev"    
                                               ,"DATETIME","DATETIME_prev","DATETIME_diff"))
})

app$stop()