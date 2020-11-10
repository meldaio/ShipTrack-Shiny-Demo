library(shinytest)
library(testthat)
context("Test Shiny app")

app <- ShinyDriver$new("./", loadTimeout = 1e+05, seed = 4323)

test_that("all data files are in place", {
  expect_equal(file.exists("ships.rds"),TRUE)
  expect_equal(file.exists("shipmeta.rds"),TRUE)
  expect_equal(file.exists("ships_processed.rds"),TRUE)
})

test_that("data written the shiny app matches data and disk", {
  exported_df <- app$getAllValues()$export$test_df
  expect_equal(exported_df, readRDS("ships_processed.rds"))  
})

test_that("processing data is successful", {
  exported_processed_df <- app$getAllValues()$export$test_processed_df
  expect_equal(exported_processed_df, readRDS("ships_processed.rds"))
  expect_equal(names(exported_processed_df),c("SHIPNAME","SPEED","LON","LAT","LON_prev","LAT_prev", "dist2prev"    
                                               ,"DATETIME","DATETIME_prev","DATETIME_diff"))
})



app$stop()