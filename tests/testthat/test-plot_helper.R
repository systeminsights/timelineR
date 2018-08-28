library(testthat)
library(timelineR)

Sys.setenv(TZ="Asia/Kolkata")
context("Testing generate_color_mapping function")

test_that("All columns have same set of values", {
  test_df = data.frame(timestamp = as.POSIXct(c("2017-01-01","2017-01-02", "2017-01-03")), 
                       col_1 = c("value1", "value2", "value3"), 
                       col_2 = c("value1", "value2", "value3"))
  default_color_mapping = c("value1" = "green", "value2" = "red", "value3" = "green")
  
  expected_output = structure(list(col_1 = structure(c("green", "red", "green"), .Names = c("value1", "value2", "value3")),                                      col_2 = structure(c("green", "red", "green"), .Names = c("value1", "value2", "value3"))),                               .Names = c("col_1","col_2"))
  
  expect_equal(generate_color_mapping(test_df, default_color_mapping), expected_output)
  # plot_timeline(test_df, color_mapping =  expected_output)
  
})

test_that("All columns do not contain all values", {
  test_df = data.frame(timestamp = as.POSIXct(c("2017-01-01","2017-01-02", "2017-01-03")), 
                       col_1 = c("value1"), 
                       col_2 = c("value1", "value2", "value3"))
  default_color_mapping = c("value1" = "green", "value2" = "red", "value3" = "green")
  
  expected_output = structure(list(col_1 = structure(c("green"), .Names = c("value1")),                                                                          col_2 = structure(c("green", "red", "green"), .Names = c("value1", "value2", "value3"))),                               .Names = c("col_1","col_2"))
  
  expect_equal(generate_color_mapping(test_df, default_color_mapping), expected_output)
})

