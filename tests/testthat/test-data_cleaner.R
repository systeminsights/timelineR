library(testthat)
library(timelineR)

context("data_cleaner: check_input_arguments")

test_that("Check if is sorted", {
  test_df = data.frame(timestamp = as.POSIXct(c("2017-01-01","2017-01-03", "2017-01-02")), 
                       col_1 = c("value1", "value2", "value3"), 
                       col_2 = c("value1", "value2", "value3"))
  expect_output(plot_timeline(test_df), "WARN.*")
})