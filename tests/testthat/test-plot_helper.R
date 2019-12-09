library(testthat)
library(timelineR)
library(dplyr)

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


# --------------------------------------------------------------

context("get_pretty_breaks_labels")

test_that("Hours - minutes seconds case",{
  
  time_limits = c("2019-09-17 06:59:28", "2019-09-17 07:02:35") %>% as.POSIXct()
  
  expected_labels_list <- list(prt_brks = base::pretty(n = 10, x = time_limits),
                               brk_labels = c("2019-09-17 06:59:00", "06:59:30", "07:00:00", "07:00:30", 
                                              "07:01:00", "07:01:30", "07:02:00", "07:02:30", "07:03:00"),
                               xlabel = "Time (HH:MM:SS)")
  
  actual_labels_list <- timelineR:::get_pretty_breaks_labels(time_limits)
  
  expect_equal(actual_labels_list, expected_labels_list)
})


test_that("Hours - minutes case",{
  
  time_limits = c("2019-09-17 06:08:28", "2019-09-17 12:08:35") %>% as.POSIXct()
  
  expected_labels_list <- list(prt_brks = base::pretty(n = 10, x = time_limits),
                               brk_labels = c("2019-09-17 06:00:00", "06:30", "07:00", "07:30", "08:00",
                                              "08:30", "09:00", "09:30", "10:00", "10:30", "11:00",
                                              "11:30", "12:00", "12:30"),
                               xlabel = "Time (HH:MM)")
  
  actual_labels_list <- timelineR:::get_pretty_breaks_labels(time_limits)
  
  expect_equal(actual_labels_list, expected_labels_list)
})


test_that("Seconds case",{
  
  time_limits = c("2019-09-17 12:08:28", "2019-09-17 12:08:35") %>% as.POSIXct()
  
  expected_labels_list <- list(prt_brks = base::pretty(n = 10, x = time_limits),
                               brk_labels = c("2019-09-17 12:08:28", "29", "30", "31", "32",
                                              "33", "34", "35"),
                               xlabel = "Time (s)")
  
  actual_labels_list <- timelineR:::get_pretty_breaks_labels(time_limits)
  
  expect_equal(actual_labels_list, expected_labels_list)
})


test_that("Date Hours - minutes case",{
  
  time_limits = c("2019-09-15 16:08:28", "2019-09-17 12:08:35") %>% as.POSIXct()
  
  expected_labels_list <- list(prt_brks = base::pretty(n = 10, x = time_limits),
                               brk_labels = c("2019-09-15 12:00:00", "Sep 15 18:00", "Sep 16 00:00", "Sep 16 06:00",
                                              "Sep 16 12:00",  "Sep 16 18:00",  "Sep 17 00:00",  "Sep 17 06:00",
                                              "Sep 17 12:00",  "Sep 17 18:00"),
                               xlabel = "Time (Date HH:MM)")
  
  actual_labels_list <- timelineR:::get_pretty_breaks_labels(time_limits)
  
  expect_equal(actual_labels_list, expected_labels_list)
  
})


test_that("Date case",{
  
  time_limits = c("2019-09-01 12:08:28", "2019-09-17 12:08:35") %>% as.POSIXct()
  
  expected_labels_list <- list(prt_brks = base::pretty(n = 10, x = time_limits),
                               brk_labels = c("2019-09-01", "Sep 03", "Sep 05", "Sep 07", "Sep 09",
                                              "Sep 11", "Sep 13", "Sep 15", "Sep 17", "Sep 19"),
                               xlabel = "Time (Date)")
  
  actual_labels_list <- timelineR:::get_pretty_breaks_labels(time_limits)
  
  expect_equal(actual_labels_list, expected_labels_list)
  
})


test_that("Date-Month case",{
  
  time_limits = c("2018-11-17 06:08:28", "2019-09-17 12:08:35") %>% as.POSIXct()
  
  expected_labels_list <- list(prt_brks = base::pretty(n = 10, x = time_limits),
                               brk_labels =  c("2018-11-01", "Dec", "Jan", "Feb", "Mar",
                                               "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"),
                               xlabel = "Time (Date-Month)")
  
  actual_labels_list <- timelineR:::get_pretty_breaks_labels(time_limits)
  
  expect_equal(actual_labels_list, expected_labels_list)
})
