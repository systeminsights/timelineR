library(vayu)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

source("./R/visualize.R")

test_device = readRDS("../res-common/case-studies/core/Rules_profile_analysis/shiny_app/jwpmfg_A88e_311_8315dc")
test_device_df = mtconnectR::merge(test_device)

start_time = "2016-11-03 06:32:49.203" %>% as.POSIXct
end_time = "2016-11-08 13:45:28.007" %>% as.POSIXct

save_path = "device.png"

color_palette_manual = c("#6fc376", "#f6928f","#c1c1c1")


timeline_df = test_device_df
PlotDataItems(timeline_df, color_palette_manual = color_palette_manual,save_path = save_path,xlabels = "Appender line")







