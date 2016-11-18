library(vayu)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

source("./R/fns_visualize.R")

device_data <- readRDS("./gegv_innershrouds_NQA2_702457_f98702")
device_data_df <- merge(device_data)
start_time = "2016-09-03 06:32:49.203" %>% as.POSIXct
end_time = "2016-09-08 13:45:28.007" %>% as.POSIXct

savepath = "device.png"

color_palette_manual = c("#6fc376", "#f6928f","#c1c1c1")
PlotDataItems(device_report, color_palette_manual = color_palette_manual,savePath = savepath,xlabels = "Appender line")







