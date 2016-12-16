library(vimana)
library(ggplot2)
library(gtable)
library(dplyr)
library(lubridate)
library(stringr)
library(futile.logger)
library(plotly)

source("./R/visualize.R")

context()

test_device = readRDS("../res-common/case-studies/core/Rules_profile_analysis/shiny_app/jwpmfg_Grob_G550_cbeab7")
test_device_df = mtconnectR::merge(test_device) # %>% na.omit()
str(test_device_df)
range(test_device_df$timestamp)

start_time = "2016-11-11 02:32:49.203" %>% as.POSIXct
end_time = "2016-11-11 07:45:28.007" %>% as.POSIXct

# save_path = "device.png"
save_path = NULL
color_palette_manual = c("#6fc376", "#f6928f","#c1c1c1")


timeline_df = test_device_df
data_grep = "mode|exec|peed"
names(timeline_df)
invert = F
add_legend = T
# PlotDataItems(timeline_df, color_palette_manual = color_palette_manual,save_path = save_path,xlabels = "Appender line")

grep_vec = scale_vals = c("SPEED-ACTUAL" = 1e-4, "SPEED-COMMANDED" = 1e2)
ylimits = list("SPEED-ACTUAL" = c(100, 1800))
titles = c("mode" = "THE MODE", "exec" = "THE EX", "peed" = "PEED")
xlabels = c("mode" = "THE MODE", "exec" = "THE EX", "peed" = "PEED")
ylabels = c("mode" = "THE MODE", "exec" = "THE EX", "peed" = "PEED")
state_plot_size = .3

color_mapping = list("jwpmfg_Grob_G550_cbeab7<Device>:mode<CONTROLLER_MODE>" = c("Unavailable" = "green", "AUTOMATIC" = "blue"))

ggplot() + state_plots[[1]] + numeric_plots[[1]]

# Things to verify
# Test that the label has come correctly
# Test that the x

expect_true(TRUE)


plotly::ggplotly(state_plots[[1]])
