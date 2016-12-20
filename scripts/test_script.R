library(vimana)
library(ggplot2)
library(gtable)
library(dplyr)
library(lubridate)
library(stringr)
library(futile.logger)
library(plotly)

test_device = readRDS("../res-common/case-studies/core/Rules_profile_analysis/shiny_app/jwpmfg_Grob_G550_cbeab7")
test_device_df = mtconnectR::merge(test_device) # %>% na.omit()
# names(test_device_df)= c("timestamp", "s_ovr", "s_speed", "s_speed_com", "execution", "controller_mode")

timeline_df = test_device_df
# timeline_df[[4]][1000:9500] = NA
start_time = "2015-11-11 02:32:49.203" %>% as.POSIXct
end_time = "2017-11-11 07:45:28.007" %>% as.POSIXct
save_path = "device.png"
add_legend = T

ylimits = list("SPEED-ACTUAL" = c(100, 1800)) %>% match_grep(names(timeline_df))
scale_vals = c("SPEED-ACTUAL" = 1e-4, "SPEED-OVER" = 1e2)  %>% match_grep(names(timeline_df))
data_cols = c("MODE", "EXEC", "SPEED") %>% match_grep(names(timeline_df), use_values = T, return_names = T)
titles = c("MODE" = "THE MODE", "EXEC" = "THE EX", "SPEED-ACTUAL" = "PEED") %>% match_grep(names(timeline_df))
ylabels = c("MODE" = "THE MODE", "execution" = "THE EX", "s_speed" = "PEED") %>% match_grep(names(timeline_df))
plot_size_ratios = c("MODE" = 0.5, "execution" = 0.5, "SPEED-ACTUAL" = 2) %>% match_grep(names(timeline_df))
# color_mapping = list("MODE" = c("Unavailable" = "gray", "AUTOMATIC" = "darkgreen"),
                     # "EXEC" = c("ACTIVE" = "darkgreen", "READY" = "blue", "Unavailable" = "gray")) %>% match_grep(names(timeline_df))
overlap_plots = NULL
state_plot_size = .3


output_grob = plot_timeline(timeline_df, data_cols, start_time, end_time,
              ylimits, scale_vals, titles, 
              ylabels, save_path = NULL, 
              add_legend, plot_size_ratios,
              overlap_plots = NULL, color_mapping = NULL)
  
# Things to verify
# Test that the label has come correctly
# Test that the x

expect_true(TRUE)


plotly::ggplotly(state_plots[[1]])
