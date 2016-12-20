#' @importFrom dplyr %>% filter mutate_if
#' @importFrom lubridate is.POSIXct
#' @importFrom futile.logger flog.info
#' @importFrom stringr str_detect str_replace_all str_split_fixed str_trim
#' @importFrom grDevices dev.off png
#' @import ggplot2
#' @import gtable

#' @title Plotting function (standard style)
#' @description Plots time series data of State type (factors) as stripe charts, Numeric data type as step charts and an overlapping
#' combination of a plot of State type and Numeric type .
#' @param timeline_df Dataframe
#' @param data_cols A vector showing the columns to subset for plotting
#' @param start_time is left end point of the plot
#' e.g: start_time="2014-01-30 09:53:02.792663 UTC" or start_time=1391075599
#' @param end_time is right end point of the plot
#' @param ylimits A named vector to determine the limits on the y-axis for Sample plots
#' e.g: ylimits=list(a=c(0,100),d=c(-100,50)). The names must be present in the data frame
#' @param scale_vals A named vector to scale numeric data
#' e.g: scale_vals = c(a=10), matching data will be multiplied by 10
#' @param titles A named vector to give titles to the plot. For state and numeric plots, the names should be the same
#' as in the data frame. For overlapping plots, it should be the same as the name given in the overlap_plots_names.
#' e.g: titles = c(ab="first plot",cd="second plot")
#' @param ylabels change the labels on y-axis of plots
#' e.g: ylabel=c(ab="value",bcd="tmeperature")
#' @param save_path if a file_path is specified, then the image will be saved to that location.
#' @param add_legend TRUE (default) if legend is needed for the plots
#' @param plot_size_ratios proportion of event plot size to the sample plot size
#' @param overlap_plots_names specify the data items to be overlapped. Plots of the same type can only be overlapped for now.
#' This argument can be used to specify the order of plots.
#' e.g.: overlap_plots="list(overlap_plot1 = c(state1,numeric1), overlap_plot2 = c(state1,numeric2)" 
#' @param color_mapping A named list of named vectors. The names of the list are the names of the state columns in 
#' the data frame. Each named vector for a state should have color mapping for all the states in the column.
#' @param order_plots A vector containing the name of the plots to be plotted. The plots in the final output are 
#' arranged according to the order of the names in this vector.
#' @param plot_output Logical argument to specify if the output is required to be plotted or not. TRUE(default)
#' @return A grob of all the plots 
#' @export
plot_timeline <- function(timeline_df, data_cols = NULL, start_time=NULL, end_time=NULL,
                          ylimits=NULL, scale_vals=NULL, titles=NULL, 
                          ylabels=NULL, save_path = NULL, 
                          add_legend=TRUE, plot_size_ratios=NULL,
                          overlap_plots_names=NULL, color_mapping = list(),
                          order_plots = NULL, plot_output = T) {
  
  # This function takes in a data.frame of format
  # |Timestamp|Event_A|Event_B|Sample_A|Sample_B|
  # |2011-01-01 00:00:00.000Z|Type_A|Type_B|10|20|
  # The data.frame should have one timestamp columns, and one or more state and numeric columns
  # state columns SHOULD be factors or characters.
  # numeric columns should be numeric
  if(is.null(data_cols)) data_cols = names(timeline_df)
  check_input_arguments(timeline_df, data_cols, ylimits, scale_vals, titles,
                        ylabels, overlap_plots_names, plot_size_ratios)
  timeline_df %>% mutate_if(is.factor, as.character) -> timeline_df  
  col_type = get_col_types(timeline_df)
  
  time_limits = get_time_limits(start_time, end_time)

  
  timeline_df_subset = timeline_df[ , union(col_type$ts_col, data_cols)]
  timeline_df_subset_range = subset_data_into_time_range(timeline_df_subset, time_limits, col_type$ts_col)
  timeline_cleaned = scale_data(timeline_df_subset_range, scale_vals, col_type$numeric_cols)
  actual_ylimits <- get_plot_limits(timeline_cleaned, col_type$numeric_cols, ylimits)
  
  unique_state_factors <- lapply(timeline_df_subset_range[col_type$state_cols], function(x) if(is.character(x)) unique(x))
  state_plots <- timeline_cleaned %>% 
    create_state_plots(col_type$ts_col, col_type$state_cols) %>% 
    add_colors_to_state_plots(color_mapping, unique_state_factors)
  numeric_plots <- create_numeric_plots(timeline_cleaned, col_type$ts_col, col_type$numeric_cols, actual_ylimits) 

  all_plots <- c(numeric_plots, state_plots) %>% 
    add_legend_to_plots(add_legend) %>% 
    add_titles_to_the_plot(titles) %>% 
    add_pretty_breaks_and_xlabel(range(timeline_cleaned[[col_type$ts_col]]))

  ## Below function uses some intelligence to label X axis and also add x ticks with breaks
  all_plots <- add_ylabels_to_the_plot(all_plots, ylabels, col_type$state_cols)
  
  overlap_plots_grob <- create_all_overlapping_plots(all_plots, col_type$state_cols, 
                                                     col_type$numeric_cols, overlap_plots_names, titles)
  grob_output = align_the_plots(all_plots, overlap_plots_grob, plot_size_ratios, order_plots)
  draw_the_plots(grob_output, save_path, plot_output)
  return(grob_output)
}
