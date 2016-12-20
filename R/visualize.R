


#' @title Plotting function (standard style)
#' @description Plots time series data of Event type (character type data) as stripe charts, Sample data type (numeric type data) as step charts.
#' @param input is a data.frame or, list of data.frames or DataItems
#' @param dataGrep is a regex expression used to select some of input objects to plot. 
#' e.g: dataGrep="a|e"
#' @param invert TRUE if you want to exclude the items that match the dataGrep, similar to invert argument in grep function
#' @param start_time is left end point of the plot
#' e.g: start_time="2014-01-30 09:53:02.792663 UTC" or start_time=1391075599
#' @param end_time is right end point of the plot
#' @param ylimits this is used to determine the limits on the y-axis for Sample plots
#' e.g: ylimits=list(a=c(0,100),d=c(-100,50)). Any variable that has "a" in the name will be plotted with limits (0,100).
#' @param scale_vals used to multiply the values by a factor
#' e.g: scale_valse=c(a=10), matching data will be multiplied by 10
#' @param titles change titles of the plot
#' e.g: title=c(ab="first plot",cd="second plot")
#' @param ylabels change the labels on y-axis of plots
#' e.g: ylabel=c(ab="value",bcd="tmeperature")
#' @param returnGG if TRUE, GGplot objects will be returned for further manual processing
#' @param add_legend TRUE (default) if legend is needed
#' @param event_plot_size proportion of event plot size to the sample plot size
#' @param save_path if a file_path is specified, then the image will be saved to that location.
#' @param ifPlotAsSample for plotting event variables as sample overriding the default stripe chart behavior
#' e.g.: ifPlotAsSample=c(xyz=TRUE)
#' @param overlap_plots specify the data items to be overlapped. Plots of the same type can only be overlapped for now.
#' This argument can be used to specify the order of plots.
#' e.g.: overlap_plots="(abc,pqr),(pqr),(xyz,123,345)", 
#' overlap_plots = "(S1speed),(path_feedrate1),(executi,CONTROLLER_MODE)"
#' @return list of the filtered data will be returned invisibly 
plot_timeline <- function(timeline_df, data_cols = NULL, start_time=NULL, end_time=NULL,
                          ylimits=NULL, scale_vals=NULL, titles=NULL, 
                          ylabels=NULL, save_path = NULL, 
                          add_legend=TRUE, plot_size_ratios=NULL,
                          overlap_plots_names=NULL, color_mapping = list()) {
  
  # This function takes in a data.frame of format
  # |Timestamp|Event_A|Event_B|Sample_A|Sample_B|
  # |2011-01-01 00:00:00.000Z|Type_A|Type_B|10|20|
  # The data.frame should have one timestamp columns, and one or more state and numeric columns
  # state columns SHOULD be factors or characters.
  # numeric columns should be numeric
  
  if(!is.null(data_cols)) data_cols = names(timeline_df)
  
  check_input_arguments(timeline_df, data_cols, ylimits, scale_vals, titles,
                        ylabels, overlap_plots_names, plot_size_ratios)
  
  time_limits = get_time_limits(start_time, end_time)
  ts_col = names(timeline_df)[timeline_df %>% sapply(is.POSIXct)]
  timeline_df_subset = timeline_df[ , union(ts_col, data_cols)]
  
  timeline_df_subset_range = subset_data_into_time_range(timeline_df_subset, time_limits, ts_col)
  # remove_nas <- function(timeline_df_subset_range){
  #   timeline_df_subset_range = timeline_df_subset_range %>% na.omit()
  # }
  # 
  
  numeric_cols = names(timeline_df_subset_range)[timeline_df_subset_range %>% sapply(is.numeric)]
  timeline_cleaned = scale_data(timeline_df_subset_range, scale_vals, numeric_cols)
  
  state_cols = names(timeline_cleaned)[timeline_cleaned %>% sapply(is.character)]
  actual_ylimits <- get_plot_limits(timeline_cleaned, numeric_cols, ylimits)
  
  unique_state_factors <- lapply(timeline_df_subset_range[state_cols], function(x) if(is.character(x)) unique(x))
  state_plots <- timeline_cleaned %>% 
    create_state_plots(ts_col, state_cols) %>% 
    add_colors_to_state_plots(color_mapping, unique_state_factors)
  numeric_plots <- create_numeric_plots(timeline_cleaned, ts_col, numeric_cols, actual_ylimits) 

  all_plots <- c(numeric_plots, state_plots) %>% 
    add_legend_to_plots(add_legend) %>% 
    add_titles_to_the_plot(titles) %>% 
    add_pretty_breaks_and_xlabel(time_limits)

  ## Below function uses some intelligence to label X axis and also add x ticks with breaks
  all_plots <- add_ylabels_to_the_plot(all_plots, ylabels, state_cols)
  
  # if(returnGG) return(all_plots)
  overlap_plots_grob <- create_all_overlapping_plots(all_plots, state_cols, numeric_cols, overlap_plots_names, titles)
  grob_output = align_and_draw_the_plots(all_plots, overlap_plots_grob, plot_size_ratios, save_path)  
  return(grob_output)
}
