
give_match_status <- function(grep_result, actual_names, echo = F){
  no_matches = rowSums(grep_result) < 1
  if(echo && sum(no_matches))
    flog.info(sprintf("No matches found for scaling %s", paste(actual_names[no_matches], collapse = ", ")))
  
  multiple_matches = rowSums(grep_result) > 1
  if(echo && sum(multiple_matches))
    flog.info(sprintf("Multiple matches found for scaling %s", paste(actual_names[multiple_matches], collapse = ", ")))
  
  rowSums(grep_result) == 1
}

match_grep <- function(grep_vec, actual_names, use_values = F, return_names = F, echo = F) {
  
  if(use_values) names(grep_vec) = grep_vec
  grep_result <- vapply(X = names(grep_vec), FUN = stringr::str_detect, string=actual_names,
                        USE.NAMES = FALSE, FUN.VALUE = logical(length(actual_names)))
  if(is.vector(grep_result)) grep_result <- matrix(grep_result,nrow = length(actual_names))
  
  one_match_check = give_match_status(grep_result, actual_names, echo = echo)
  
  which_matches <- apply(X = grep_result[one_match_check, ,drop=FALSE], MARGIN = 1, FUN = which)
  result_vec <- grep_vec[which_matches]
  names(result_vec) <- actual_names[one_match_check]
  if(return_names) return(names(result_vec))
  result_vec
}


get_new_names <- function(input_statement, nam_in, len_in){
  new_nams <- str_trim(str_split_fixed(str_replace_all(str_trim(input_statement),
                                                       "^list\\(|\\)$",""),pattern = ",",
                                       n = len_in)[1,])
  if(is.null(nam_in)) nam_in <- rep("",times = len_in)
  which_check <- nam_in %in% ""
  nam_in[which_check] <- new_nams[which_check]
  return(nam_in)
}

delete_empty_data_items <- function(data_list, SorE){
  #removing empty data items
  zero_row_items <- vapply(X=data_list,FUN=function(x) return((nrow(x)==0)||is.null(x)),FUN.VALUE=FALSE,USE.NAMES=FALSE)
  if(any(zero_row_items)) {
    message(paste0(names(data_list)[zero_row_items],collapse=", ")," are either NULL or have zero rows. Excluding these DIs.")
    data_list[zero_row_items] <- NULL
    SorE <- SorE[!zero_row_items]
  }
  return(list(data_list = data_list, SorE = SorE))
}


scale_data <- function(timeline_df_subset_range, scale_vals, numeric_cols){
  if(is.null(scale_vals)) return(timeline_df_subset_range)
  flog.info("Scaling data accoding to 'scale_vals'")
  
  timeline_df_subset_range[names(scale_vals)] = 
    lapply(names(scale_vals), function(x) scale_vals[x] * timeline_df_subset_range[[x]])
  timeline_df_subset_range
}

subset_data_into_time_range <- function(timeline_df_subset, time_limits, ts_col){
  
  #finding start and end time limits, only when the limits are not already specified
  time_range <- range(timeline_df_subset[[ts_col]])
  if(!is.null(time_limits$start_time)) time_range[1] <- time_limits$start_time
  if(!is.null(time_limits$end_time))   time_range[2] <- time_limits$end_time
  
  # function to subset data frame into time range
  
  timeline_df_subset %>% filter(timeline_df_subset[[ts_col]] >= time_range[1], timeline_df_subset[[ts_col]] <= time_range[2])
  
}

# convert start and end into POSIXct objects
get_time_limits <- function(start_time, end_time){
  if(!is.null(start_time)) start_time <- toPOSIXct(start_time)
  if(!is.null(end_time)) end_time <- toPOSIXct(end_time)
  list(start_time = start_time, end_time = end_time)
}

check_input_arguments <- function(timeline_df, data_cols, ylimits, scale_vals,
                                  titles, ylabels, overlap_plots, plot_size_ratios){
  if(!all(data_cols %in% names(timeline_df))) flog.stop("All Data columns not in timeline_df!")
  if(!all(names(ylimits) %in% names(timeline_df))) flog.stop("All Ylimit names not in timeline_df!")
  if(!all(names(scale_vals) %in% names(timeline_df))) flog.stop("All scale_vals names not in timeline_df!")
  if(!all(names(titles) %in% names(timeline_df))) flog.stop("All titles names not in timeline_df!")
  if(!all(names(ylabels) %in% names(timeline_df))) flog.stop("All ylabels names not in timeline_df!")
  # if(!all(names(overlap_plots) %in% names(timeline_df))) flog.stop("All overlap_plots names not in timeline_df!")
  if(!all(names(plot_size_ratios) %in% names(timeline_df))) flog.stop("All plot_size_ratios names not in timeline_df!")
  return(TRUE)
}
