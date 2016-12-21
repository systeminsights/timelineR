
give_match_status <- function(grep_result, actual_names, echo = F){
  no_matches = rowSums(grep_result) < 1
  if(echo && sum(no_matches))
    flog.info(sprintf("No matches found for scaling %s", paste(actual_names[no_matches], collapse = ", ")))
  
  multiple_matches = rowSums(grep_result) > 1
  if(echo && sum(multiple_matches))
    flog.info(sprintf("Multiple matches found for scaling %s", paste(actual_names[multiple_matches], collapse = ", ")))
  
  rowSums(grep_result) == 1
}

#' @title Regular expression based extraction
#' @description This function does a regular expression based search for each name in one vector for the values in the other vector and
#' returns a named vector with names as the matched names and values as given in the queried vector. 
#' @param grep_vec A named vector with the names to be searched for and the values, which the matching names should hold.
#' It can also be a unnamed vector of names to search for. 
#' @param actual_names A vector giving the names in which the search is to be made
#' @param use_values Logical value. (TRUE) if the values in the grep_vec are to be used for searching. Defualut is FALSE
#' @param return_name Logical value (TRUE) if just want to return the matching names and not the values. Defualut is FALSE
#' @param echo Logical value(TRUE) To print for each name in the grep_vec, which values in actual_names match and didnt match.
#'  Defualut is FALSE
#' @return A named vector with the matched names and substituted values or a vector of macthed names
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
  if(!is.null(start_time)) start_time <- as.POSIXct(start_time)
  if(!is.null(end_time)) end_time <- as.POSIXct(end_time)
  list(start_time = start_time, end_time = end_time)
}

get_col_types <- function(timeline_df) {
  ts_col = names(timeline_df)[timeline_df %>% sapply(is.POSIXct)]
  numeric_cols = names(timeline_df)[timeline_df %>% sapply(is.numeric)]
  state_cols = names(timeline_df)[timeline_df %>% sapply(is.character)]
  
  if(length(ts_col) == 0) stop("No POSIXct columns detected to assign the timestamp!")
  if(length(ts_col) > 1) stop("Multiple POSIXct columns detected. Timeline DF should have only on timestamp!")
  
  flog.info(sprintf("%s has been selected as the timestamp column", ts_col))
  flog.info(sprintf("%s has been selected as the numeric column(s)", paste(numeric_cols, collapse = ", ")))
  flog.info(sprintf("%s has been selected as the state column(s)", paste(state_cols, collapse = ", ")))
  list(ts_col = ts_col, numeric_cols = numeric_cols, state_cols = state_cols)
}

check_input_arguments <- function(timeline_df, data_cols, ylimits, scale_vals,
                                  titles, ylabels, overlap_plots, plot_size_ratios){
  
  if(!all(data_cols %in% names(timeline_df))) stop("All Data columns not in timeline_df!")
  if(!all(names(ylimits) %in% names(timeline_df))) stop("All Ylimit names not in timeline_df!")
  if(!all(names(scale_vals) %in% names(timeline_df))) stop("All scale_vals names not in timeline_df!")
  if(!all(names(titles) %in% c(names(timeline_df),names(overlap_plots)))) stop("All titles names not in timeline_df!")
  if(!all(names(ylabels) %in% names(timeline_df))) stop("All ylabels names not in timeline_df!")
  if(!all(names(overlap_plots) %in% c(names(timeline_df), names(overlap_plots)))) 
    stop("All overlap_plots names not in timeline_df!")
  if(!all(names(plot_size_ratios) %in% c(names(timeline_df), names(overlap_plots))))
    stop("All plot_size_ratios names not in timeline_df!")
  return(TRUE)
}
