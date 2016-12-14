
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
  flog.info("Scaling few DIs from 'scale_vals'")
  
  grep_match_result <- match_grep(grep_vec = scale_vals, actual_names = names(timeline_df_subset_range[, numeric_cols]))
  timeline_df_subset_range[names(grep_match_result)] = 
    lapply(names(grep_match_result), function(x) grep_match_result[x] * timeline_df_subset_range[[x]])
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
  c(start_time = start_time, end_time, end_time)
}

