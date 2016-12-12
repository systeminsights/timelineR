#' removes each non-timestamp column in the input and keeps into a different DataItem object and then returns the list of DataItems
CreateDataItemlistFromDF <- function(mergedDF) {
  if(!(("timestamp" %in% names(mergedDF)) | all(c("start","end") %in% names(mergedDF))))
    return(vimana::return_NULL_with_message("No required columns in mergedDF!"))
  
  #extract variable columns and time columns
  varCols <- names(mergedDF)[!((names(mergedDF)=="timestamp")|(names(mergedDF)=="start")|
                                 (names(mergedDF)=="end")|(names(mergedDF)=="duration"))]
  timeCols <- names(mergedDF)[((names(mergedDF)=="timestamp")|(names(mergedDF)=="start")|
                                 (names(mergedDF)=="end")|(names(mergedDF)=="duration"))]
  
  #cut the dataframe into a list
  dataItemList <- lapply(varCols, function(col_name) {
    temp <- mergedDF[,c(timeCols,col_name)]
    rownames(temp) <- NULL
    names(temp) <- c(timeCols,"value")
    DIType_S_or_E <- ifelse(is.numeric(temp$value),"Sample","Event")
    new("DataItem",temp,DIType_S_or_E,"TEMP")
  })
  names(dataItemList) <- varCols
  return(dataItemList)
}

#' finds the upper and lower time limits of a dataItemList
FindTimeRangeOfList <- function(input, type="outer") {
  if(is(input,"data.frame")) {
    time_cols <- which(vapply(X=input,FUN=is.POSIXct,FUN.VALUE=FALSE,USE.NAMES=FALSE))
    if(length(time_cols)==0) {
      time_cols <- which(names(input) %in% c("timestamp","start","end"))
    }
    mins <- numeric(0)
    maxs <- numeric(0)
    for(i in time_cols) {
      range_temp <- range(input[[i]],na.rm = TRUE)
      mins <- c(mins,range_temp[1])
      maxs <- c(maxs,range_temp[2])
    }
    lims <- c("min"=min(mins,na.rm = TRUE),"max"=max(maxs,na.rm = TRUE))
    if(is.POSIXct(input[[time_cols[[1]]]])) lims <- toPOSIXct(lims)
    return(lims)
  } else if(is(input,"DataItem")) {
    return(FindTimeRangeOfList(input@data))
  } else if(is(input,"list")) {
    mins <- numeric(0)
    maxs <- numeric(0)
    for(list_obj in input) {
      range_temp <- FindTimeRangeOfList(list_obj,type=type)
      mins <- c(mins,range_temp[1])
      maxs <- c(maxs,range_temp[2])
    }
    if(type %in% "outer") {
      lims <- toPOSIXct(c("min"=min(mins,na.rm = TRUE),"max"=max(maxs,na.rm = TRUE)))
    } else if(type %in% "inner") {
      lims <- toPOSIXct(c("min"=max(mins,na.rm = TRUE),"max"=min(maxs,na.rm = TRUE)))
    } else {
      stop("wrong type option!")
    }
    return(lims)
  } else if(is.atomic(input)) {
    lims <- range(input,na.rm = TRUE)
    names(lims) <- c("min","max")
    lims <- toPOSIXct(lims)
    return(lims)
  } else {
    stop("no supported objects found!")
  }
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


organize_input_to_df <- function(nam_in, len_in, input){
  # organizing all the inputs into a list of data.frames
  SorE <- character(0)
  data_list <- list()
  
  for(i in seq_len(len_in)) {
    
    single_obj <- input[[i]]
    if(is(single_obj,"DataItem")) {
      data_list <- c(data_list,
                     structure(list(single_obj@data),.Names=nam_in[i])
      )
      SorE <- c(SorE,
                structure(single_obj@is.sample,.Names=nam_in[i])
      )
    } else if(is(object = single_obj,class2 = "data.frame")) {
      nam_cols <- names(single_obj)
      # TODO: Timecols are always timestamp,start,end or duration
      time_cols <- grep(pattern = "timestamp|start|end|duration",x = nam_cols,ignore.case = FALSE,value = TRUE)
      if(length(time_cols)==0) stop("no time column in input!")
      other_cols <- setdiff(x = nam_cols,time_cols)
      ## TODO: If value is there, that object is just like that returned.
      if('value' %in% other_cols) {
        data_list <- c(data_list,
                       structure(list(single_obj),.Names=nam_in[i])
        )
        SorE <- c(SorE,
                  structure(
                    ifelse(is.character(single_obj[["value"]]),"Event","Sample"),
                    .Names=nam_in[i])
        )
      } else {
        to_add <- lapply(other_cols,function(x) {
          tempDF <- single_obj[,c(time_cols,x)]
          # TODO: Second column is by default renamed to value here
          names(tempDF)[ncol(tempDF)] <- "value"
          tempDF
        })
        
        # TODO: If more than two columns (ts,v1,v2), to_add will be a list of 2 where each is a df with
        # ts,v1 and ts,v2 respectively
        # TODO: This is the place where title of each plot is generated, would be good to have " " as separator (name of list and column name)
        names_to_add <- paste0(nam_in[i],"_",other_cols)
        data_list <- c(data_list,
                       structure(to_add,.Names=names_to_add) 
        )
        SorE <- c(SorE,structure(
          # TODO: SorE -> Sample or Event
          ifelse(
            vapply(single_obj[other_cols],is.character,FUN.VALUE = FALSE,USE.NAMES = FALSE),
            "Event","Sample"
          ),
          .Names=names_to_add
        )
        )
      }
    } else {
      stop("wrong input type")
    }
  }
  rm(single_obj)
  ## TODO: whatever is done in to_add has to be done everywhere, have to find out why the exception if
  ## value column is present
  
  # TODO: List of stuff computed till now -> 1. data_list 2. SorE 3. len_in 4. nam_in (updated based on name of object parsed) 5. input_statement
  # Note: Names of data list now becomes name(first list)_othercols, also names of SorE
  
  # grepping the required data items
  
  # TODO: Options INVERT and DATAGREP are used here
  # TODO: Why is invert an option? Is it like invert and dataGrep lets us choose which items of list should be plotted. It does seem to be that
  return(list(SorE = SorE, data_list = data_list))
}


get_start_and_end_time_limits <- function(start_time, end_time, data_list){
  #finding start and end time limits, only when the limits are not already specified
  lims <- c(Sys.time(),Sys.time())
  if( is.null(start_time) || is.null(end_time) ) {
    lims <- FindTimeRangeOfList(data_list)
  }
  
  if(!is.null(start_time))
    lims[1] <- start_time
  if(!is.null(end_time))
    lims[2] <- end_time
  
  return(lims)  
}


filter_data_list_limits <- function(data_list, lims){
  message("Filtering within specified Time Range")
  data_list <- mapply(FUN = FilterinTimeRangeDataframe,dataframe=data_list,
                      MoreArgs = list(startTimes=lims[1],endTimes=lims[2]),
                      SIMPLIFY = FALSE,USE.NAMES = TRUE)
  return(data_list)
}


modify_for_sample_plot <- function(SorE, data_list, ifPlotAsSample){
  message("Plotting few DIs as Sample from 'ifPlotAsSample'")
  # Do we really need this match_grep function? Can't we have something simpler
  newSorE <- .match_grep(grep_vec = ifPlotAsSample,actual_names = names(data_list))
  newSorE <- ifelse(newSorE,"Sample","Event")
  SorE[names(newSorE)] <- newSorE
  return(SorE)
}


scale_vals_data <- function(data_list, scale_vals){
  message("Scaling few DIs from 'scale_vals'")
  grep_match_result <- .match_grep(grep_vec = scale_vals,actual_names = names(data_list))
  for(i in names(grep_match_result)) {
    data_list[[i]]$value <- data_list[[i]]$value * grep_match_result[i]
  }
  return(data_list)
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
