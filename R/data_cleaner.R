
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
