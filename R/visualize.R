

.get_x_limits_from_ggplot <- function(ggobject) {
  num_layers <- length(ggobject$layers)
  if(!is.null(ggobject$coordinates$limits$x)) {
    return(ggobject$coordinates$limits$x)
  }
  range_vec <- numeric(0)
  for(i in seq_len(num_layers)) {
    x_var <- as.character(ggobject$layers[[i]]$mapping$x)
    if(length(x_var)>0) {
      if(length(ggobject$layers[[1]]$data)==0) {
        new_range <- range(ggobject$data[[x_var]])
      } else {
        new_range <- range(ggobject$layers[[i]]$data[[x_var]])
      }
    } else {
      xmin_var <- as.character(ggobject$layers[[i]]$mapping$xmin)
      xmax_var <- as.character(ggobject$layers[[i]]$mapping$xmax)
      if(length(ggobject$layers[[1]]$data)==0) {
        min_value <- min(ggobject$data[[xmin_var]])
        max_value <- max(ggobject$data[[max_value]])
      } else {
        min_value <- min(ggobject$layers[[i]]$data[[xmin_var]])
        max_value <- max(ggobject$layers[[i]]$data[[xmax_var]])
      }
      new_range <- c(min_value,max_value)
    }
    range_vec <- range(range_vec,new_range)
  }
  range_vec
}

.AddExtrasToGGPlot <- function(ggobject,xrange=NULL) {
  if(is.null(xrange) || anyNA(xrange))
    xrange <- toPOSIXct(.get_x_limits_from_ggplot(ggobject))
  prt_brks <- base::pretty(n = 10,x = xrange)
  xlabels <- attr(prt_brks, "labels")
  one_label <- xlabels[1]
  xlabels[1] <- as.character(prt_brks[1])
  if(str_detect(string=one_label,pattern="^[[:digit:]]+:[[:digit:]]+:[[:digit:]]+$")) {
    lable <- paste("Time (HH:MM:SS)")
  } else if(str_detect(string=one_label,pattern="^[[:digit:]]+:[[:digit:]]+$")) {
    lable <- paste("Time (HH:MM)")
  } else if(str_detect(string=one_label,pattern="^[[:digit:]]+$")) {
    lable <- paste("Time (s)")
  } else if(str_detect(string=one_label,pattern="^[[:alpha:]]+ [[:digit:]]+ [[:digit:]]+:[[:digit:]]+$")) {
    lable <- paste("Time (Date HH:MM)")
  } else if(str_detect(string=one_label,pattern="^[[:alpha:]]+ [[:digit:]]+$")) {
    lable <- paste("Time (Date)")
  } else {
    stop("something wrong!")
  }
  ggobject <- ggobject + xlab(lable) + scale_x_datetime(breaks=prt_brks,labels=xlabels)
  return(ggobject)
}





.map_values_to_colors <- function(input, color_palette_manual = NULL) {
  
  # TODO: What is this colorfill?
  if("colorfill" %in% names(input)) {
    input %>%
      select(value, colorfill) %>% 
      distinct ->
      mapped_colors_df
    mapped_colors <- mapped_colors_df$colorfill
    names(mapped_colors) <- mapped_colors_df$value
    return(mapped_colors)
  }
  
  color_palette <- c(green = "#6fc376", pink = "#f6928f", grey = "#c1c1c1", white = "#ffffff")
  if(!is.null(color_palette_manual))
    color_palette[1:length(color_palette_manual)] = color_palette_manual
  
  color_mapping <- c(`Data-Unavailable` = "grey", `Non-Producing` = "pink", Producing = "green", `NA` = "white")
  
  uniq_values <- unique(input$value)
  mapped_colors <- color_palette[color_mapping[uniq_values]]
  names(mapped_colors) <- uniq_values
  which_nas <- is.na(mapped_colors)
  num_nas <- sum(which_nas)
  # Able to understand the result of below code but is'nt it too complex?
  mapped_colors[which_nas] <- 
    c(
      setdiff(color_palette,mapped_colors),
      rep(unname(color_palette),times = ceiling(num_nas/length(color_palette)))
    )[seq_len(num_nas)]
  mapped_colors
}

# TODO: Go through this later
.generate_sample_plot_layer <- function(data_to_plot, lineWidth=0.3) {
  if(nrow(data_to_plot)<2) {
    if(anyNA(data_to_plot$value))
      return(vimana::return_NULL_with_message("NAs found in the value column. Not Plotting this particular data."))
    data_to_plot <- MakeEndTime(data=data_to_plot,endtime.lastrow=toPOSIXct("2100-01-01"))
  }
  
  if("start" %in% names(data_to_plot)) {
    data_to_plot$value2 <- data_to_plot$value
    continuous_points <- which(data_to_plot$end[-nrow(data_to_plot)] == data_to_plot$start[-1L])
    if(length(continuous_points)>0) {
      data_to_plot <- rbind(data_to_plot,
                            data.frame(start=data_to_plot$start[continuous_points+1L],end=data_to_plot$end[continuous_points],
                                       value=data_to_plot$value[continuous_points],value2=data_to_plot$value[continuous_points+1L])
      )
    }
    plotobj <- geom_segment(data=data_to_plot,aes(x=start,xend=end,y=value,yend=value2),size=lineWidth)
    return(plotobj)
  } else {
    plotobj <- geom_step(data=data_to_plot,aes(x=timestamp,y=value,group=1),direction="hv",size=lineWidth)
    return(plotobj)
  }
}

.overlap_sample_plots <- function(list_of_plots) {
  all_names <- names(list_of_plots)
  for(i in seq_along(list_of_plots)) {
    list_of_plots[[i]]$mapping$colour <- suppressWarnings(ExtractParamfromPath(all_names[i]))
  }
  Reduce(f = `+`,x = list_of_plots,init = ggplot()) 
}

.overlap_event_plots <- function(list_of_plots) {
  plot_obj <- ggplot()
  count <- 0L
  for(single_layer in list_of_plots) {
    single_layer$mapping$ymin <- count
    count <- count + 1L
    single_layer$mapping$ymax <- count
    plot_obj <- plot_obj + single_layer
  }
  plot_obj
}

#' plots two or more ggplots one below another
#' @param gtl is a list of ggplot or gtable objects
#' @param ratio if not null, is a numeric vector of the same length as gtl. Plots are resized to this ratio.
.rbind_ggplots <- function(gtl, ratio=NULL){ 
  
  gtl <- lapply(gtl,function(x) {
    
    if(gtable::is.gtable(x)) return(x)
    ggplotGrob(x)
  })
  
  if(!is.null(ratio)) stopifnot(length(gtl)==length(ratio))
  
  num_cols <- vapply(gtl,FUN = ncol,FUN.VALUE = 0L)
  
  ## What exactly is this check for?
  if(length(unique(num_cols))!=1) stop("number of cols are not the same for all the ggplots!")
  
  max_widths <- Reduce(f = grid::unit.pmax,x = lapply(gtl,function(x) x$widths))
  
  # TODO : Use of cbind_ggplot
  # multiplying the heights using the ratio
  gtl <- mapply(x=gtl,y=ratio,FUN = function(x,y) {
    which_panels <- str_detect(string = x$layout$name,pattern = "panel")
    starts <- x$layout$t[which_panels]
    ends <- x$layout$b[which_panels]
    all_heights_req <- unique(vayu::seqC(from = starts,to = ends,by = 1L)$Sequence)
    for(i in all_heights_req){
      x$heights[i] =  x$heights[i] * y
    }
    x
  })
  
  all_grobs <- Reduce(f = append,x = lapply(gtl,function(x) x$grobs))
  
  all_heights <- Reduce(f = grid::unit.c,x = lapply(gtl,function(x) x$heights))
  
  all_rownames <- Reduce(f = c,x = lapply(gtl,function(x) x$rownames))
  
  num_rows <- unname(vapply(gtl,FUN = nrow,FUN.VALUE = 0L))
  num_rows <- c(0,num_rows[-length(num_rows)])
  num_rows <- cumsum(num_rows)
  gtl <- mapply(x=gtl,y=num_rows,function(x,y) {
    x$layout$t <- x$layout$t + y
    x$layout$b <- x$layout$b + y
    x
  })
  
  all_layout <- Reduce(f = rbind,x = lapply(gtl,function(x) x$layout))
  
  final_plot <- gtl[[1]]
  final_plot$layout <- all_layout
  final_plot$heights <- all_heights
  final_plot$rownames <- all_rownames
  final_plot$widths <- max_widths
  final_plot$grobs <- all_grobs
  
  final_plot
}

#' plots two or more ggplots by dividing the screen into vertical sections
#' @inheritParams .rbind_ggplots
.cbind_ggplots <- function(gtl){
  gtl <- lapply(gtl,function(x) {
    if(gtable::is.gtable(x)) return(x)
    ggplotGrob(x)
  })
  bind2 <- function (x, y) 
  {
    stopifnot(nrow(x) == nrow(y))
    if (ncol(x) == 0) 
      return(y)
    if (ncol(y) == 0) 
      return(x)
    y$layout$l <- y$layout$l + ncol(x)
    y$layout$r <- y$layout$r + ncol(x)
    x$layout <- rbind(x$layout, y$layout)
    x$widths <- gtable:::insert.unit(x$widths, y$widths)
    x$colnames <- c(x$colnames, y$colnames)
    x$heights <- grid::unit.pmax(x$heights, y$heights)
    x$grobs <- append(x$grobs, y$grobs)
    x
  }
  Reduce(bind2, gtl)
}

.match_grep <- function(grep_vec,actual_names) {
  
  grep_result <- vapply(X = names(grep_vec),FUN = str_detect,string=actual_names,USE.NAMES = FALSE,FUN.VALUE = logical(length(actual_names)))
  if(is.vector(grep_result)) grep_result <- matrix(grep_result,nrow = length(actual_names))
  one_match_check <- rowSums(grep_result)==1
  which_matches <- apply(X = grep_result[one_match_check,,drop=FALSE],MARGIN = 1,FUN = which)
  result_vec <- grep_vec[which_matches]
  names(result_vec) <- actual_names[one_match_check]
  result_vec
}

.match_grep_vec <- function(grep_vec, char_vec) {
  ans <- rep(x = NA_character_,times = length(grep_vec))
  if(length(char_vec)==0) return(ans)
  grep_result <- vapply(X = grep_vec,FUN = str_detect,string=char_vec,USE.NAMES = FALSE,FUN.VALUE = logical(length(char_vec)))
  if(is.vector(grep_result)) grep_result <- matrix(grep_result,nrow = length(char_vec))
  one_match_check <- colSums(grep_result)==1
  which_matches <- apply(X = grep_result,MARGIN = 2,FUN = which)
  ans[one_match_check] <- char_vec[which_matches]
  ans
}


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
#' @param xlabels change the labels on x-axis of plots
#' e.g: xlabel=c(ab="time",cd="count")
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
PlotDataItems <- function(timeline_df, dataGrep="", start_time=NULL, end_time=NULL,
                          invert = F, ylimits=NULL, scale_vals=NULL, titles=NULL, 
                          xlabels=NULL, ylabels=NULL, save_path = NULL, ifPlotAsSample=NULL,
                          returnGG=FALSE, add_legend=TRUE, event_plot_size=0.6,
                          overlap_plots=NULL, color_palette_manual = NULL) {
  
  # This function takes in a data.frame of format
  # |Timestamp|Event_A|Event_B|Sample_A|Sample_B|
  # |2011-01-01 00:00:00.000Z|Type_A|Type_B|10|20|
  # The data.frame should have one timestamp columns, and one or more state and numeric columns
  # state columns SHOULD be factors or characters.
  # numeric columns should be numeric

  
  # convert start and end into POSIXct objects
  if(!is.null(start_time)) start_time <- toPOSIXct(start_time)
  if(!is.null(end_time)) end_time <- toPOSIXct(end_time)
  
  # getting the length and input names
  # len_in <- length(input)
  # nam_in <- names(input)
  
  # grepping the required data items
  ts_col = names(timeline_df)[timeline_df %>% sapply(is.POSIXct)]
  which_grep <- names(timeline_df)[grep(data_grep, names(timeline_df), invert=invert)]
  timeline_df_subset <- timeline_df[union(ts_col, which_grep)]
  
  # function check for na and show warning and remove na
  
  state_cols = names(timeline_df)[timeline_df %>% sapply(is.character)]
  numeric_cols = names(timeline_df)[timeline_df %>% sapply(is.numeric)]
  
  #finding start and end time limits, only when the limits are not already specified
  time_range <- range(timeline_df_subset[[ts_col]])
  if(!is.null(start_time)) time_range[1] <- start_time
  if(!is.null(end_time))   time_range[2] <- end_time
  
  # function to subset data frame into time range
  timeline_df_subset_range = timeline_df_subset %>%
    filter(timeline_df_subset[[ts_col]] >= time_range[1], timeline_df_subset[[ts_col]] <= time_range[2])
  
  # TODO: scaleVals option is used here
  # Only value column is multipled. Should the name of the column to be multiplied be made an input?
  # Should be careful with this, if the value is event, it errors out
  if(!is.null(scaleVals)) {
    message("Scaling few DIs from 'scaleVals'")
    grep_match_result <- .match_grep(grep_vec = scaleVals,actual_names = names(timeline_df_subset_range))
    for(i in names(grep_match_result)) {
      DataList[[i]]$value <- DataList[[i]]$value * grep_match_result[i]
    }
  }
  
  
  # if names are not defined, creating names from the actual statement given for input 
  # We can remove the below and make it compulsory for all lists to be named and give default name to
  # unnamed lists 
  
  # if(is.null(nam_in) || any(nam_in %in% "")) {
  #   # TODO: Using the name of the input object passed, we are trying to name every item of the list, how can we use this?
  #   names(input) <- get_new_names(input_statement, nam_in, len_in)
  # }
  
  # combined_data_sor_list <- organize_input_to_df(nam_in, len_in, input)
  
  # SorE = combined_data_sor_list$SorE
  # data_list = combined_data_sor_list$data_list
  
  # which_grep <- grep(dataGrep, names(data_list), invert=invert)
  # data_list <- data_list[which_grep]
  # SorE <- SorE[which_grep]
  
  
  
  
  #finding start and end time limits, only when the limits are not already specified
  lims <- get_start_and_end_time_limits(start_time, end_time, data_list)
  
  if( !(is.null(start_time) && is.null(end_time)) ) {
    data_list <- filter_data_list_limits(data_list, lims)
  }
  
  # IFPLOTASSAMPLE option is used here. The name of the item of the list (name_othercols) should be passed as c(gegv = TRUE)
  # Only the items where all the column names other than timestamp has the 
  if(!is.null(ifPlotAsSample)) {
    SorE <- modify_for_sample_plot(SorE, data_list, ifPlotAsSample)
  }
  
  # scale_vals option is used here
  # Similar input type as of ifplotassample
  # Only value column is multipled. Should the name of the column to be multiplied be made an input?
  # Should be careful with this, if the value is event, it errors out
  if(!is.null(scale_vals)) {
    data_list <- scale_vals_data(data_list, scale_vals)
  }
  
  cleaned_data <- delete_empty_data_items(data_list, SorE)
  data_list <- cleaned_data$data_list
  SorE <- cleaned_data$SorE
  
  ## To be noted that in case of a list with ts,value and other columns, only value is taken
  ## i.e, the new list has form TS, Value with the name of the column already appended to the title.
  
  event_plots <- create_event_plots(data_list, SorE, lims)
  line_plots <- create_line_plots(data_list, SorE)  
  
  if(!is.null(overlap_plots)) {
    combined_plot_list <- create_overlap_plots(overlap_plots, line_plots, event_plots)
  } 
  else {
    combined_plot_list <- create_non_overlap_plots(line_plots, event_plots)
  }
  plot_type = combined_plot_list$plot_type
  all_plots = combined_plot_list$all_plots
  
  all_plots <- legendify_and_colorify_the_plots(all_plots, plot_type, add_legend, color_palette_manual)
  
  default_ylimits <- get_plot_limits(all_plots, plot_type, ylimits)
  all_plots <- mapply(FUN = function(x,y) x + coord_cartesian(xlim = lims,ylim = y),x=all_plots,y=default_ylimits,SIMPLIFY = FALSE,USE.NAMES = TRUE)
  
  all_plots <- add_titles_to_the_plot(all_plots, titles)
  all_plots <- add_labels_to_the_plot(all_plots, plot_type, xlabels, ylabels)
  
  ## Below function uses some intelligence to label X axis and also add x ticks with breaks
  all_plots <- sapply(X = all_plots,FUN = .AddExtrasToGGPlot,simplify = FALSE,USE.NAMES = TRUE,xrange=lims)
  
  if(returnGG) return(all_plots)
  
  align_and_draw_the_plots(all_plots, plot_type, event_plot_size, save_path)  
  #return filtered data invisibly
  message("Plotting DONE!!!")
  return(invisible(data_list))
}
