

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

#' removes each non-timestamp column in the input and keeps into a different DataItem object and then returns the list of DataItems
CreateDataItemlistFromDF <- function(mergedDF) {
  if(!(("timestamp" %in% names(mergedDF)) | all(c("start","end") %in% names(mergedDF))))
    return(returnNULLwithMessage("No required columns in mergedDF!"))

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
#' @param scaleVals used to multiply the values by a factor
#' e.g: scaleValse=c(a=10), matching data will be multiplied by 10
#' @param titles change titles of the plot
#' e.g: title=c(ab="first plot",cd="second plot")
#' @param xlabels change the labels on x-axis of plots
#' e.g: xlabel=c(ab="time",cd="count")
#' @param ylabels change the labels on y-axis of plots
#' e.g: ylabel=c(ab="value",bcd="tmeperature")
#' @param returnGG if TRUE, GGplot objects will be returned for further manual processing
#' @param addLegend TRUE (default) if legend is needed
#' @param eventPlotSize proportion of event plot size to the sample plot size
#' @param savePath if a file_path is specified, then the image will be saved to that location.
#' @param ifPlotAsSample for plotting event variables as sample overriding the default stripe chart behavior
#' e.g.: ifPlotAsSample=c(xyz=TRUE)
#' @param overlapPlots specify the data items to be overlapped. Plots of the same type can only be overlapped for now.
#' This argument can be used to specify the order of plots.
#' e.g.: overlapPlots="(abc,pqr),(pqr),(xyz,123,345)",
#' overlapPlots = "(S1speed),(path_feedrate1),(executi,CONTROLLER_MODE)"
#' @return list of the filtered data will be returned invisibly
PlotDataItems <- function(timeline_df, dataGrep="", start_time=NULL, end_time=NULL,
                          invert = F, ylimits=NULL, scaleVals=NULL, titles=NULL,
                          xlabels=NULL, ylabels=NULL, savePath = NULL,
                          returnGG=FALSE, addLegend=TRUE, eventPlotSize=0.6,
                          overlapPlots=NULL, color_palette_manual = NULL) {

  # TODO: This function is not able to directly plot single_device_mtc@DataItemlist, .rbind_ggplots failing

  # convert start and end into POSIXct objects - throw error if timestamp cannot be converted correctly
  if(!is.null(start_time)) start_time <- as.POSIXct(start_time)
  if(!is.null(end_time)) end_time <- as.POSIXct(end_time)

  # have function to convert factors to characters
  # Have functions to check if only one ts_col exists
  # we need exactly one timestamp, and one or more character or numeric

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

  #removing empty data items
  zero_row_items <- vapply(X=DataList,FUN=function(x) return((nrow(x)==0)||is.null(x)),FUN.VALUE=FALSE,USE.NAMES=FALSE)
  if(any(zero_row_items)) {
    message(paste0(names(DataList)[zero_row_items],collapse=", ")," are either NULL or have zero rows. Excluding these DIs.")
    DataList[zero_row_items] <- NULL
    SorE <- SorE[!zero_row_items]
  }


  ## TODO: To be noted that in case of a list with ts,value and other columns, only value is taken
  ## i.e, the new list has form TS, Value with the name of the column already appended to the title.

  message("creating event plot layers")
  eventplots <- sapply(X = DataList[SorE %in% "Event"],
                       simplify = FALSE,USE.NAMES = TRUE,
                       FUN = .generate_event_plot_layer,
                       end_time = lims[2]
  )
  message("creating sample plot layers")
  # TODO: Go through generate_sample_plot_layer
  lineplots <- sapply(X = DataList[(SorE%in%"Sample")|(SorE%in%"HSPMSample")],
                      simplify = FALSE,USE.NAMES = TRUE,
                      FUN = .generate_sample_plot_layer
  )

  lineplots <- lineplots[!vapply(X = lineplots,FUN = is.null,FUN.VALUE = FALSE,USE.NAMES = FALSE)]
  eventplots <- eventplots[!vapply(X = eventplots,FUN = is.null,FUN.VALUE = FALSE,USE.NAMES = FALSE)]


  # option OVERLAPPLOTS is used here
  # TODO: Omit from here to ...
  if(!is.null(overlapPlots)) {
    message("creating overlapping plots")
    extract_plots <- str_extract_all(string = overlapPlots,pattern = "\\([^\\(\\)]*\\)")[[1]]
    nam_line_p <- names(lineplots)
    nam_event_p <- names(eventplots)
    plots_already_used <- character(0)
    new_names <- character(0)
    plot_type <- character(0)
    overlapping_plots <- lapply(extract_plots,function(x) {
      x <- str_trim(str_split(string = x,pattern = "\\(|\\)|,")[[1]])
      x <- x[x %notin% ""]
      grep_result <- .match_grep_vec(x,nam_line_p)
      if(!anyNA(grep_result)) {
        plots_already_used <<- c(plots_already_used,grep_result)
        new_names <<- c(new_names,paste0(grep_result,collapse = "|"))
        plot_type <<- c(plot_type,"Sample")
        return(.overlap_sample_plots(lineplots[grep_result]))
      } else {
        grep_result <- .match_grep_vec(x,nam_event_p)
        if(!anyNA(grep_result)) {
          plots_already_used <<- c(plots_already_used,grep_result)
          new_names <<- c(new_names,paste0(grep_result,collapse = "|"))
          plot_type <<- c(plot_type,"Event")
          return(.overlap_event_plots(eventplots[grep_result]))
        } else {
          warning("matching didn't happen properly for overlapping plots!")
          return(NULL)
        }
      }
    })
    overlapping_plots <- overlapping_plots[!vapply(X = overlapping_plots,FUN = is.null,FUN.VALUE = FALSE,USE.NAMES = FALSE)]
    names(overlapping_plots) <- new_names
    plot_type <- c(plot_type,
                   rep("Sample",times = length(setdiff(nam_line_p,plots_already_used))),
                   rep("Event",times = length(setdiff(nam_event_p,plots_already_used)))
    )
    remaining_plots <- sapply(X = c(lineplots,eventplots)[setdiff(c(nam_line_p,nam_event_p),plots_already_used)],
                              FUN = function(x) {
                                ggplot() + x
                              },simplify = FALSE,USE.NAMES = TRUE)
    allplots <- c(overlapping_plots,remaining_plots)
    names(plot_type) <- names(allplots)
  }  # TODO: here.....
  else {
    allplots <- sapply(X = c(lineplots,eventplots),FUN = function(x) {
      ggplot() + x
    },simplify = FALSE,USE.NAMES = TRUE)
    plot_type <- c(rep(x = "Sample",times = length(lineplots)),rep(x = "Event",times = length(eventplots)))
    names(plot_type) <- names(allplots)
  }


  # TODO: We now have allplots which is a list of ggplots with automatic legend scales
  # TODO: Options for adding legend location details and specifying the colors in case of event plots
  # TODO: plot_type is used here which is just the titles as a named list
  # Check what color is given to those variables which does not exist in the scales
  # Use scale_manual as a layer for ggplot

  allplots <- mapply(basic_plot = allplots,ptype=plot_type,FUN = function(basic_plot,ptype) {


    basic_plot <- basic_plot + theme(panel.background = element_rect(fill="#EEEEEE", colour="black"),
                                     legend.justification=c(1,1),legend.direction = "horizontal",
                                     legend.title=element_blank())
    if(addLegend) {
      basic_plot <- basic_plot + theme(legend.position=c(1,1))
    } else {
      basic_plot <- basic_plot + theme(legend.position="none")
    }

    if(ptype %in% "Event") {
      # Y-axis ticks are remvoved for events
      basic_plot <- basic_plot + scale_y_continuous(breaks=NULL)
      # TODO: What is this below line for?
      if(length(basic_plot$layers) > 1) browser()
      # TODO: color_mapping is just a named list. Inside the below function, the usual color coding can
      # be hard-coded
      color_mappng <- .map_values_to_colors(basic_plot$layers[[1]]$data, color_palette_manual)
      basic_plot <- basic_plot + scale_fill_manual(values=color_mappng)
    }

    basic_plot
  },SIMPLIFY = FALSE,USE.NAMES = TRUE)

  # Should we arrange the labels in the legend as per any specific order?
  # TODO: Should legend be added only for one of the stripecharts instead of all of them?
  # This will not be a good idea if different stripecharts contain different values

  # x and y limits
  # event plots are given a std ylimit of c(0,1)
  event_plot_limits <- lapply(allplots[plot_type %in% "Event"],FUN = function(x) {
    c(0L,length(x$layers))
  })
  default_ylimits <- c(
    rep(x = list(NULL),times = sum(plot_type %in% "Sample")),
    event_plot_limits
  )
  names(default_ylimits) <- c(names(allplots)[plot_type %in% "Sample"],names(allplots)[plot_type %in% "Event"])
  # TODO: I don't think this is going to be useful, particulary when different data items have same column
  # name but different range of values
  if(!is.null(ylimits)) {
    grep_match_result <- .match_grep(grep_vec = ylimits,actual_names = names(allplots))
    grep_match_result <- grep_match_result[names(plot_type[plot_type %in% "Sample"])]
    for(i in names(grep_match_result)) {
      default_ylimits[[i]] <- grep_match_result[[i]]
    }
  }
  default_ylimits <- default_ylimits[names(allplots)]

  allplots <- mapply(FUN = function(x,y) x + coord_cartesian(xlim = lims,ylim = y),x=allplots,y=default_ylimits,SIMPLIFY = FALSE,USE.NAMES = TRUE)

  default_titles <- names(allplots)
  names(default_titles) <- names(allplots)

  # This option can be retained.
  if(!is.null(titles)) {
    message("Adding new plot titles")
    grep_match_result <- .match_grep(grep_vec = titles,actual_names = names(allplots))
    default_titles[names(grep_match_result)] <- grep_match_result
  }
  allplots <- mapply(FUN = function(x,y) x+ggtitle(y),x=allplots,y=default_titles,SIMPLIFY = FALSE,USE.NAMES = TRUE)

  if(!is.null(xlabels)) {
    message("Adding new x-labels")
    grep_match_result <- .match_grep(grep_vec = xlabels,actual_names = names(allplots))
    allplots[names(grep_match_result)] <-
      mapply(FUN = function(x,y) x+xlab(y),x=allplots[names(grep_match_result)],y=unname(grep_match_result),SIMPLIFY = FALSE,USE.NAMES = TRUE)
  }

  default_ylabs <- c("Sample"="Value","Event"="State")[plot_type]
  names(default_ylabs) <- names(plot_type)
  if(!is.null(ylabels)) {
    message("Adding new y-labels")
    grep_match_result <- .match_grep(grep_vec = ylabels,actual_names = names(allplots))
    default_ylabs[names(grep_match_result)] <- grep_match_result
  }
  default_ylabs <- default_ylabs[names(allplots)]
  allplots <- mapply(FUN = function(x,y) x+ylab(y),x=allplots,y=default_ylabs,SIMPLIFY = FALSE,USE.NAMES = TRUE)


  ## TODO: Below function uses some intelligence to label X axis and also add x ticks with breaks
  allplots <- sapply(X = allplots,FUN = .AddExtrasToGGPlot,simplify = FALSE,USE.NAMES = TRUE,xrange=lims)

  if(returnGG) return(allplots)

  message("Aligning plots")
  # size of 1 is hardcoded for samples
  # Note: You are yet to read sample_layer plot
  sizelist <- c( rep(1,times=sum(plot_type %in% "Sample")), rep(eventPlotSize,times=sum(plot_type %in% "Event")) )
  # browser()
  allplots <- .rbind_ggplots(allplots,ratio = sizelist)

  if(!is.null(savePath)) {
    message("Writing image to file as PNG in: ",  savePath)
    png(savePath, width = 1500, height = 800)
    grid::grid.draw(allplots)
    dev.off()
  }

  if(is.null(savePath)) {
    message("Plotting")
    grid::grid.draw(allplots)
  }

  #return filtered data invisibly
  message("Plotting DONE!!!")
  return(invisible(DataList))
}

#' @title Multiple plot function
#' @param ... ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' @param cols Number of columns in layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#' @description
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
Multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      ggplot2:::print.ggplot(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                       layout.pos.col = matchidx$col))
    }
  }
}

plot_time_series <- function(list_of_dfs) {
  ggobj <- ggplot()
  y_value <- 0
  for(i in seq_along(list_of_dfs)) {
    req_cols <- setdiff(names(list_of_dfs[[i]]),c("start","end"))
    for(j in req_cols) {
      ggobj <- ggobj + geom_rect(data=list_of_dfs[[i]],aes_string(xmin="start",xmax="end",ymin=y_value,ymax=y_value+1,fill=j))
      y_value <- y_value + 1
    }
    y_value <- y_value + 1
  }
  print(ggobj)
}

.generate_event_plot_layer <- function(data_to_plot, end_time=NULL) {

  if( "start" %notin% names(data_to_plot) ) {
    if(is.unsorted(data_to_plot$timestamp))
      data_to_plot <- data_to_plot[order(data_to_plot$timestamp,na.last=FALSE),]
    if(is.null(end_time)) {
      all_times <- c(data_to_plot$timestamp,toPOSIXct("2100-01-01"))
    } else {
      all_times <- c(data_to_plot$timestamp,toPOSIXct(end_time))
    }
    data_to_plot$timestamp <- NULL
    data_to_plot$start <- all_times[-length(all_times)]
    data_to_plot$end <- all_times[-1L]
  }

  if( anyNA(data_to_plot$start) || anyNA(data_to_plot$end) )
    stop("NAs found in time columns!")

  geom_rect(data=data_to_plot,aes(xmin = start, xmax = end, ymin = 0L, ymax = 1L,fill=value))
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

  color_palette <- c(Blue="#2D2DB2", Red="#B22D2D", Green="#2BAD2B",
                     Orange="#E59100", Gray="#545454", Yellow="#FFFF33",
                     Brown="#A65628", Purple="#CC79A7",
                     "#33FFB2","#7A97CC", "#82656A", "#454545",
                     "#00195E", "#37E900", "#D2D22B", "#2B2700")
  if(!is.null(color_palette_manual))
    color_palette[1:length(color_palette_manual)] = color_palette_manual

  color_mapping <- c(ACTIVE="Green", AVAILABLE="Green", AUTOMATIC="Green",
                     UNAVAILABLE="Gray", Unavailable="Gray",
                     STOPPED="Red", MANUAL="Red",
                     READY="Orange", SEMI_AUTOMATIC="Orange",
                     INTERRUPTED="Blue", MANUAL_DATA_INPUT="Blue")

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
      return(returnNULLwithMessage("NAs found in the value column. Not Plotting this particular data."))
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
    all_heights_req <- unique(seqC(from = starts,to = ends,by = 1L)$Sequence)
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
  browser()
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
