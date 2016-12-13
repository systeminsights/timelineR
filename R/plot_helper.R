
generate_state_plot_layer <- function(data_to_plot) {
  start_values = data_to_plot[1:(nrow(data_to_plot) - 1), 1]
  end_values = data_to_plot[2:nrow(data_to_plot), 1]
  names(data_to_plot)[2] = "value"
  
  ggplot() + geom_rect(data=data_to_plot[-nrow(data_to_plot),],
            aes_string(xmin = "start_values", xmax = "end_values",
                       ymin = 0L, ymax = 1L, fill = "value")) 
}

generate_numeric_plot_layer <- function(data_to_plot, line_width=0.3) {
  names(data_to_plot)[2] = "value"
  ggplot() + geom_step(data=data_to_plot, 
            aes_string(x = names(data_to_plot)[1], y = "value", group = 1), size = line_width)
}


create_state_plots <- function(timeline_cleaned, ts_col){
  flog.info("creating state plot layers")
  state_cols = names(timeline_cleaned)[timeline_cleaned %>% sapply(is.character)]
  state_plots <- lapply(state_cols, function(x) generate_state_plot_layer(timeline_cleaned[, c(ts_col, x)]))
  
  #state_plots <- state_plots[!vapply(X = event_plots, FUN = is.null, FUN.VALUE = FALSE,USE.NAMES = FALSE)]
  return(state_plots)
}


create_numeric_plots <- function(timeline_cleaned, ts_col){
  flog.info("creating sample plot layers")
  numeric_cols = names(timeline_cleaned)[timeline_cleaned %>% sapply(is.numeric)]
  numeric_plots <- lapply(numeric_cols, function(x) generate_numeric_plot_layer(timeline_cleaned[, c(ts_col, x)]))
  
  return(numeric_plots)
}


get_overlapping_plots <- function(){
 
}

create_non_overlap_plots <- function(numeric_plots, state_plots){
  all_plots <- lapply(X = c(numeric_plots, state_plots),FUN = function(x) {
    ggplot() + x
  })
  plot_type <- c(rep("Numeric", length(numeric_plots)),rep("State", length(state_plots)))
  names(plot_type) <- names(all_plots)
  return(list(all_plots = all_plots, plot_type = plot_type))
}

#  We now have all_plots which is a list of ggplots with automatic legend scales
#  Options for adding legend location details and specifying the colors in case of event plots
#  plot_type is used here which is just the titles as a named list
# Check what color is given to those variables which does not exist in the scales
# Use scale_manual as a layer for ggplot
legendify_and_colorify_the_plots <- function(all_plots, plot_type, add_legend, color_palette_manual){
  all_plots <- mapply(basic_plot = all_plots,ptype=plot_type,FUN = function(basic_plot,ptype) {
    basic_plot <- basic_plot + theme(panel.background = element_rect(fill="#EEEEEE", colour="black"),
                                     legend.justification=c(1,1),legend.direction = "horizontal",
                                     legend.title=element_blank())
    if(add_legend) {
      basic_plot <- basic_plot + theme(legend.position=c(1,1))
    } else {
      basic_plot <- basic_plot + theme(legend.position="none")
    }
    
    if(ptype %in% "Event") {
      # Y-axis ticks are remvoved for events
      basic_plot <- basic_plot + scale_y_continuous(breaks=NULL)
      # color_mapping is just a named list. Inside the below function, the usual color coding can
      # be hard-coded
      color_mappng <- .map_values_to_colors(basic_plot$layers[[1]]$data, color_palette_manual)
      basic_plot <- basic_plot + scale_fill_manual(values=color_mappng)
    }
    
    basic_plot
  },SIMPLIFY = FALSE,USE.NAMES = TRUE)
  return(all_plots)
}

# x and y limits
# event plots are given a std ylimit of c(0,1)
get_plot_limits <- function(all_plots, plot_type, ylimits){
  event_plot_limits <- lapply(all_plots[plot_type %in% "Event"],FUN = function(x) {
    c(0L,length(x$layers))
  })
  default_ylimits <- c(
    rep(x = list(NULL),times = sum(plot_type %in% "Sample")),
    event_plot_limits
  )
  names(default_ylimits) <- c(names(all_plots)[plot_type %in% "Sample"],names(all_plots)[plot_type %in% "Event"])
  # TODO: I don't think this is going to be useful, particulary when different data items have same column 
  # name but different range of values
  if(!is.null(ylimits)) {
    grep_match_result <- .match_grep(grep_vec = ylimits,actual_names = names(all_plots))
    grep_match_result <- grep_match_result[names(plot_type[plot_type %in% "Sample"])]
    for(i in names(grep_match_result)) {
      default_ylimits[[i]] <- grep_match_result[[i]]
    }
  }
  default_ylimits <- default_ylimits[names(all_plots)]
  return(default_ylimits)
}


add_titles_to_the_plot <- function(all_plots, titles){
  default_titles <- names(all_plots)
  names(default_titles) <- names(all_plots)
  
  # This option can be retained. 
  if(!is.null(titles)) {
    message("Adding new plot titles")
    grep_match_result <- .match_grep(grep_vec = titles,actual_names = names(all_plots))
    default_titles[names(grep_match_result)] <- grep_match_result
  }
  all_plots <- mapply(FUN = function(x,y) x+ggtitle(y), x=all_plots, y=default_titles, SIMPLIFY = FALSE,USE.NAMES = TRUE)
  return(all_plots)
}


add_labels_to_the_plot <- function(all_plots, plot_type, xlabels, ylabels){
  if(!is.null(xlabels)) {
    message("Adding new x-labels")
    grep_match_result <- .match_grep(grep_vec = xlabels,actual_names = names(all_plots))
    all_plots[names(grep_match_result)] <- 
      mapply(FUN = function(x,y) x+xlab(y),x=all_plots[names(grep_match_result)],y=unname(grep_match_result),SIMPLIFY = FALSE,USE.NAMES = TRUE)
  }
  
  default_ylabs <- c("Sample"="Value","Event"="State")[plot_type]
  names(default_ylabs) <- names(plot_type)
  if(!is.null(ylabels)) {
    message("Adding new y-labels")
    grep_match_result <- .match_grep(grep_vec = ylabels,actual_names = names(all_plots))
    default_ylabs[names(grep_match_result)] <- grep_match_result
  }
  default_ylabs <- default_ylabs[names(all_plots)]
  all_plots <- mapply(FUN = function(x,y) x+ylab(y),x=all_plots,y=default_ylabs,SIMPLIFY = FALSE,USE.NAMES = TRUE)
  return(all_plots)
}


align_and_draw_the_plots <- function(all_plots, plot_type, event_plot_size, savePath){
  message("Aligning plots")
  
  # size of 1 is hardcoded for samples
  # Note: You are yet to read sample_layer plot
  sizelist <- c( rep(1,times=sum(plot_type %in% "Sample")), rep(event_plot_size,times=sum(plot_type %in% "Event")) )
  all_plots <- .rbind_ggplots(all_plots,ratio = sizelist)
  
  if(!is.null(savePath)) {
    message("Writing image to file as PNG in: ",  savePath)
    png(savePath, width = 1500, height = 800)
    grid::grid.draw(combined_plot_list)
    dev.off() 
  }
  
  if(is.null(savePath)) {
    message("Plotting")
    grid::grid.draw(all_plots)
  }
  
}

