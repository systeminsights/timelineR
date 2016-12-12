create_event_plots <- function(data_list, SorE, lims){
  message("creating event plot layers")
  event_plots <- sapply(X = data_list[SorE %in% "Event"], 
                        simplify = FALSE,USE.NAMES = TRUE, 
                        FUN = .generate_event_plot_layer,
                        end_time = lims[2]
  )
  event_plots <- event_plots[!vapply(X = event_plots,FUN = is.null,FUN.VALUE = FALSE,USE.NAMES = FALSE)]
  return(event_plots)
}


create_line_plots <- function(data_list, SorE){
  message("creating sample plot layers")
  # TODO: Go through generate_sample_plot_layer
  line_plots <- sapply(X = data_list[(SorE%in%"Sample")|(SorE%in%"HSPMSample")],
                       simplify = FALSE,USE.NAMES = TRUE, 
                       FUN = .generate_sample_plot_layer
  )
  
  line_plots <- line_plots[!vapply(X = line_plots,FUN = is.null,FUN.VALUE = FALSE,USE.NAMES = FALSE)]
  return(line_plots)
}


get_overlapping_plots <- function(extract_plots, nam_line_p, plots_already_used,
                                  plot_type, new_names){
  overlapping_plots <- lapply(extract_plots,function(x) {
    x <- str_trim(str_split(string = x,pattern = "\\(|\\)|,")[[1]])
    x <- x[x %notin% ""]
    grep_result <- .match_grep_vec(x,nam_line_p)
    if(!anyNA(grep_result)) {
      plots_already_used <<- c(plots_already_used,grep_result)
      new_names <<- c(new_names,paste0(grep_result,collapse = "|"))
      plot_type <<- c(plot_type,"Sample")
      return(.overlap_sample_plots(line_plots[grep_result]))
    } else {
      grep_result <- .match_grep_vec(x,nam_event_p)
      if(!anyNA(grep_result)) {
        plots_already_used <<- c(plots_already_used,grep_result)
        new_names <<- c(new_names,paste0(grep_result,collapse = "|"))
        plot_type <<- c(plot_type,"Event")
        return(.overlap_event_plots(event_plots[grep_result]))
      } else {
        warning("matching didn't happen properly for overlapping plots!")
        return(NULL)
      }
    }
  })
  overlapping_plots <- overlapping_plots[!vapply(X = overlapping_plots,FUN = is.null,FUN.VALUE = FALSE,USE.NAMES = FALSE)]
  names(overlapping_plots) <- new_names
  combined_plot_list <- list(plots_already_used = plots_already_used,
                             new_names = new_names,
                             plot_type = plot_type,
                             overlapping_plots = overlapping_plots)
  return(combined_plot_list)
} 

create_overlap_plots <- function(overlap_plots, line_plots, event_plots){
  message("creating overlapping plots")
  extract_plots <- str_extract_all(string = overlap_plots,pattern = "\\([^\\(\\)]*\\)")[[1]]
  nam_line_p <- names(line_plots)
  nam_event_p <- names(event_plots)
  plots_already_used <- character(0)
  new_names <- character(0)
  plot_type <- character(0)
  
  combined_plot_list <- get_overlapping_plots(extract_plots, nam_line_p, plots_already_used,
                                              plot_type, new_names)
  overlapping_plots <- combined_plot_list$overlapping_plots
  new_names <- combined_plot_list$new_names
  plots_already_used <- combined_plot_list$plots_already_used
  plot_type <- combined_plot_list$plot_type
  
  plot_type <- c(plot_type,
                 rep("Sample",times = length(setdiff(nam_line_p,plots_already_used))),
                 rep("Event",times = length(setdiff(nam_event_p,plots_already_used)))
  )
  remaining_plots <- sapply(X = c(line_plots,event_plots)[setdiff(c(nam_line_p,nam_event_p),plots_already_used)],
                            FUN = function(x) {
                              ggplot() + x
                            },simplify = FALSE,USE.NAMES = TRUE)
  all_plots <- c(overlapping_plots,remaining_plots)
  names(plot_type) <- names(all_plots)
  return(list(all_plots = all_plots, plot_type = plot_type))
}

create_non_overlap_plots <- function(line_plots, event_plots){
  all_plots <- sapply(X = c(line_plots,event_plots),FUN = function(x) {
    ggplot() + x
  },simplify = FALSE,USE.NAMES = TRUE)
  plot_type <- c(rep(x = "Sample",times = length(line_plots)),rep(x = "Event",times = length(event_plots)))
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
    grid::grid.draw(all_plots)
    dev.off() 
  }
  
  if(is.null(savePath)) {
    message("Plotting")
    grid::grid.draw(all_plots)
  }
  
}
