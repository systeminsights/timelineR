
generate_state_plot_layer <- function(data_to_plot) {
  start_values = data_to_plot[1:(nrow(data_to_plot) - 1), 1]
  end_values = data_to_plot[2:nrow(data_to_plot), 1]
  names(data_to_plot)[2] = "value"
  
  ggplot() + geom_rect(data=data_to_plot[-nrow(data_to_plot),],
                       aes(xmin = start_values, xmax = end_values,
                       ymin = 0L, ymax = 1L, fill = value)) 
}

generate_numeric_plot_layer <- function(data_to_plot, current_ylimit, line_width=0.3) {
  names(data_to_plot)[2] = "value"
  ggplot() + 
    geom_step(data=data_to_plot, aes_string(x = names(data_to_plot)[1], y = "value", group = 1), size = line_width) +
    coord_cartesian(ylim = current_ylimit)
    
}


create_state_plots <- function(timeline_cleaned, ts_col, state_cols){
  flog.info("creating state plot layers")
  
  state_plots <- sapply(state_cols, function(x) generate_state_plot_layer(timeline_cleaned[, c(ts_col, x)]),
                        simplify = F, USE.NAMES = T)
  
  #state_plots <- state_plots[!vapply(X = event_plots, FUN = is.null, FUN.VALUE = FALSE,USE.NAMES = FALSE)]
  return(state_plots)
}


create_numeric_plots <- function(timeline_cleaned, ts_col, numeric_cols, actual_ylimits){
  flog.info("creating sample plot layers")
  numeric_plots <- mapply(current_plot = numeric_cols, current_ylimit = actual_ylimits,
                          FUN = function(current_plot, current_ylimit)
                            generate_numeric_plot_layer(timeline_cleaned[, c(ts_col, current_plot)], current_ylimit),
                          SIMPLIFY = F, USE.NAMES = T)
  
  return(numeric_plots)
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
add_legend_to_plots <- function(all_plots, add_legend){
  
  all_plots <- lapply(all_plots, function(basic_plot) {
    basic_plot <- basic_plot + theme(panel.background = element_rect(fill="#EEEEEE", colour="black"),
                                     legend.justification=c(1,1),legend.direction = "horizontal",
                                     legend.title=element_blank())
    if(add_legend) {
      basic_plot <- basic_plot + theme(legend.position=c(1,1))
    } else {
      basic_plot <- basic_plot + theme(legend.position="none")
    }
    
    basic_plot
  })
  return(all_plots)
}


add_colors_to_state_plots <- function(all_plots, color_palette_manual){
  all_plots <- lapply(all_plots, function(basic_plot) {
    basic_plot <- basic_plot + scale_y_continuous(breaks=NULL)
    color_mapping <- map_values_to_colors(basic_plot$layers[[1]]$data, color_palette_manual)
    basic_plot <- basic_plot + scale_fill_manual(values=color_mapping)
    
    basic_plot  
  })
}

get_plot_limits <- function(timeline_cleaned, numeric_plots, ylimits){
  numeric_cols = names(timeline_df_subset_range)[timeline_df_subset_range %>% sapply(is.numeric)]
  
  default_ylimits <- rep(list(NULL), length(numeric_plots))
  names(default_ylimits) <- numeric_cols
  
  grep_match_result <- match_grep(grep_vec = ylimits, actual_names = numeric_cols)
  
  default_ylimits[names(grep_match_result)] = grep_match_result
  default_ylimits[numeric_cols]
}


add_titles_to_the_plot <- function(all_plots, titles){
  default_titles <- names(all_plots)
  names(default_titles) <- names(all_plots)
  
  if(!is.null(titles)) {
    message("Adding new plot titles")
    grep_match_result <- match_grep(grep_vec = titles,actual_names = names(all_plots))
    default_titles[names(grep_match_result)] <- grep_match_result
  }
  all_plots <- mapply(FUN = function(x,y) x + ggtitle(y), x=all_plots, y=default_titles, SIMPLIFY = FALSE,USE.NAMES = TRUE)
  return(all_plots)
}


add_ylabels_to_the_plot <- function(all_plots, ylabels, state_cols){
  default_ylabs <- sapply(names(all_plots), function(x){
    if(x %in% state_cols) return("State") else return("Numeric")
  })
  names(default_ylabs) <- names(all_plots)
  if(!is.null(ylabels)) {
    flog.info("Adding new y-labels")
    grep_match_result <- match_grep(grep_vec = ylabels, actual_names = names(all_plots))
    default_ylabs[names(grep_match_result)] <- grep_match_result
  }
  default_ylabs <- default_ylabs[names(all_plots)]
  all_plots <- mapply(FUN = function(x,y) x + ylab(y), x = all_plots, y = default_ylabs, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  return(all_plots)
}


align_and_draw_the_plots <- function(all_plots, numeric_cols, state_cols, event_plot_size, savePath){
  message("Aligning plots")
  
  sizelist <- c(rep(1, times = length(numeric_cols)), rep(event_plot_size, times = length(state_cols)))
  all_plots <- rbind_ggplots(all_plots,ratio = sizelist)
  
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

#' If need arises to add xlabel, they can be added as prefix by passing to this function
add_pretty_breaks_and_labels_to_one_oplot <- function(ggobject, prt_brks, xlabels){
  break_patterns = list(
    "Time (HH:MM:SS)"   = "^[[:digit:]]+:[[:digit:]]+:[[:digit:]]+$",
    "Time (HH:MM)"      = "^[[:digit:]]+:[[:digit:]]+$",
    "Time (s)"          = "^[[:digit:]]+$",
    "Time (Date HH:MM)" = "^[[:alpha:]]+ [[:digit:]]+$",
    "Time (Date)"       = "^[[:alpha:]]+ [[:digit:]]+$"
  )
  
  which_pattern = which(sapply(break_patterns, function(x) str_detect(xlabels[2], x)))
  if(length(which_pattern) == 0)  stop("Coudn't find break pattern!")
  
  xlabels[1] <- as.character(prt_brks[1])
  ggobject + xlab(names(which_pattern)[1]) + scale_x_datetime(breaks = prt_brks, labels = xlabels)
}

add_pretty_breaks_and_xlabel <- function(all_plots, xrange) {
  
  prt_brks <- base::pretty(n = 10,x = xrange)
  xlabels <- attr(prt_brks, "labels")

  sapply(X = all_plots, FUN = add_pretty_breaks_and_labels_to_one_oplot,
         prt_brks, xlabels, USE.NAMES = TRUE, simplify = F)
  
}


# TODO : cleanup and pass the color mapping as an argument
map_values_to_colors <- function(input, color_palette_manual = NULL) {
  
  color_palette <- c(green = "#6fc376", pink = "#f6928f", grey = "#c1c1c1", white = "#ffffff")
  if(!is.null(color_palette_manual))
    color_palette[1:length(color_palette_manual)] = color_palette_manual
  
  color_mapping <- c(`Data-Unavailabel` = "grey", `Non-Producing` = "pink", Producing = "green", `NA` = "white")
  
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
rbind_ggplots <- function(gtl, ratio=NULL){ 
  
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
    all_heights_req <- unique(seq(from = starts,to = ends,by = 1L))
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

give_match_status <- function(grep_result, actual_names){
  no_matches = rowSums(grep_result) < 1
  if(sum(no_matches))
    flog.info(sprintf("No matches found for scaling %s", paste(actual_names[no_matches], collapse = ", ")))
  
  multiple_matches = rowSums(grep_result) > 1
  if(sum(multiple_matches))
    flog.info(sprintf("Multiple matches found for scaling %s", paste(actual_names[multiple_matches], collapse = ", ")))
  
  rowSums(grep_result) == 1
}

match_grep <- function(grep_vec, actual_names) {
  
  grep_result <- vapply(X = names(grep_vec), FUN = stringr::str_detect, string=actual_names,
                        USE.NAMES = FALSE, FUN.VALUE = logical(length(actual_names)))
  if(is.vector(grep_result)) grep_result <- matrix(grep_result,nrow = length(actual_names))
  
  one_match_check = give_match_status(grep_result, actual_names)
  
  which_matches <- apply(X = grep_result[one_match_check, ,drop=FALSE], MARGIN = 1, FUN = which)
  result_vec <- grep_vec[which_matches]
  names(result_vec) <- actual_names[one_match_check]
  result_vec
}
