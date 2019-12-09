
[![codecov.io](https://codecov.io/github/alexsanjoseph/compareDF/coverage.svg?branch=master)](https://codecov.io/github/systeminsights/timelineR?branch=master)
[![Travis-CI Build Status](https://api.travis-ci.org/systeminsights/timelineR.svg?branch=master)](https://travis-ci.org/systeminsights/timelineR)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/timelineR)](https://cran.r-project.org/package=timelineR)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/timelineR)](https://www.r-pkg.org/pkg/timelineR)
![CRAN Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/timelineR)
![License](https://img.shields.io/badge/license-MIT%20License-blue.svg)
# timelineR: timeline  visualizations in R

This package helps in visualizing multi-variate time series data having numeric and factor variables. 

The main function to plot time series is `plot_timeline`

A data helper function `match_grep` is provided to extract names from large data frames


# Plot timelineR

The `plot_timeline` function in R helps visualize multi-variate 
time-series having numeric and state variables. The data used for
demonstration details the pollution levels in Delhi, which is downloaded from http://www.cpcb.gov.in/ .

In this package, the `futile:logger` package is used for logging since it provides 
a more granular control over the logging. This is useful to use the package in production
systems but you can treat the logs like normal R logs.

## Input data

For the package to work correctly, it expects the data to be structured in a specific way with a 
single timestamp column and one or more state and numeric variables. 

State variables are variables represented by `factor` or `character` columns, are categorical in nature.

Numeric variables are represented by `numeric` columns, are numeric and ordinal in nature.

Also, input data frame should have one column of the type `POSIXct` which represents the time of 
occurence of each observation. 

```{r}
library(dplyr, quietly = T)
data_path <- system.file("extdata/delhi_air_pollution.csv", package = "timelineR")
air_pollution <- read.csv(data_path) %>% mutate(date = as.POSIXct(date)) %>% filter(date < as.POSIXct("2016-05-01"))
air_pollution$date = as.POSIXct(air_pollution$date)
str(air_pollution)
```

## `plot_timeline`

### default plotting

The default configuration of the `plot_timeline` package plots univariate 
time series for all the variables in the order they appear 
in the data frame. It returns a `grob` object (`grid` package).

```{r, fig.width=8, fig.height=10}
require(timelineR)
plot_grob <- plot_timeline(air_pollution)
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-2-1.png" width="500" />

### Subset the variables to plot

The columns for which the data should be plotted can be subsetted 
by passing the names of the column in the argument `data_cols`. By default, 
the data for all the variables is plotted.

```{r, fig.width=8, fig.height=4}
data_cols = c("pm10", "no")
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols)
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-3-1.png" width="500" />

### Subset the time to plot

The time to plot can be subsetted by giving the start and the end times as values in the arguments `start_time` and `end_time` respectively.

```{r, fig.width=8, fig.height=4}
data_cols = c("pm10", "no")
start_time = as.POSIXct("2016-03-05")
end_time = as.POSIXct("2016-03-10")
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, start_time = start_time, end_time = end_time)
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-4-1.png" width="500" />


### Specify Y limits

The limit to plot on each axis can be passed as a named list with limits for the numeric plot

```{r, fig.width=8, fig.height=4}
data_cols = c("pm10", "no")
start_time = as.POSIXct("2016-03-05")
end_time = as.POSIXct("2016-03-10")
ylimits = list("pm10" = c(120,180))
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, start_time = start_time,
                           end_time = end_time, ylimits = ylimits)
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-5-1.png" width="500" />


### Scale numeric variables

To bring multiple numeric variables to a comparable level, they can be scaled. The scaling information is passed as a named vector with name as the name of the column and value as its corresponding multiplication factor.

```{r, fig.width=8, fig.height=5}
data_cols = c("pm10", "no", "odd_even")
scale_vals = c("pm10" = 0.5, "no" = 2)
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, scale_vals = scale_vals)
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-6-1.png" width="500" />


### Legend

For state variables, it can be specified if the legend is required or not. By default, the legends are shown

```{r, fig.width=8, fig.height=4}
data_cols = c("pm10", "odd_even")
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, add_legend = F)
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-7-1.png" width="500" />


### Color mapping

For changing the fill color values for the plot of state variables, a color mapping can be passed. Color mapping can be defined for one or more state variables. However for a given state variable, mapping should be defined for all the possible states in that variable.

The package internally uses the ggplot package and accepts color input in the format supported by `ggplot`

```{r, fig.width=8, fig.height=2}
data_cols = c("odd_even")
color_mapping = list("odd_even" = c("NORMAL" = "#E67E22", "ODD-EVEN" = "green4"))
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, color_mapping = color_mapping)
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-8-1.png" width="500" />


### Numeric plot type

The time series for data of numeric variables can be plotted in three ways:

* Line plot
* Step plot
* Point plot

The type of plot can be specified in the argument `numeric_plot_type`. Only three types are supported, hence the value for this argument should be one of `line`, `step` or `point`. By default, the plot type is `line`.

```{r, fig.width=8, fig.height=2}
data_cols = c("pm10")
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, numeric_plot_type = "step")
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-9-1.png" width="500" />


```{r, fig.width=8, fig.height=2}
data_cols = c("co")
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, numeric_plot_type = "point")
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-10-1.png" width="500" />


### Labeling the Y axis

By default, the label on Y-axis for the state and numeric plot is `State` and `Numeric` respectively. This can be changed by passing a name vector with names being the names of the plots and values as the name of the label.

```{r, fig.width=8, fig.height=6}
data_cols = c("co", "pm25", "odd_even")
ylabels = c("pm25" = "concentration", "odd_even" = "day type")
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, ylabels = ylabels)
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-11-1.png" width="500" />


### Option to show the plot

Many times it is required to not show the output of the current function. In that case, the plot can be drawn by passing the `plot_output` argument as `FALSE`. By default, the plot is drawn.

```{r, fig.width=8, fig.height=6}
data_cols = c("co", "pm25", "odd_even")
ylabels = c("pm25" = "concentration", "odd_even" = "day type")
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, ylabels = ylabels, plot_output = F)
```


### Overlapping plots

In time series data visualization, many times it is required to study the relation between a state variable and a numeric variable. For this, with the help of `plot_timeline`, it is possible to overlap the plot of numeric variable on that of a state variable.

To draw overlapping plots, a named list of vector is passed. Each name in the list is the name of the overlapping plot. Each element in the list is a vector of two elements with the first element as the name of the state variable and the second variable as the name of the numeric variable. Both the variables should be present in the `data_cols`.

```{r, fig.width=8, fig.height=6}
data_cols = c("pm25", "odd_even")
overlapping_plot_names = list("pm25_with_odd_even" = c("odd_even", "pm25"))
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, overlap_plots_names = overlapping_plot_names)
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-13-1.png" width="500" />


### Assigning titles to the plots

Each plot can be assigned a title. By default the title is the name of the variable for univariate plots and the name of the plot for the overlapping plots. The information is passed as a named vector with names as the name of the plot and value as the name of the title.

```{r, fig.width=8, fig.height=6}
data_cols = c("pm25", "odd_even")
titles = c("pm25" = "Concentration of particulate 2.5 matter", "pm25_with_odd_even" = "Study of concentration of PM 2.5 matter with odd-even policy")
overlapping_plot_names = list("pm25_with_odd_even" = c("odd_even", "pm25"))
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, overlap_plots_names = overlapping_plot_names, titles = titles)
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-14-1.png" width="500" />


### Ordering the plots

While visualizing data, it is preferred to have plots arranged in a required order for better understanding. The order is specified as a vector with the names of variables and overlapping plots arranged in the required order. Only the plots given in the argument `order_plot` are drawn.

By default the plots for univariate variables are arranged in the order they appear in the data frame followed by the overlapping plots.

```{r, fig.width=8, fig.height=6}
data_cols = c("pm25", "odd_even")
titles = c("pm25" = "Concentration of particulate 2.5 matter", "pm25_with_odd_even" = "Study of concentration of PM 2.5 matter with odd-even policy")
overlapping_plot_names = list("pm25_with_odd_even" = c("odd_even", "pm25"))
order_plots = c("pm25_with_odd_even", "pm25")
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, overlap_plots_names = overlapping_plot_names, titles = titles, order_plots = order_plots)
```
<img src="https://github.com/systeminsights/timelineR/raw/master/vignettes/figure/unnamed-chunk-15-1.png" width="500" />


### Relative size of the plots

To emphasie on some of the plots, the relative size of the plots can be adjusted. The relative sizes are passed as a named vector with tha name as the name of the plot and value as the relative ratio. By default each plot has relative size of 1.

```{r, fig.width=8, fig.height=7}
data_cols = c("pm25", "odd_even")
titles = c("pm25" = "Concentration of particulate 2.5 matter", "pm25_with_odd_even" = "Study of concentration of PM 2.5 matter with odd-even policy")
overlapping_plot_names = list("pm25_with_odd_even" = c("odd_even", "pm25"))
plot_size_ratios = c("pm25_with_odd_even" = 2, "odd_even" = 0.5)
plot_grob <- plot_timeline(air_pollution, data_cols = data_cols, overlap_plots_names = overlapping_plot_names, titles = titles, plot_size_ratios = plot_size_ratios)
```


### Saving the plots

It is possible to save the plot from the function. The name to be saved is passed as `save_path` argument. Only PNG format is supported.


# Auxiliary data helper function

To ease the procedure of extracting required names which are passed as arguments in the `plot_timeline` function, a helper function `match_grep` based on regular expression is provided for the same.

```{r}
data_path <- system.file("extdata/test_data.csv", package = "timelineR")
test_data <- read.csv(data_path)
test_data %>% str
```

## `match_grep`

The first argument `grep_vec` is the named vector which is to be searched. The second vector `actual_names` is the vector in which the search is to be named. A named vector is returned with the names as the matched names and values given in `grep_vec`.

```{r}
grep_vec = c("state" = 1, "num" = 2)
match_grep(grep_vec, names(test_data))
```

### Un named `grep_vec`

If it is required to search the values given in a vector then the argument `use_values` should be set as `TRUE`.

```{r}
grep_vec = c("state" , "num")
match_grep(grep_vec, names(test_data), use_values = T)
```

### Return the names

To return just the matched values from `actual_names` instead of the named vector, pass the argument `return_names` as `TRUE`.

```{r}
grep_vec = c("state" , "num")
match_grep(grep_vec, names(test_data), use_values = T, return_names = T)
```

### Show information of matched and unmatched names

To show for each value searched, what values in `actual_names` matched or not, the argument `echo` can be set to `TRUE`.

```{r}
grep_vec = c("state" , "num")
match_grep(grep_vec, names(test_data), use_values = T, return_names = T, echo = T)
```
