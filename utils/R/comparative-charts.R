library(dplyr)
library(ggplot2)


# DATA

## names of the folders containing the 2 data sets you want to compare.
### The code expects the folder and the data to be in specific forms and places:
### Each folder must contain all results of a witan.send model run
### The code will extract what type of run the data comes from (e.g baseline, Scenario 1) the name of the folder, and will use this for
### labelling the plot
data_folder = "/Users/Seb/code/witan.send/data/demo/"
data1 = "results-alt/"
data2 = "results/"


# INPUTS

## alphas for lines of the mean values, and interquartile range area (ribbons)
alpha_line = 0.5
alpha_ribbon = 0.2

## colour palette for plots

### colourblind palette
### taken from here http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
### 2nd and 6th colours seem to be liked internally and pass colourblind contrast test:
### http://www.color-blindness.com/coblis-color-blindness-simulator/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### palette Henry used for other ribbon plots
palette = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#D55E00", "#CC79A7")

### used in figures:
cols = cbPalette[c(2,6)]


# PLOTS

## cost
source("utils/R/comparative-plot-cost.R")
comparative_plot_cost(data_folder, data1, data2)

## population count
source("utils/R/comparative-plot-population.R")
comparative_plot_population(data_folder, data1, data2)
