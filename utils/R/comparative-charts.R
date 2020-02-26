library(dplyr)
library(ggplot2)
library(stringr)

# DATA

### The Most straightforward way of running this script is from "witan.send/utils/R/"
### Output directory must be the root directory of the clients repo. Results will be 
### stored in a directory called comparisons within the output directory. The path
### to historical transitions should be within the output dir.
### data1 and data2 correspond to the folders containing the 2 data sets you 
### want to compare.
### Each data folder must contain all results of a witan.send model run
### The code will extract what the name of the folder, and will use this for 
### labelling the plot.
### example:
### `> Rscript comparative-charts.R "~/home/repo/" "data/transitions.csv" 
###       "first-results/" "second-results/"`

args = commandArgs(trailingOnly=TRUE)
output_dir = args[1]
historical_transitions = args[2]
data1 = args[3]
data2 = args[4]

dir.create(paste0(output_dir, "comparisons"))

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
source("comparative-plot-cost.R")
comparative_plot_cost(output_dir, data1, data2)

## population count
source("comparative-plot-population.R")
comparative_plot_population(output_dir, historical_transitions, data1, data2)

# setting counts
source("comparative-plot-settings-needs.R")
comparative_plot_settings_needs(output_dir, data1, data2)
