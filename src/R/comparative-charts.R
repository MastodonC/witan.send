library(dplyr)
library(ggplot2)


# DATA

## cost
baseline_cost_data = "data/total_cost/TH-VersionA-baseline-29-03-18-Cost.csv"
scenario_cost_data = "data/total_cost/TH-VersionA-scenario1-27-04-18-Cost.csv"

## population count
baseline_pop_data = "data/total_population/TH-VersionA-baseline-29-03-18-Count.csv"
scenario_pop_data = "data/total_population/TH-VersionA-scenario1-27-04-18-Count.csv"


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
source("src/R/send_cost_plot_comparative.R") 
send_cost_plot_comparative(baseline_cost_data, scenario_cost_data)

## population count
source("src/R/population_line_plot_comparative.R") 
population_count_plot_comparative(baseline_pop_data, scenario_pop_data)
