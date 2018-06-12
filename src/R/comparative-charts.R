library(dplyr)
library(ggplot2)


# DATA

## path to the folder containing the 2 data sets you want to compare. 
### The code expects the folder and the data to be in specific forms and places: 
### - the data must be placed inside the data/ folder
### - the data must be csvs
### - the data must take the same name as the folder (i.e. TH-VersionA-baseline-29-03-18)
### - plus an additional name-part for the type of data (e.g. -Cost, -Count) followed by the .csv extension
### The code will extract what type of run the data comes from (e.g baseline, Scenario 1) the name of the folder, and will use this for
### labelling the plot
data1_folder = "data/TH-VersionA-baseline-29-03-18"
data2_folder = "data/TH-VersionA-scenario1-27-04-18"


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
send_cost_plot_comparative(data1_folder, data2_folder)

## population count
source("src/R/population_line_plot_comparative.R") 
population_count_plot_comparative(data1_folder, data2_folder)
