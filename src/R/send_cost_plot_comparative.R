# SEND total cost boxplot - comparative
# equivalent to send-cost-plot in charts.clj

library(dplyr)
library(ggplot2)

# INPUTS

# colour palette for plots

## palette Henry used for other ribbon plots
palette = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#D55E00", "#CC79A7")

## colourblid palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols = cbPalette[c(1,2)]


# alphas
alpha_line = 0.5
alpha_ribbon = 0.2


# DATA

### import the baseline dataset and convert to millions
cost_data_baseline = read.csv("/Users/sunnyt/Documents/Mastodon C/SEND working folder/total cost/TH-VersionA-baseline-29-03-18-Cost.csv")
cost_data_baseline[,-1] =  cost_data_baseline[,-1] / 1000000
cost_data_baseline$group = "baseline"

### import the scenario dataset and convert to millions
cost_data_scenario = read.csv("/Users/sunnyt/Documents/Mastodon C/SEND working folder/total cost/TH-VersionA-scenario1-27-04-18-Cost.csv")
cost_data_scenario[,-1] =  cost_data_scenario[,-1] / 1000000
cost_data_scenario$group = "scenario"


# PLOTS

## ribbon plot - not zero-indexed

g <- ggplot(cost_data_baseline, aes(x=calendar.year)) +
  geom_line(aes(y=mean), colour=cols[1], alpha = alpha_line) +
  geom_ribbon(aes(ymin=q1, ymax=q3), fill=cols[1], alpha = alpha_ribbon)  +
  geom_ribbon(aes(ymin=q1, ymax=q3), fill=cols[1], alpha = alpha_ribbon) +
  geom_line(data=cost_data_scenario, aes(y=mean), colour=cols[2], alpha = alpha_line) +
  geom_ribbon(data=cost_data_scenario, aes(ymin=q1, ymax=q3), fill=cols[2], alpha = alpha_ribbon) +
  labs(y = "Total Projected SEND Cost / £ million") +
  scale_x_continuous(name="Year",
                     breaks = seq(min(cost_data_baseline$calendar.year), max(cost_data_baseline$calendar.year)),
                     limits = c(min(cost_data_baseline$calendar.year), max(cost_data_baseline$calendar.year)+1)) +
  ggtitle("SEND Cost Projection") +
  theme(axis.text.x = element_text(size=8)) +
  geom_text(x=2027+0.8,
           y=cost_data_baseline$mean[cost_data_baseline$calendar.year==2027],
           label= "Baseline",
           colour = cols[1]) +
  geom_text(x=2027+0.8,
           y=cost_data_scenario$mean[cost_data_scenario$calendar.year==2027],
           label= "Scenario A",
           colour = cols[2]) +
  theme(legend.position="none")

print(g)

ggsave("target/Total_Cost_Comparative.pdf",
       width=8,
       height=6,
       units="in")



## ribbon plot - zero-indexed

g + scale_y_continuous(limits = c(0, max(cost_data_baseline$q3, na.rm=T)))

ggsave("target/Total_Cost_Comparative_Zeroindexed.pdf",
       width=8,
       height=6,
       units="in")
