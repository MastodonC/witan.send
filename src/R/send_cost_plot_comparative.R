# SEND total cost boxplot - comparative
# equivalent to send-cost-plot in charts.clj

library(dplyr)
library(ggplot2)

## data

### import the baseline dataset and convert to millions
cost_data_baseline = read.csv("/Users/sunnyt/Documents/Mastodon C/SEND working folder/total cost/TH-VersionA-baseline-29-03-18-Cost.csv")
cost_data_baseline[,-1] =  cost_data_baseline[,-1] / 1000000
cost_data_baseline$group = "baseline"

### import the scenario dataset and convert to millions
cost_data_scenario = read.csv("/Users/sunnyt/Documents/Mastodon C/SEND working folder/total cost/TH-VersionA-scenario1-27-04-18-Cost.csv")
cost_data_scenario[,-1] =  cost_data_scenario[,-1] / 1000000
cost_data_scenario$group = "scenario"

### combine into single dataset for input into plot
cost_data = bind_rows(cost_data_baseline, cost_data_scenario)


## boxplot

# ggplot(cost_data, aes(x=calendar.year, fill=group)) +
#   geom_boxplot(aes(lower = q1,
#                    upper = q3,
#                    middle = median,
#                    ymin = high.ci,
#                    ymax = low.ci),
#                stat = "identity",
#                position=position_dodge(0.6)) +
#   scale_y_continuous(name = "Total Projected SEND Cost / £ million",
#                      limits = c(0, max(cost_data$high.ci))) +
#   scale_x_continuous(name="Year",
#                      breaks = seq(min(cost_data$calendar.year), max(cost_data$calendar.year)),
#                      limits = c(min(cost_data$calendar.year), max(cost_data$calendar.year))) +
#   theme_bw() +
#   ggtitle("SEND Cost Projection") +
#   scale_fill_discrete("")


## ribbon plot version

ggplot(cost_data, aes(x=calendar.year)) +
  geom_line(aes(y=mean, colour=group)) +
  geom_ribbon(aes(ymin=q1, ymax=q3, fill=group), alpha=0.2)  +
  scale_y_continuous(name = "Total Projected SEND Cost / £ million",
                     limits = c(min(cost_data$mean), max(cost_data$high.ci)))+
  scale_x_continuous(name="Year",
                     breaks = seq(min(cost_data$calendar.year), max(cost_data$calendar.year)),
                     limits = c(min(cost_data$calendar.year), max(cost_data$calendar.year)+1)) +
  theme(axis.text.x = element_text(size=8)) +
  ggtitle("SEND Cost Projection") +
#  scale_color_manual(values=c("cornflowerblue", "orange1")) +
  annotate("text", x=2027+0.8,
           y=cost_data_baseline$mean[cost_data_baseline$calendar.year==2027],
           label= "Baseline") + 
  annotate("text", x=2027+0.8,
           y=cost_data_scenario$mean[cost_data_scenario$calendar.year==2027],
           label= "Scenario A") +
  theme(legend.position="none")
