# SEND total cost plot
# equivalent to send-cost-plot in charts.clj

library(dplyr)
library(ggplot2)

## data

### import the data and convert to millions
cost_data = read.csv("/Users/sunnyt/Documents/Mastodon C/witan.send/data/TH-VersionA-baseline-29-03-18/TH-VersionA-baseline-29-03-18-Cost.csv")
cost_data[,-1] =  cost_data[,-1] / 1000000

## helpers for plots
min_x = min(cost_data$calendar.year)
max_x = max(cost_data$calendar.year)


## boxplot

### not zero-indexed

b <- ggplot(cost_data, aes(x=calendar.year)) +
  geom_boxplot(aes(lower = q1,
                   upper = q3,
                   middle = median,
                   ymin = low.ci,
                   ymax = high.ci),
               stat = "identity",
               position=position_dodge(0.6)) +
  labs(name = "Total Projected SEND Cost / £ million") +
  scale_x_continuous(name="Year",
                     breaks = seq(min_x, max_x),
                     limits = c(min_x-0.5, max_x+0.5)) +
  ggtitle("SEND Cost Projection") +
  theme_bw() +
  theme(axis.text.x = element_text(size=8))

print(b)

### zero-indexed

b + scale_y_continuous(limits = c(0, max(cost_data$high.ci)))



## ribbon plot version

### non zero-indexed
r <- ggplot(cost_data, aes(x=calendar.year)) +
  geom_line(aes(y=mean)) +
  geom_ribbon(aes(ymin=q1, ymax=q3), alpha=0.2)  +
  labs(y = "Total Projected SEND Cost / £ million") +
  scale_x_continuous(name="Year",
                     breaks = seq(min_x, max_x),
                     limits = c(min_x-0.5, max_x+0.5)) +
  ggtitle("SEND Cost Projection") +
  theme_bw() +
  theme(axis.text.x = element_text(size=8))

print(r)

### zero indexed

r + scale_y_continuous(limits = c(0, max(cost_data$high.ci)))
