# SEND total cost boxplot
# equivalent to send-cost-plot in charts.clj

cost_data = read.csv("target/output-cost.csv")

# scale to millions
cost_data[,-1] = cost_data[,-1]/1000000

# boxplot
ggplot(data=cost_data, aes(x=calendar.year)) +
  geom_boxplot(aes(lower = q1,
                   upper = q3,
                   middle = median,
                   ymin = high.ci,
                   ymax = low.ci),
               stat = "identity",
               fill = "snow",
               colour = "darkcyan") +
  scale_y_continuous(name = "Total Projected SEND Cost / £ million",
                     limits = c(0, max(cost_data$high.ci))) +
  scale_x_continuous(name="Year", 
                     breaks = seq(min(cost_data$calendar.year), max(cost_data$calendar.year)),
                     limits = c(min(cost_data$calendar.year)-0.5, max(cost_data$calendar.year)+0.5)) +
  theme_bw()