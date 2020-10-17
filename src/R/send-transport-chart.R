library(ggplot2)

df <- read.csv("../../data/transport/Transport-Output.csv")

df$year <- as.factor(df$calendar.year)
mil <- 1000000
df$ymin <- df$min/mil
df$lower <- df$q1/mil
df$middle <- df$median/mil
df$upper <- df$q3/mil
df$ymax <- df$max/mil

ggplot(data=df,aes(year)) + 
geom_boxplot(stat='identity', fill='snow', colour='darkcyan', 
           aes(ymin=ymin, lower=lower, middle=middle, upper=upper, ymax=ymax)) +
ggtitle("SEND Transport Costs") +
scale_y_continuous(name="Total Projected SEND Transport Costs / £ million",
                   limits=c(0,max(df$ymax)))

ggsave("../../data/transport/Transport-Chart.pdf")

