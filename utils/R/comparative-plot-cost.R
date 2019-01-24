comparative_plot_cost = function(output, data1, data2){
  
  # DATA
  
  ## import the baseline dataset and convert to millions
  cost_data1 = read.csv(paste0(data1, "Output_Cost.csv"))
  cost_data1[,-1] =  cost_data1[,-1] / 1000000

  ## import the scenario dataset and convert to millions
  cost_data2 = read.csv(paste0(data2, "Output_Cost.csv"))
  cost_data2[,-1] =  cost_data2[,-1] / 1000000

  
  # PLOTS

  ## helpers for plots
  min_x = min(cost_data1$calendar.year)
  max_x = max(cost_data1$calendar.year)
  
  ## data labels for plots
  data1_label = str_to_title(sapply(strsplit(data1, "-"), "[", 3))
  data2_label = str_to_title(sapply(strsplit(data2, "-"), "[", 3))
  
  ## ribbon plot - not zero-indexed
  g <- ggplot(cost_data1, aes(x=calendar.year)) +
    geom_line(aes(y=mean), colour=cols[1], alpha = alpha_line) +
    geom_ribbon(aes(ymin=q1, ymax=q3), fill=cols[1], alpha = alpha_ribbon)  +
    geom_ribbon(aes(ymin=q1, ymax=q3), fill=cols[1], alpha = alpha_ribbon) +
    geom_line(data=cost_data2, aes(y=mean), colour=cols[2], alpha = alpha_line) +
    geom_ribbon(data=cost_data2, aes(ymin=q1, ymax=q3), fill=cols[2], alpha = alpha_ribbon) +
    labs(y = "Total Projected SEND Cost / £ million") +
    scale_x_continuous(name="Year",
                       breaks = seq(min_x, max_x),
                       limits = c(min_x, max_x+1)) +
    ggtitle("SEND Cost Projection") +
    theme(axis.text.x = element_text(size=8)) +
    geom_text(x=max_x+0.8,
              y=cost_data1$mean[cost_data1$calendar.year==max_x],
              label= data1_label,
              colour = cols[1]) +
    geom_text(x=max_x+0.8,
              y=cost_data2$mean[cost_data2$calendar.year==max_x],
              label= data2_label,
              colour = cols[2]) +
    theme(legend.position="none")
  
  ggsave(paste0(output, "comparisons/", "Total_Cost_Comparative.png"),
         width=8,
         height=6,
         units="in",
         device="png")

  
  ## ribbon plot - zero-indexed
  g + scale_y_continuous(limits = c(0, max(cost_data1$q3, na.rm=T)))
  
  ggsave(paste0(output, "comparisons/", "Total_Cost_Comparative_Zeroindexed.png"),
         width=8,
         height=6,
         units="in",
         device="png")
}
