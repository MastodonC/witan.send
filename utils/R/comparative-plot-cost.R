comparative_plot_cost = function(output, data1, data2){
  
  # DATA
  
  ## import the baseline dataset and convert to millions
  cost_data1 = read.csv(paste0(output, data1, "Output_Cost.csv"))
  cost_data1[,-1] =  cost_data1[,-1] / 1000000

  ## import the scenario dataset and convert to millions
  cost_data2 = read.csv(paste0(output, data2, "Output_Cost.csv"))
  cost_data2[,-1] =  cost_data2[,-1] / 1000000

  
  # PLOTS

  ## helpers for plots
  min_x = min(cost_data1$calendar.year)
  max_x = max(cost_data1$calendar.year)
  
  ## data labels for plots
  data1_label = tail(strsplit(data1, "/")[[1]], 1)
  data2_label = tail(strsplit(data2, "/")[[1]], 1)
  
  ## ribbon plot - not zero-indexed
  g <- ggplot(cost_data1, aes(x=calendar.year)) +
    geom_line(aes(y=mean, colour=cols[1]), alpha = alpha_line) +
    geom_ribbon(aes(ymin=q1, ymax=q3), fill=cols[2], alpha = alpha_ribbon) +
    geom_line(data=cost_data2, aes(y=mean, colour=cols[2]), alpha = alpha_line) +
    geom_ribbon(data=cost_data2, aes(ymin=q1, ymax=q3), fill=cols[1], alpha = alpha_ribbon) +
    labs(y = "Total Projected SEND Cost / £ million") +
    scale_color_discrete(name = "", labels = c(data2_label, data1_label)) +
    scale_x_continuous(name="Year",
                       breaks = seq(min_x, max_x),
                       limits = c(min_x, max_x)) +
    ggtitle("SEND Cost Projection") +
    theme(axis.text.x = element_text(size=8)) +
    theme(legend.position="none") +
    theme_bw()

  ggsave(paste0(output, "comparisons/", "Total_Cost_Comparative.png"),
         width=8,
         height=6,
         dpi = 400,
         device="png")

  
  ## ribbon plot - zero-indexed
  g + scale_y_continuous(limits = c(0, max(cost_data1$max, cost_data2$max)))
  
  ggsave(paste0(output, "comparisons/", "Total_Cost_Comparative_Zeroindexed.png"),
         width=8,
         height=6,
         dpi = 400,
         device="png")
}
