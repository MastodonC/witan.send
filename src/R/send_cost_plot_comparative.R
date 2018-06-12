send_cost_plot_comparative = function(baseline_filenameandpath, scenario_filenameandpath){
  
  # DATA
  
  ### import the baseline dataset and convert to millions
  cost_data_baseline = read.csv(baseline_filenameandpath)
  cost_data_baseline[,-1] =  cost_data_baseline[,-1] / 1000000
  cost_data_baseline$group = "baseline"
  
  ### import the scenario dataset and convert to millions
  cost_data_scenario = read.csv(scenario_filenameandpath)
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
  
  ggsave("target/Total_Cost_Comparative.pdf",
         width=8,
         height=6,
         units="in")
  
  
  
  ## ribbon plot - zero-indexed
  
  g + scale_y_continuous(limits = c(0, max(cost_data_baseline$q3, na.rm=T)))
  
  ggsave("target/zero_indexed/Total_Cost_Comparative_Zeroindexed.pdf",
         width=8,
         height=6,
         units="in")
}