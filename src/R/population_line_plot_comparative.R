population_count_plot_comparative = function(baseline_filenameandpath, scenario_filenameandpath){
  
  # DATA
  
  ## historic data
  
  ### import historic data - taken from send_charts.R
  remove_colons <-  function(x) {str_replace(x, ':', '')}
  df_historical <- read.csv("target/historic-data.csv") %>%
    mutate_all(funs(remove_colons)) %>%
    filter(need.1 != "NONSEND")
  
  n_hist_years <-  df_historical %>%
    summarise(n_distinct(calendar.year)) + 1.5
  
  ### extract year and population counts
  count_data_historic = df_historical %>%
    group_by(calendar.year) %>%
    count() %>%
    rename(mean=n, calendar.year.str=calendar.year) %>%
    mutate(calendar.year =  as.numeric(calendar.year.str))
  
  
  ## projections
  
  ### import the baseline dataset
  count_data_baseline = read.csv(baseline_filenameandpath) %>%
    mutate(group = "baseline")
  
  ### import the scenario dataset
  count_data_scenario = read.csv(scenario_filenameandpath) %>%
    mutate(group = "scenario") %>%
    bind_rows(count_data_historic)
  
  
  # PLOTS
  
  # ribbon plot version - not zero-indexed
  g <- ggplot(count_data_baseline, aes(x=calendar.year)) +
    geom_line(aes(y=mean), colour=cols[1], alpha = alpha_line) +
    geom_ribbon(aes(ymin=q1, ymax=q3), fill=cols[1], alpha = alpha_ribbon) +
    geom_line(data=count_data_scenario, aes(y=mean), colour=cols[2], alpha = alpha_line) +
    geom_ribbon(data=count_data_scenario, aes(ymin=q1, ymax=q3), fill=cols[2], alpha = alpha_ribbon) +
    labs(y="Total SEND Population") +
    scale_x_continuous(name="Year",
                       breaks = seq(min(count_data_historic$calendar.year), max(count_data_baseline$calendar.year)),
                       limits = c(min(count_data_historic$calendar.year), max(count_data_baseline$calendar.year)+1.5)) +
    ggtitle("SEND Population Projection") +
    theme(axis.text.x = element_text(size=8)) +
    geom_vline(xintercept = max(count_data_historic$calendar.year) + 0.5,
               color = "dodgerblue",
               linetype = "dashed")  +
    geom_text(x=max(count_data_historic$calendar.year) + 0.5,
              y=max(count_data_baseline$q3, na.rm=T),
              label = "<-- Historical      Projected -->",
              color = "dodgerblue") +
    geom_text(x=2027+1,
              y=count_data_baseline$mean[count_data_baseline$calendar.year==2027]+20,
              label= "Baseline",
              colour = cols[1]) +
    geom_text(x=2027+1,
              y=count_data_scenario$mean[count_data_scenario$calendar.year==2027]-20,
              label= "Scenario A",
              colour = cols[2]) +
    theme(legend.position="none")
  
  ggsave("target/Total_Population_Comparative.pdf",
         width=8,
         height=6,
         units="in")
  
  
  # ribbon plot version - zero-indexed
  g + scale_y_continuous(limits = c(0, max(count_data_baseline$q3, na.rm=T)))
  
  ggsave("target/zero_indexed/Total_Population_Comparative_Zeroindexed.pdf",
         width=8,
         height=6,
         units="in")
}