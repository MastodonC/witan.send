comparative_plot_population = function(output, data1, data2){
  
  # DATA
  
  ### import historic data - taken from send_charts.R
  remove_colons <-  function(x) {str_replace(x, ':', '')}
  df_historical <- read.csv(paste0(output, "data/baseline/transitions.csv")) %>%
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
  count_data1 = read.csv(paste0(data1, "Output_Count.csv"))
  
  ### import the scenario dataset
  ### then bind to historic data for use in plot
  count_data2 = read.csv(paste0(data2, "Output_Count.csv")) %>%
    bind_rows(count_data_historic)
  
  
  # PLOTS
  
  ## helpers for plots
  min_x = min(count_data_historic$calendar.year)
  max_x = max(count_data1$calendar.year)
  
  ## data labels for plots
  data1_label = tail(strsplit(data1, "/")[[1]], 1)
  data2_label = tail(strsplit(data2, "/")[[1]], 1)

  ## ribbon plot version - not zero-indexed
  g <- ggplot(count_data1, aes(x=calendar.year)) +
    geom_line(aes(y=mean, colour=cols[1]), alpha = alpha_line) +
    geom_ribbon(aes(ymin=q1, ymax=q3), fill=cols[2], alpha = alpha_ribbon) +
    geom_line(data=count_data2, aes(y=mean, colour=cols[2]), alpha = alpha_line) +
    geom_ribbon(data=count_data2, aes(ymin=q1, ymax=q3), fill=cols[1], alpha = alpha_ribbon) +
    labs(y="Total SEND Population") +
    scale_color_discrete(name = "", labels = c(data2_label, data1_label)) +
    scale_x_continuous(name="Year",
                       breaks = seq(min_x, max_x),
                       limits = c(min_x, max_x+1.5)) +
    ggtitle("SEND Population Projection") +
    theme(axis.text.x = element_text(size=8)) +
    geom_vline(xintercept = max(count_data_historic$calendar.year) + 1.5,
               color = "dodgerblue",
               linetype = "dashed")  +
    geom_text(x=max(count_data_historic$calendar.year) + 1.5,
              y=max(count_data1$q3, na.rm=T),
              label = "<-- Historical      Projected -->",
              color = "dodgerblue") +
    theme(legend.position="none") +
    theme_bw()
  
  ggsave(paste0(output, "comparisons/", "Total_Population_Comparative.png"),
         width=8,
         height=6,
         dpi = 400,
         device = "png")

  
  # ribbon plot version - zero-indexed
  g + scale_y_continuous(limits = c(0, max(count_data1$max, count_data2$max)))
  
  ggsave(paste0(output, "comparisons/", "Total_Population_Comparative_Zeroindexed.png"),
         width=8,
         height=6,
         dpi = 400,
         device="png")
}