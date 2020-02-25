comparative_plot_settings_needs = function(output, data1, data2){
  
  ## projections
  
  ### import the baseline datasets
  count_setting_1 = read.csv(paste0(output, data1, "Output_Setting.csv"))
  count_need_1 = read.csv(paste0(output, data1, "Output_Need.csv"))
  
  ### import the scenario datasets
  ### then bind to historic data for use in plot
  count_setting_2 = read.csv(paste0(output, data2, "Output_Setting.csv"))
  count_need_2 = read.csv(paste0(output, data2, "Output_Need.csv"))
  
  settings = unique(count_setting_1$setting)
  needs = unique(count_need_1$need)
  
  # PLOTS
  
  ## helpers for plots
  min_x = min(count_setting_1$calendar.year, count_setting_2$calendar.year)
  max_x = max(count_setting_1$calendar.year, count_setting_2$calendar.year)
  
  ## data labels for plots
  #data1_label = str_to_title(sapply(strsplit(data1, "/"), tail, 1)) # this doesn't appear to work
  #data2_label = str_to_title(sapply(strsplit(data2, "/"), tail, 1))
  
  comparison_plot <- function(df1,df2, entity){
    max_y <- max(df1$max, df2$max)
    first_data = tail(strsplit(data1, "/")[[1]], 1)
    second_data = tail(strsplit(data2, "/")[[1]], 1)
    g <- ggplot(df1, aes(x=calendar.year)) +
      geom_line(aes(y=mean, colour=cols[1]), alpha = alpha_line) +
      geom_line(data=df2, aes(y=mean, colour=cols[2]), alpha = alpha_line) +
      geom_ribbon(data=df1, aes(ymin=q1, ymax=q3), fill=cols[2], alpha = alpha_ribbon) +
      geom_ribbon(data=df2, aes(ymin=q1, ymax=q3), fill=cols[1], alpha = alpha_ribbon) +
      scale_color_discrete(name = "", labels = c(second_data, first_data)) +
      labs(y = paste(entity, "SEND Population")) +
      scale_y_continuous(limits = c(0, max_y)) +
      scale_x_continuous(name="Year",
                         breaks = seq(min_x, max_x),
                         limits = c(min_x, max_x)) +
      ggtitle(paste(i, "Population Projection")) +
      theme(axis.text.x = element_text(size=8)) +
      theme_bw()

    ggsave(paste0(output, "comparisons/", entity, "_Population_Comparative.png"),
           width=8,
           height=6,
           dpi = 400,
           device = "png")
  }
  
  for(i in settings) {
    df1 <- count_setting_1  %>%
      filter(setting == i)
    df2 <- count_setting_2  %>%
      filter(setting == i)
    comparison_plot(df1, df2, i)
  }
  for(i in needs) {
    df1 <- count_need_1  %>%
      filter(need == i)
    df2 <- count_need_2  %>%
      filter(need == i)
    comparison_plot(df1, df2, i)
  }
}
