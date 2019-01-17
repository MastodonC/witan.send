comparative_plot_settings = function(output, data1, data2){
  
  ## projections
  
  ### import the baseline dataset
  count_data1 = read.csv(paste0(data1, "Output_Setting.csv"))
  
  ### import the scenario dataset
  ### then bind to historic data for use in plot
  count_data2 = read.csv(paste0(data2, "Output_Setting.csv"))
  
  settings = unique(count_data1$setting)
  
  # PLOTS
  
  ## helpers for plots
  min_x = min(count_data1$calendar.year)
  max_x = max(count_data1$calendar.year)
  
  ## data labels for plots
  data1_label = str_to_title(sapply(strsplit(data1, "/"), tail, 1)) # this doesn't appear to work
  data2_label = str_to_title(sapply(strsplit(data2, "/"), tail, 1))

  ## ribbon plot version - not zero-indexed
  for(i in settings) {
    
    df1 <- count_data1  %>%
      filter(setting == i)
    df2 <- count_data2  %>%
      filter(setting == i)
  
    g <- ggplot(df1, aes(x=calendar.year)) +
      geom_line(aes(y=mean), colour=cols[1], alpha = alpha_line) +
      geom_ribbon(aes(ymin=q1, ymax=q3), fill=cols[1], alpha = alpha_ribbon) +
      geom_line(data=df2, aes(y=mean), colour=cols[2], alpha = alpha_line) +
      geom_ribbon(data=df2, aes(ymin=q1, ymax=q3), fill=cols[2], alpha = alpha_ribbon) +
      labs(y = paste(i, "SEND Population")) +
      scale_x_continuous(name="Year",
                         breaks = seq(min_x, max_x),
                         limits = c(min_x, max_x+1.5)) +
      ggtitle(paste(i, "Population Projection")) +
      theme(axis.text.x = element_text(size=8)) +
      geom_text(x=max_x+1,
                y=df1$mean[df1$calendar.year==max_x]+20,
                label= data1_label,
                colour = cols[1]) +
      geom_text(x=max_x+1,
                y=df2$mean[df2$calendar.year==max_x]-20,
                label= data2_label,
                colour = cols[2]) +
      theme(legend.position="none")
    
    ggsave(paste0(output, "comparisons/", i, "_Population_Comparative.pdf"),
           width=8,
           height=6,
           units="in")
    }
  
  # ribbon plot version - zero-indexed
#   g + scale_y_continuous(limits = c(0, max(count_data1$q3, na.rm=T)))
#   
#   ggsave(paste0(output, "comparisons/", i, "l_Population_Comparative_Zeroindexed.pdf"),
#          width=8,
#          height=6,
#          units="in")
 }