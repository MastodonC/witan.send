#!/usr/bin/env Rscript
# svglite requires cairo dev packages
packages_to_check = c("dplyr", "ggplot2", "reshape2", "stringr", "svglite", 
                      "logging", "optparse", "cowplot")

install_missing_packages <- function(package_name) {
  if(!require(package_name, character.only = TRUE)) {
    install.packages(package_name, repos='http://cran.us.r-project.org')}}

for(package in packages_to_check) {
  install_missing_packages(package) }

library("optparse")

option_list = list(
  make_option(c("-o", "--output-dir"), type="character", 
              help="output dir name [default= %default]", metavar="character"),
  make_option(c("-p", "--population-file"), type="character", 
              help="population file name", metavar="character"),
  make_option(c("-xs", "--exclude-settings"), type="character", default="", 
              help="settings to exclude", metavar="character"),
  make_option(c("-b", "--bound"), type="character", 
              help="confidence bound", metavar="character"));

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

output_dir = opt$`output-dir`
pop_path = opt$`population-file`
settings_to_remove <- strsplit(opt$`exclude-settings`, split=",")[[1]]

low_bound <- paste0("low", opt$bound)
high_bound <- paste0("high", opt$bound)

library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(ggforce)
library(logging)
library(cowplot)

basicConfig()
loginfo(output_dir)

evaluate_string <- function(string) {
  eval(parse(text=string))
}

remove_colons <- function(x) {str_replace(x, ':', '')}

if_acronym <- function(string) {
  ifelse(string == toupper(string),
         return(string),
         return(str_to_title(string)))
}

save_plot <- function(path) {
  ggsave(path, device = "png", width = 8, height = 6, dpi = 400)
}

### Variables for all charts ###

df_historical <- read.csv(paste0(output_dir, "/historic-data.csv")) %>%
  mutate_all(funs(remove_colons)) %>%
  mutate(setting.2 = gsub("_", " ", setting.2)) %>%
  mutate(setting.1 = gsub("_", " ", setting.1))

n_hist_years <-  df_historical %>%
  summarise(n_distinct(calendar.year)) + 1.5

###  Send by academic year ###

df_projected_ay <- read.csv(paste0(output_dir, "/Output_AY.csv")) %>%
  select(calendar.year, academic.year, mean)

df_historical_ay <- df_historical %>%
  filter(need.1 != "NONSEND" | setting.1 != "NONSEND") %>%
  group_by(academic.year.1, calendar.year) %>%
  rename(academic.year = academic.year.1) %>%
  count() %>%
  rename(mean = n) %>%
  as.data.frame()

prep_ay_groups <- function(data) {
  data %>%
    group_by(calendar.year) %>%
    summarise(NCY_0_under = sum(mean[academic.year <= 0]),
              NCY_1_6 =  sum(mean[academic.year >= 1 & academic.year <= 6]),
              NCY_7_11 = sum(mean[academic.year >= 7 & academic.year <= 11]),
              NCY_12_13 = sum(mean[academic.year >= 12 & academic.year <= 13]),
              NCY_14_up = sum(mean[academic.year >= 14])) %>%
    melt("calendar.year")
}

df_ay <- prep_ay_groups(rbind(df_historical_ay[,c(2,1,3)], df_projected_ay) %>%
  transform(academic.year = as.numeric(academic.year)))

y_max <- max(df_ay$value)

ggplot(df_ay, aes(x=calendar.year, y=value, group=variable)) +
  geom_line(aes(color=variable)) +
  geom_point(aes(color=variable)) +
  scale_x_discrete(name='Calendar Year') +
  scale_y_continuous(name='SEND Population') +
  scale_color_manual(name = "NCY",
                     values = c("cyan", "coral", "dodgerblue", "hotpink", "green4"),
                     labels = c("0 and under", "1 to 6", "7 to 11", "12 to 13", "14 and up")) +
  theme_bw() +
  ggtitle("SEND Trends, grouped by National Curriculum Years") +
  geom_vline(xintercept = n_hist_years[1,], color = "dodgerblue", linetype = "dashed") +
  annotate("text", label = "<-- Historical      Projected -->", x=n_hist_years[1,], y=y_max, color = "dodgerblue")

save_plot(paste0(output_dir, "/NCY_Population_Trends.png"))

### Project count by need and AY ###

df_projected_state <- read.csv(paste0(output_dir, "/Output_State.csv")) %>%
  select(calendar.year, academic.year, mean, need.setting) %>%
  mutate(need.setting = gsub(":", "", need.setting)) %>%
  mutate(need = gsub("-.*$", "", need.setting)) %>%
  mutate(setting = gsub("^.*-", "", need.setting))

df_projected_need_ay_counts <- df_projected_state %>%
  group_by(need, calendar.year, academic.year) %>%
  summarise(mean = sum(mean)) %>%
  as.data.frame()

dir.create(paste0(output_dir, "/needs"))

plot_need_ay <- function(data, need_str) {
  need_data <- prep_ay_groups(data %>% filter(need == need_str))
  ggplot(need_data, aes(x=calendar.year, y=value, group=variable)) +
    geom_line(aes(color=variable)) +
    geom_point(aes(color=variable)) +
    scale_x_continuous(name="Calendar Year", breaks=need_data$calendar.year) +
    scale_y_continuous(name='SEND Population') +
    scale_color_manual(name = "NCY",
                       values = c("cyan", "coral", "dodgerblue", "hotpink", "green4"),
                       labels = c("0 and under", "1 to 6", "7 to 11", "12 to 13", "14 and up")) +
    theme_bw() +
    ggtitle(paste(need_str, "Trends, grouped by National Curriculum Years")) #+
  #geom_vline(xintercept = n_hist_years[1,], color = "dodgerblue", linetype = "dashed") #+ ## Add back in for historical data
  #annotate("text", label = "<-- Historical      Projected -->", x=n_hist_years[1,], y=y_max, color = "dodgerblue")
  save_plot(paste0(output_dir, "/needs/", need_str, "_Trends.png"))
}

for (n in unique(df_projected_need_ay_counts$need)){
  plot_need_ay(df_projected_need_ay_counts, n)
}

### Project count by setting and AY ###

df_projected_setting_ay_counts <- df_projected_state %>%
  group_by(setting, calendar.year, academic.year) %>%
  summarise(mean = sum(mean)) %>%
  as.data.frame()

dir.create(paste0(output_dir, "/settings"))

plot_setting_ay <- function(data, setting_str) {
  setting_data <- prep_ay_groups(data %>% filter(setting == setting_str))
  ggplot(setting_data, aes(x=calendar.year, y=value, group=variable)) +
    geom_line(aes(color=variable)) +
    geom_point(aes(color=variable)) +
    scale_x_continuous(name="Calendar Year", breaks=setting_data$calendar.year) +
    scale_y_continuous(name='SEND Population') +
    scale_color_manual(name = "NCY",
                       values = c("cyan", "coral", "dodgerblue", "hotpink", "green4"),
                       labels = c("0 and under", "1 to 6", "7 to 11", "12 to 13", "14 and up")) +
    theme_bw() +
    ggtitle(paste(setting_str, "Trends, grouped by National Curriculum Years")) #+
  #geom_vline(xintercept = n_hist_years[1,], color = "dodgerblue", linetype = "dashed") #+ ## Add back in for historical data
  #annotate("text", label = "<-- Historical      Projected -->", x=n_hist_years[1,], y=y_max, color = "dodgerblue")
  save_plot(paste0(output_dir, "/settings/", setting_str, "_Trends.png"))
}

plot_setting_ay(df_projected_setting_ay_counts, "MU")

for (s in unique(df_projected_setting_ay_counts$setting)){
  plot_setting_ay(df_projected_setting_ay_counts, s)
}

### Projected count by setting ###

df_projected_set <- read.csv(paste0(output_dir, "/Output_Setting.csv")) %>%
  select(calendar.year, setting, mean) %>%
  mutate(setting = gsub("_", " ", setting)) %>%
  rename(Setting = setting)

df_historical_set <- df_historical %>%
  group_by(setting.1, calendar.year) %>%
  count() %>%
  rename(Setting = setting.1, mean = n) %>%
  as.data.frame()

nonsend_list = c("NONSEND")

list_to_filter_by <- c(nonsend_list, settings_to_remove)

df_set <- rbind(df_historical_set[,c(2,1,3)], df_projected_set) %>%
  filter(!Setting %in% list_to_filter_by) %>%
  mutate(Setting = gsub("_", " ", Setting))

df_set_years = unique(df_set$calendar.year)

df_set_setting = unique(df_set$Setting)

check_years_present <- function(year, setting, df) {
  if(nrow(merge(data.frame(calendar.year=year, Setting=setting), df)) == 0) {
    df_new = df %>%
      rbind(c(year, setting, 0))
  } else {
    df_new = df
  }
  return(df_new)
}

for (y in df_set_years){
  for (s in df_set_setting){
    df_set = check_years_present(y, s, df_set)
  }
}

df_set = df_set %>%
  mutate(mean = as.numeric(mean))

df_set_counts <- df_set %>%
  filter(mean != 0) %>%
  group_by(Setting) %>%
  summarise(mean(mean)) %>%
  rename(mean = "mean(mean)") %>%
  filter(Setting != "NONSEND") %>%
  arrange(desc(mean))

max_settings_per_chart <- 5
n_settings <- nrow(df_set_counts)
n_charts <- ceiling(n_settings/max_settings_per_chart)
settings_per_chart <- ceiling(n_settings/n_charts)
idx_start <- 1
idx_end <- settings_per_chart

for(i in 1:n_charts) {
  df_i <- subset(df_set, Setting %in% df_set_counts[idx_start:idx_end,]$Setting)
  idx_start <- idx_end + 1
  idx_end <-idx_end + settings_per_chart
  y_max <- max(df_i$mean)

  ggplot(df_i, aes(x=calendar.year, y=mean, group=Setting)) +
    geom_line(aes(color=Setting)) +
    geom_point(aes(color=Setting)) +
    scale_x_discrete(name='Calendar Year') +
    scale_y_continuous(name='SEND Population', limits=c(0,y_max)) +
    theme_bw() +
    ggtitle(paste("SEND Trends, by Setting (", i, " of ", n_charts, ")", sep="")) +
    geom_vline(xintercept = n_hist_years[1,], color = "dodgerblue", linetype = "dashed") +
    annotate("text", label = "<-- Historical      Projected -->", x=n_hist_years[1,], y=y_max, color = "dodgerblue")

  save_plot(paste0(output_dir, "/Settings_Trends_", i,".png"))
}


### Projected count by need type ###

df_projected_need <- read.csv(paste0(output_dir, "/Output_Need.csv")) %>%
  mutate(need = gsub("_", " ", need)) %>%
  mutate(need = if_acronym(need)) %>%
  select(calendar.year, need, mean) %>%
  rename(Need = need)

df_historical_need <- df_historical %>%
  filter(need.1 != "NONSEND" | setting.1 != "NONSEND") %>%
  mutate(need.1 = gsub("_", " ", need.1)) %>%
  mutate(need.1 = if_acronym(need.1)) %>%
  group_by(need.1, calendar.year) %>%
  count() %>%
  rename(Need = need.1, mean = n) %>%
  as.data.frame()

df_need <- rbind(df_historical_need[,c(2,1,3)], df_projected_need)
y_max <- max(df_need$mean)

ggplot(df_need, aes(x=calendar.year, y=mean, group=Need)) +
  geom_line(aes(color=Need)) +
  geom_point(aes(color=Need)) +
  scale_x_discrete(name='Calendar Year') +
  scale_y_continuous(name='SEND Population', limits=c(0,max(df_need$mean))) +
  theme_bw() +
  ggtitle("SEND Trends, by Need") +
  geom_vline(xintercept = n_hist_years[1,], color = "dodgerblue", linetype = "dashed") +
  annotate("text", label = "<-- Historical      Projected -->", x=n_hist_years[1,], y=y_max, color = "dodgerblue")

save_plot(paste0(output_dir, "/Need_Trends.png"))


### Projected special setting count ###

df_valid_settings <- read.csv(paste0(output_dir, "/valid-settings.csv"), header = FALSE) %>%
  mutate_all(funs(remove_colons)) %>%
  rename(Setting = V1, Type = V2)

df_projected_set <- read.csv(paste0(output_dir, "/Output_Setting.csv")) %>%
  select(calendar.year, setting, mean) %>%
  rename(Setting = setting)

df_historical_set <- df_historical %>%
  group_by(setting.1, calendar.year) %>%
  count() %>%
  rename(Setting = setting.1, mean = n) %>%
  as.data.frame()

df_set <- rbind(df_historical_set[,c(2,1,3)], df_projected_set)

df_set_type <- merge(df_set,  df_valid_settings, by="Setting")

df_set_ss <- df_set_type %>%
  filter(str_detect(Type, 'Special'))

y_max <- df_set_ss %>%
  summarise(max = sum(mean[calendar.year == max(df_projected_set$calendar.year)]))

ggplot(df_set_ss, aes(x = calendar.year, y = mean)) +
  geom_bar(aes(fill = Setting), stat="identity") +
  scale_x_discrete(name='Calendar Year') +
  scale_y_continuous(name='Total') +
  theme_bw() +
  ggtitle("Projected SEND Counts, by Special Setting") +
  geom_vline(xintercept = n_hist_years[1,], color = "dodgerblue", linetype = "dashed") +
  annotate("text", label = "<-- Historical      Projected -->", x=n_hist_years[1,], y=y_max$max, color = "dodgerblue")

save_plot(paste0(output_dir, "/Special_Setting_Counts.png"))


### Projected aggregate setting count ###

df_type <- df_set_type %>%
  filter(Setting != "NONSEND") %>%
  group_by(calendar.year, Type) %>%
  summarise(mean = sum(mean))

y_max <- max(df_type$mean)

ggplot(df_type, aes(x=calendar.year, y=mean, group=Type)) +
  geom_line(aes(color=Type)) +
  geom_point(aes(color=Type)) +
  scale_x_discrete(name='Calendar Year') +
  scale_y_continuous(name='SEND Population', limits=c(0,y_max)) +
  theme_bw() +
  ggtitle("SEND Trends, by Setting Type") +
  geom_vline(xintercept = n_hist_years[1,], color = "dodgerblue", linetype = "dashed") +
  annotate("text", label = "<-- Historical      Projected -->", x=n_hist_years[1,], y=y_max, color = "dodgerblue")

save_plot(paste0(output_dir, "/Setting_Type_Counts.png"))


### SEND population Projection ###

count_data_historic = df_historical %>%
  filter(need.1 != "NONSEND" | setting.1 != "NONSEND") %>%
  group_by(calendar.year) %>%
  count() %>%
  rename(mean=n, calendar.year.str=calendar.year) %>%
  mutate(calendar.year =  as.numeric(calendar.year.str))

count_data_projected <- read.csv(paste0(output_dir, "/Output_Count.csv"))

count_data <- bind_rows(count_data_projected, count_data_historic)

geom_bound <- function(bound) {
  paste0("geom_line(aes(y=", bound, ", colour='conf', linetype='conf')) +")
}

total_pop_text <- paste0("ggplot(count_data, aes(x=calendar.year, y=mean)) +
                          geom_line(aes(colour='mean', linetype='mean')) +
                          geom_point(colour='darkcyan') +",
                         geom_bound(as.name(low_bound)),
                         geom_bound(as.name(high_bound)),
                          "scale_y_continuous(name = \"Total SEND Population\", limits = c(0, max(count_data$max))) +
                          scale_x_continuous(name=\"Year\", breaks = seq(min(count_data$calendar.year), max(count_data$calendar.year)), limits = c(min(count_data$calendar.year), max(count_data$calendar.year))) +
                          scale_linetype_manual(name=\"\", values=c(mean='solid', conf='dashed'), labels=c(mean='Mean', conf=\"Confidence Bounds\")) +
                          scale_color_manual(name=\"\", values=c(mean='darkcyan', conf=\"grey38\"), labels=c(mean='Mean', conf=\"Confidence Bounds\")) +
                          theme_bw() +
                          theme(axis.text.x = element_text(size=8)) +
                          ggtitle(\"SEND Population Projection\") +
                          geom_vline(xintercept = max(count_data_historic$calendar.year) + 1.0, color = \"dodgerblue\", linetype = \"dashed\")  +
                          annotate(\"text\",label = \"<-- Historical      Projected -->\", x=max(count_data_historic$calendar.year) + 1.0, y=max(count_data_projected$max), color = \"dodgerblue\")")

evaluate_string(total_pop_text)

save_plot(paste0(output_dir, "/Total_Population.png"))


### SEND cost projection ####

cost_projected <- read.csv(paste0(output_dir, "/Output_Cost.csv"))
cost_projected[,-1] <-  cost_projected[,-1] / 1000000
min_x = min(cost_projected$calendar.year)
max_x = max(cost_projected$calendar.year)

boxplot <- paste0("ggplot(cost_projected, aes(x=calendar.year)) +",
         "geom_boxplot(aes(lower = q1, upper = q3, middle = median, ymin = ", as.name(low_bound),
         ", ymax = ", as.name(high_bound), "), fill='snow', colour='darkcyan', stat = \"identity\") +",
         "ggtitle(\"SEND Cost Projection\") +",
         "scale_x_continuous(name='Calendar Year', breaks=seq(min_x, max_x, by=1), limits=c(min_x-0.5, max_x+0.5)) +",
         "scale_y_continuous(name = \"Total projected SEND cost / £ million\", limits = c(0, max(cost_projected$",
         as.name(high_bound),
         "))) +",
         "theme_bw()")

evaluate_string(boxplot)

save_plot(paste0(output_dir, "/Total_Cost.png"))

### Sankey plot ###

colour_list <- c("#3cb44b", "#ffe119", "#4363d8", "#f58231", "#aaffc3", "#42d4f4", 
                 "#f032e6", "#bfef45", "#fabebe", "#469990", "#e6beff", "#9A6324",
                 "#fffac8", "#800000", "#808000", "#ffd8b1", "#000075", "#a9a9a9",
                 "#e6194b", "#0099ff", "#ff99cc", "#ffcc00", "#cc9999", "#99ffcc")

sankey <- function(data, title, colour_map) {
  ggplot(data, aes(x, id = id, split = factor(y), value = value)) +
    ggtitle(title) +
    geom_parallel_sets_axes(axis.width = 0.2, fill = "#F6F6F6", color = "#DDDDDD") +
    geom_parallel_sets(aes(fill = Setting), alpha = 0.7, axis.width = 0.1) +
    scale_fill_manual(values = colour_map) +
    geom_parallel_sets_labels(color = "#444444", angle = 0, size = 2.5) +
    theme(axis.title.x = element_blank(), axis.text.y = element_blank())
}

### Historic Primary to Secondary Transitions ###

df_prim_sec_trans <- df_historical %>%
  filter(setting.1 != "NONSEND") %>%
  filter(setting.2 != "NONSEND")

years = as.numeric(unique(df_historical$calendar.year))

settings = unique(df_historical$setting.1)

get_transition <- function(data, setting1, setting2) {
  table((data$setting.1 == setting1) & (data$setting.2 == setting2))[[2]]
}

sankey_prim_sec_trans <- function(data, calendar_year, colour_map) {
  result<-data.frame()
  v = -1
  filtered_data<-filter(data, calendar.year == calendar_year)

  for (s1 in settings) {
    for (s2 in settings) {
      transitions <- try(get_transition(filtered_data, s1, s2), silent = T)
      if(is.numeric(transitions)) {
        v = v + 1
        result <- rbind(result, data.frame(y = s1, value = transitions, Setting = s2, id = v, x = "from"))
        result <- rbind(result, data.frame(y = s2, value = transitions, Setting = s2, id = v, x = "to"))
      }
    }
  }

  result <- merge(result,  df_valid_settings, by="Setting") %>%
    subset(select = -c(Setting)) %>%
    rename(temp = Type) %>%
    rename(Setting = y) %>%
    merge(df_valid_settings, by="Setting") %>%
    subset(select = -c(Setting)) %>%
    rename(Setting = temp) %>%
    rename(y = Type) %>%
    arrange(Setting)

  sankey(result, paste0("Aggregate Settings Transitions ", calendar_year, "/", (calendar_year + 1)), colour_map)
}

transition_colours <- colour_list
names(transition_colours) <- unique(df_valid_settings$Type)

for (f in years) {
  sankey_prim_sec_trans(df_prim_sec_trans, f, transition_colours)
  save_plot(paste0(output_dir, "/Historic_Transitions_", f, ".png"))
}

sankey_ay_trans <- function(ay){
  df_prim_sec_trans <- df_historical %>%
    filter(setting.1 != "NONSEND" & academic.year.1 == ay ) %>%
    filter(setting.2 != "NONSEND")
  return(sankey_prim_sec_trans(df_prim_sec_trans, 2016, transition_colours))
}

p <- lapply(8:13, sankey_ay_trans)
pg <- plot_grid(plotlist=p, ncol=6)
cowplot::save_plot(paste0(output_dir, "/Historic_Transitions_Boom.png"), pg, ncol=6, base_height = 8, 
                   base_aspect_ratio = NULL, 
                   base_width = 5,
                   limitsize = FALSE)

### SEND Joiner Transitions ###

df_joiners <- df_historical %>%
  filter(need.1 == "NONSEND" | setting.1 == "NONSEND") %>%
  mutate(setting.2 = gsub("_", " ", setting.2))

df_joiners_trans<-data.frame()
v = -1

for (s2 in settings) {
  transitions <- try(get_transition(df_joiners, "NONSEND", s2), silent = T)
  if(is.numeric(transitions)) {
    v = v + 1
    df_joiners_trans <- rbind(df_joiners_trans, data.frame(y = "NONSEND", value = transitions, Setting = s2, id = v, x = "from"))
    df_joiners_trans <- rbind(df_joiners_trans, data.frame(y = s2, value = transitions, Setting = s2, id = v, x = "to"))
  }
}

df_joiners_trans$y <- as.character(df_joiners_trans$y)
df_joiners_trans$Setting <- as.character(df_joiners_trans$Setting)

joiner_colours <- colour_list
names(joiner_colours) <- unique(df_joiners_trans$Setting)

sankey(df_joiners_trans, "Joiner Transitions", joiner_colours)
save_plot(paste0(output_dir, "/Joiner_Transitions.png"))


### Ribbon plot ###

colour_palette = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")

create_ribbon_str <- function(year, n) {
  upper = paste0("upper", n)
  lower = paste0("lower", n)
  col = colour_palette[(n + 1)]
  paste0("geom_ribbon(aes(ymax = ", upper, ", ymin = ", lower, ", fill = \"", col, "\"), alpha = 0.2) +")
}

cum <- 0
cum_ribbon_text <- ""
year_count <- length(years)
for(y in years){
  cum_ribbon_text <- paste(cum_ribbon_text, create_ribbon_str(y, cum))
  cum <- cum + 1
}

ribbon_plot <- function(data, title) {
  concat_text <-  paste0("ggplot(data, aes(year, upper0)) +",
                        cum_ribbon_text,
                        "ggtitle(paste(title, \"probability by academic year\")) +",
                        "xlab(\"NCY\") + ",
                        "ylab(\"Probability of transition\") +",
                        "scale_fill_manual(name = \"Years\", labels = years, values = colour_palette) +",
                        "scale_x_continuous(breaks = c(-2, 0, 2, 4, 6, 8, 10, 12), limit = c(-3, 12))"
                        )
  eval(parse(text=concat_text))
  save_plot(paste0(output_dir, "/", title, "_Probability.png"))
}

### Ribbon plot data ###
df_joiner_ribbon_data <- read.csv(paste0(output_dir, "/joiner-rates.csv"), na.strings=":NA")
df_leaver_ribbon_data <- read.csv(paste0(output_dir, "/leaver-rates.csv"), na.strings=":NA")
df_mover_ribbon_data <- read.csv(paste0(output_dir, "/mover-rates.csv"), na.strings=":NA")

#ribbon_plot(df_joiner_ribbon_data, "Joiner")
#ribbon_plot(df_leaver_ribbon_data, "Leaver")
#ribbon_plot(df_mover_ribbon_data, "Mover")


### ONS Population Chart ###

if(file_test("-f", pop_path)){
  ons <- read.csv(pop_path)
  ons <- ons[!ons$academic.year>20,]
  ons$NCY <- ifelse(ons$academic.year <= 0, "NCY_00_under", 
                     ifelse(ons$academic.year <= 6, "NCY_01_06",
                            ifelse(ons$academic.year <= 11, "NCY_07_11",
                                   ifelse(ons$academic.year <= 13, "NCY_12_13", "NCY_14_up"))))
  ons <- aggregate(ons$population, by=list(Year=ons$calendar.year, NCY=ons$NCY), FUN=sum)
  
  ggplot(ons, aes(x=Year, y=x, group=NCY)) +
    geom_line(aes(color=NCY)) +
    geom_point(aes(color=NCY)) +
    scale_x_continuous(breaks = round(seq(min(ons$Year), max(ons$Year), by = 1))) +
    scale_y_continuous(name='Population') +
    expand_limits(y = 0) +
    scale_color_manual(name = "NCY",
                       values = c("cyan", "coral", "dodgerblue", "hotpink", "green4"),
                       labels = c("0 and under", "1 to 6", "7 to 11", "12 to 13", "14 and up")) +
    theme_bw() +
    ggtitle("General Population, grouped by National Curriculum Years")
  
  save_plot(paste0(output_dir, "/General_Population.png"))
}

### Delete automatically produced Rplots.pdf file ###

if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
