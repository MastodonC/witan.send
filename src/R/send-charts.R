args = commandArgs(trailingOnly=TRUE)
output_dir = args[1]

install_missing_packages <- function(package_name) {
  if(!require(package_name, character.only = TRUE)) {
    install.packages(package_name, repos='http://cran.us.r-project.org') } }

# svglite requires cairo dev packages
packages_to_check = c("dplyr", "ggplot2", "reshape2", "stringr", "svglite", "ggforce")

for(package in packages_to_check) {
  install_missing_packages(package) }

library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(ggforce)

### Variables for all charts ###

remove_colons <-  function(x) {str_replace(x, ':', '')}
df_historical <- read.csv(paste0(output_dir, "/historic-data.csv")) %>%
  mutate_all(funs(remove_colons))

n_hist_years <-  df_historical %>%
  summarise(n_distinct(calendar.year)) + 1.5

###  Send by academic year ###

df_projected_ay <- read.csv(paste0(output_dir, "/Output_AY.csv")) %>%
  select(calendar.year, academic.year, mean)

df_historical_ay <- df_historical %>%
  group_by(academic.year.1, calendar.year) %>%
  rename(academic.year = academic.year.1) %>%
  count() %>%
  rename(mean = n) %>%
  as.data.frame()

df_ay <- rbind(df_historical_ay[,c(2,1,3)], df_projected_ay) %>%
    transform(academic.year = as.numeric(academic.year))

plot_ay <- df_ay %>%
  group_by(calendar.year) %>%
  summarise(NCY_0_under = sum(mean[academic.year <= 0]),
            NCY_1_6 =  sum(mean[academic.year >= 1 & academic.year <= 6]),
            NCY_7_11 = sum(mean[academic.year >= 7 & academic.year <= 11]),
            NCY_12_13 = sum(mean[academic.year >= 12 & academic.year <= 13]),
            NCY_14_up = sum(mean[academic.year >= 14])) %>%
  melt("calendar.year")

y_max <- max(plot_ay$value)

ggplot(plot_ay, aes(x=calendar.year, y=value, group=variable)) +
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

ggsave(paste0(output_dir, "/NCY_Population_Trends.pdf"))


### Projected count by setting ###

df_projected_set <- read.csv(paste0(output_dir, "/Output_Setting.csv")) %>%
  select(calendar.year, setting, mean) %>%
  rename(Setting = setting)

df_historical_set <- df_historical %>%
  group_by(setting.1, calendar.year) %>%
  count() %>%
  rename(Setting = setting.1, mean = n) %>%
  as.data.frame()


df_set <- rbind(df_historical_set[,c(2,1,3)], df_projected_set) %>%
  filter(Setting != "NONSEND") %>%
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
  rename(mean = "mean(mean)")


split_1 <- quantile(df_set_counts$mean, 0.25)
split_2 <- quantile(df_set_counts$mean, 0.5)
split_3 <- quantile(df_set_counts$mean, 0.75)

chart_1_settings <- df_set_counts[df_set_counts$mean <= split_1,]$Setting
chart_2_settings <- df_set_counts[df_set_counts$mean > split_1 & df_set_counts$mean <= split_2,]$Setting
chart_3_settings <- df_set_counts[df_set_counts$mean > split_2 & df_set_counts$mean <= split_3,]$Setting
chart_4_settings <- df_set_counts[df_set_counts$mean > split_3,]$Setting


for(i in 1:4) {
  df_i <- subset(df_set, Setting %in% eval(parse(text = paste("chart_",i,"_settings", sep=""))))
  y_max <- max(df_i$mean)

  ggplot(df_i, aes(x=calendar.year, y=mean, group=Setting)) +
    geom_line(aes(color=Setting)) +
    geom_point(aes(color=Setting)) +
    scale_x_discrete(name='Calendar Year') +
    scale_y_continuous(name='SEND Population', limits=c(0,y_max)) +
    theme_bw() +
    ggtitle(paste("SEND Trends, by Setting (",i,"/4)",sep="")) +
    geom_vline(xintercept = n_hist_years[1,], color = "dodgerblue", linetype = "dashed") +
    annotate("text", label = "<-- Historical      Projected -->", x=n_hist_years[1,], y=y_max, color = "dodgerblue")

  ggsave(paste(output_dir, "/Settings_Trends_",i,".pdf",sep=""))
}


### Projected count by need type ###

df_projected_need <- read.csv(paste0(output_dir, "/Output_Need.csv")) %>%
  select(calendar.year, need, mean) %>%
  rename(Need = need)

df_historical_need <- df_historical %>%
  filter(need.1 != "NONSEND") %>%
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

ggsave(paste0(output_dir, "/Need_Trends.pdf"))


### Projected special setting count ###

df_projected_set <- read.csv(paste0(output_dir, "/Output_Setting.csv")) %>%
  select(calendar.year, setting, mean) %>%
  rename(Setting = setting)

df_historical_set <- df_historical %>%
  group_by(setting.1, calendar.year) %>%
  count() %>%
  rename(Setting = setting.1, mean = n) %>%
  as.data.frame()

df_set <- rbind(df_historical_set[,c(2,1,3)], df_projected_set)

df_set_ss <- df_set %>%
  filter(str_detect(Setting, 'SS'))

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

ggsave(paste0(output_dir, "/Special_Setting_Counts.pdf"))


### Projected aggregate setting count ###

df_valid_settings <- read.csv(paste0(output_dir, "/valid-settings.csv"), header = FALSE) %>%
  mutate_all(funs(remove_colons)) %>%
  rename(Setting = V1, Type = V2)

df_set_type <- merge(df_set,  df_valid_settings, by="Setting")

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

ggsave(paste0(output_dir, "/Setting_Type_Counts.pdf"))


### SEND population Projection ###

count_data_historic = df_historical %>%
  group_by(calendar.year) %>%
  count() %>%
  rename(mean=n, calendar.year.str=calendar.year) %>%
  mutate(calendar.year =  as.numeric(calendar.year.str))

count_data_projected <- read.csv(paste0(output_dir, "/Output_Count.csv"))

count_data <- bind_rows(count_data_projected, count_data_historic)

ggplot(count_data, aes(x=calendar.year, y=mean)) +
  geom_line(aes(colour='mean', linetype='mean')) +
  geom_point(colour='darkcyan') +
  geom_line(aes(y=low.ci, colour='conf', linetype='conf')) +
  geom_line(aes(y=high.ci, colour='conf', linetype='conf')) +
  scale_y_continuous(name = "Total SEND Population",
                     limits = c(0, max(count_data$high.ci))) +
  scale_x_continuous(name="Year",
                     breaks = seq(min(count_data$calendar.year), max(count_data$calendar.year)),
                     limits = c(min(count_data$calendar.year), max(count_data$calendar.year))) +
  scale_linetype_manual(name="", values=c(mean='solid', conf='dashed'), labels=c(mean='Mean', conf="95% Confidence")) +
  scale_color_manual(name="", values=c(mean='darkcyan', conf="grey38"), labels=c(mean='Mean', conf="95% Confidence")) +
  theme_bw() +
  theme(axis.text.x = element_text(size=8)) +
  ggtitle("SEND Population Projection") +
  geom_vline(xintercept = max(count_data_historic$calendar.year) + 1.0,
             color = "dodgerblue",
             linetype = "dashed")  +
  annotate("text",label = "<-- Historical      Projected -->",
           x=max(count_data_historic$calendar.year) + 1.0,
           y=max(count_data_projected$max), color = "dodgerblue")

ggsave(paste0(output_dir, "/Total_Population.pdf"))


### SEND cost projection ####

cost_projected <- read.csv(paste0(output_dir, "/Output_Cost.csv"))
cost_projected[,-1] <-  cost_projected[,-1] / 1000000
min_x = min(cost_projected$calendar.year)
max_x = max(cost_projected$calendar.year)

ggplot(cost_projected, aes(x=calendar.year)) +
  geom_boxplot(aes(lower = q1, upper = q3, middle = median,
                   ymin = low.ci, ymax = high.ci), fill='snow', colour='darkcyan', stat = "identity") +
  ggtitle("SEND Cost Projection") +
  scale_x_continuous(name='Calendar Year', breaks=seq(min_x, max_x, by=1), limits=c(min_x-0.5, max_x+0.5)) +
  scale_y_continuous(name = "Total projected SEND cost / £ million",
                     limits = c(0, max(cost_projected$high.ci))) +
  theme_bw()

ggsave(paste0(output_dir, "/Total_Cost.pdf"))

### Sankey plot ###

sankey <- function(data, title) {
  ggplot(data, aes(x, id = id, split = factor(y), value = value)) +
    ggtitle(title) +
    geom_parallel_sets_axes(axis.width = 0.2, fill = "#F6F6F6", color = "#DDDDDD") +
    geom_parallel_sets(aes(fill = Setting), alpha = 0.5, axis.width = 0.1) +
    geom_parallel_sets_labels(color = "#444444", angle = 0, size = 2.5) +
    theme(axis.title.x = element_blank(), axis.text.y = element_blank())
}

### SEND Primary to Secondary Transitions ###

df_prim_sec_trans <- df_historical %>%
  filter(setting.1 != "NONSEND" & academic.year.1 == 6) %>%
  filter(setting.2 != "NONSEND")

years = as.numeric(unique(df_historical$calendar.year))

settings = unique(df_historical$setting.1)

get_transition <- function(data, setting1, setting2) {
  table((data$setting.1 == setting1) & (data$setting.2 == setting2))[[2]]
}

sankey_prim_sec_trans <- function(data, calendar_year) {
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
    rename(y = Type)
  
  sankey(result, paste0("Aggregate Settings Transitions ", calendar_year, "/", (calendar_year + 1)))
}

for (f in years) {
  sankey_prim_sec_trans(df_prim_sec_trans, f)
  ggsave(paste0(output_dir, "/Historic_Transitions_",f,".pdf"))
}

### SEND Joiner Transitions ###

df_joiners <- df_historical %>%
  filter(setting.1 == "NONSEND")

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

sankey(df_joiners_trans, "Joiner Transitions")
ggsave(paste0(output_dir, "/Joiner_Transitions.pdf"))

### Delete automatically produced Rplots.pdf file ###

if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
