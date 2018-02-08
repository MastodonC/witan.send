library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)

### Variables for all charts ###

remove_colons <-  function(x) {str_replace(x, ':', '')}
df_historical <- read.csv("../../target/historic-data.csv") %>%
  mutate_all(funs(remove_colons)) %>%
  filter(need.1 != "NONSEND")

n_hist_years <-  df_historical %>%
  summarise(n_distinct(calendar.year)) + 1.5

###  Send by academic year ###

df_projected_ay <- read.csv("../../target/output-ay.csv") %>%
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

ggsave("../../target/NCY_Population_Trends.pdf")


### Projected count by setting ###

df_projected_set <- read.csv("../../target/output-setting.csv") %>%
  select(calendar.year, setting, mean) %>%
  rename(Setting = setting)

df_historical_set <- df_historical %>%
  group_by(setting.1, calendar.year) %>%
  count() %>%
  rename(Setting = setting.1, mean = n) %>%
  as.data.frame()


df_set <- rbind(df_historical_set[,c(2,1,3)], df_projected_set)

df_set_counts <- df_set %>%
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

  ggsave(paste("../../target/Settings_Trends_",i,".pdf",sep=""))
}


### Projected count by need type ###

df_projected_need <- read.csv("../../target/output-need.csv") %>%
  select(calendar.year, need, mean) %>%
  rename(Need = need)

df_historical_need <- df_historical %>%
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

ggsave("../../target/Need_Trends.pdf")


### Projected special setting count ###

df_projected_set <- read.csv("../../target/output-setting.csv") %>%
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

ggsave("../../target/Special_Setting_Counts.pdf")


### Projected aggregate setting count ###

df_valid_settings <- read.csv("../../target/valid-settings.csv", header = FALSE) %>%
  mutate_all(funs(remove_colons)) %>%
  rename(Setting = V1, Type = V2)

df_set_type <- merge(df_set,  df_valid_settings, by="Setting")

df_type <- df_set_type %>%
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

ggsave("../../target/Setting_Type_Counts.pdf")


### Delete automatically produced Rplots.pdf file ###

if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
