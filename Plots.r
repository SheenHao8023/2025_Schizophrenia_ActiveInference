packages <- c("ggprism", "ggsci", "ggpubr", "ggplot2", "dplyr", "tidyr", "readr", "readxl", "openxlsx", "stringr")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

########### Computing and normalizing index ###########
raw_dir <- "C:/Users/XinHao/Desktop/2025_SCZ_AI/empirical_data/behav_rawfiles"
main_file <- "C:/Users/XinHao/Desktop/2025_SCZ_AI/empirical_data/data_behav&sympt.xlsx"
basepath = 'C:/Users/XinHao/Desktop/2025_SCZ_AI/empirical_data/'
file_list <- list.files(raw_dir, pattern = "\\.xlsx$", full.names = TRUE)
mean_values <- data.frame(SubID = character(), IC = numeric(), WS = numeric(), stringsAsFactors = FALSE)

for (file in file_list) {
  sub_id <- str_extract(basename(file), "\\d{5}")
  dat <- read_excel(file)
  ic_mean <- mean(dat$IC, na.rm = TRUE)
  ws_mean <- mean(dat$WS, na.rm = TRUE)
  mean_values <- rbind(mean_values, data.frame(SubID = sub_id, IC = ic_mean, WS = ws_mean))
}

main_data <- read.xlsx(main_file)
main_data <- main_data %>%
  left_join(mean_values, by = "SubID") %>%
  mutate(
    IC = 1 / (1 + exp(-scale(IC.y))),
    WS = 1 / (1 + exp(-scale(WS.y)))
  ) %>%
  select(-IC.x, -IC.y, -WS.x, -WS.y)
write.xlsx(main_data, main_file, overwrite = TRUE)

########### EmpiricalPlot_Symptoms, FigX #############
main_data$`PANSS_P1.+.P3.(psychotic)` <- as.numeric(main_data$`PANSS_P1.+.P3.(psychotic)`)
main_data$`PANSS_N2.+.N4.(withdrawal)` <- as.numeric(main_data$`PANSS_N2.+.N4.(withdrawal)`)
main_data$`PANSS_N1.(blunted.affect)` <- as.numeric(main_data$`PANSS_N1.(blunted.affect)`)
main_data$PANSS_Total <- as.numeric(main_data$PANSS_Total)
data <- data.frame( 
  "A" = c(main_data$`PANSS_P1.+.P3.(psychotic)`[1:12] / main_data$PANSS_Total[1:12], 0,  main_data$`PANSS_P1.+.P3.(psychotic)`[9:12] / main_data$PANSS_Total[9:12]),
  "B" = c(main_data$`PANSS_N2.+.N4.(withdrawal)`[1:12] / main_data$PANSS_Total[1:12], 0,  main_data$`PANSS_N2.+.N4.(withdrawal)`[9:12] / main_data$PANSS_Total[9:12]),
  "C" = c(main_data$`PANSS_N1.(blunted.affect)`[1:12]*2 / main_data$PANSS_Total[1:12], 0,  main_data$`PANSS_N1.(blunted.affect)`[9:12]*2 / main_data$PANSS_Total[9:12]))
data$Group <- c(rep("PS", 8), rep("NS", 4), rep('NA', 1), rep("BA", 2), rep("WD", 2))
data_long <- data %>%
  pivot_longer(cols = -Group, names_to = "Metric", values_to = "Value")
summary_data <- data_long %>%
  group_by(Metric, Group) %>%
  summarise(mean = mean(Value), sd = sd(Value), .groups = 'drop')
levels(summary_data$Metric) <- c("A", "B", "C")
summary_data$Group <- factor(summary_data$Group, levels = c('PS','NS','NA','BA','WD'))
data_long$Group <- factor(data_long$Group, levels = c('PS','NS','NA','BA','WD'))
summary_data$Metric <- factor(summary_data$Metric, levels = c("A", "B", "C"))
ggplot(summary_data, aes(x = Group, y = mean, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7, colour = NA) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.4, position = position_dodge(width = 0.9), linewidth = 0.7) + 
  geom_jitter(data = subset(data_long, Group != 'NA'), aes(x = Group, y = Value), 
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7), alpha = 1, color = 'black', size = 1) + 
  scale_fill_manual(values = c('#fc8d62','#8da0cb','#6495ed'), name = "Items", labels = c("P1+P3", "N2+N4", "2N1")) + 
  scale_x_discrete(breaks = c('PS','NS','BA','WD'), labels = c('PS', 'NS', 'BA', 'WD')) + 
  coord_cartesian(ylim = c(0, 0.15)) +
  labs(x = NULL, y = "Relative Scores") +
  theme_prism(axis_text_angle = 0)+ 
  theme(legend.direction = "vertical", legend.position = 'right',
        #panel.grid = element_blank(),
        axis.line = element_line(color = 'black', size = 0.75),
        axis.ticks = element_line(color = 'black', size = 0.75),
        axis.text = element_text(size = 12, color = "black", face = 'plain'),  
        axis.title = element_text(size = 14, face = 'plain')) +
  geom_rect(aes(xmin=1.5, xmax=2.5,ymin=-0.004,ymax=0.145), fill = NA, color='black', linetype = 'dashed', size=0.75) + 
  geom_rect(aes(xmin=3.5, xmax=5.5,ymin=-0.004,ymax=0.145), fill = NA, color='black', linetype = 'dashed', size=0.75)
ggsave(filename = file.path(basepath, "EmpiricalPlot_Symptoms.png"),  width = 6, height = 4, dpi = 500)

############### EmpiricalPlot_IC, FigX ################
data <- data.frame(group = factor(c(rep('HC',12),rep('PS',8),rep('NS',4),rep('NA',1),rep('BA',2),rep('WD',2)),
  levels = c('HC','PS','NS','NA','BA','WD')),
  exp = c(main_data$IC[13:24], main_data$IC[1:8], main_data$IC[9:12], 0, main_data$IC[9:10],main_data$IC[11:12]))
ggplot(data = data, mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = NA, size = 0.9)+ 
  geom_jitter(data = subset(data, group != "NA"), size = 1)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.7)+ 
  theme_prism(axis_text_angle = 0)+ 
  theme(legend.direction = "vertical",
    axis.line = element_line(color = 'black', size = 0.75),
    axis.ticks = element_line(color = 'black', size = 0.75),
    axis.text = element_text(size=12, color="black", face='plain'),  
    axis.title = element_text(size=14, face='plain')) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = c('#66c2a5','#fc8d62','#8da0cb', 'transparent', '#e78ac3','#a6d854'))+ 
  scale_x_discrete(breaks = c('HC','PS','NS','BA','WD'), labels=c('HC','PS','NS','BA','WD'))+
  labs(x=NULL, y="IC")+
  geom_rect(aes(xmin=2.5, xmax=3.5,ymin=-0.02,ymax=0.4), fill = NA, color='black', linetype = 'dashed', size=0.75) + 
  geom_rect(aes(xmin=4.5, xmax=6.5,ymin=-0.02,ymax=0.4), fill = NA, color='black', linetype = 'dashed', size=0.75)
ggsave(filename = file.path(basepath, "EmpiricalPlot_IC.png"),  width = 5, height = 4, dpi = 500)


################## EmpiricalPlot_WS, FigX ########################
data <- data.frame(group = factor(c(rep('HC',12),rep('PS',8),rep('NS',4),rep('NA',1),rep('BA',2),rep('WD',2)),
  levels = c('HC','PS','NS','NA','BA','WD')),
  exp = c(main_data$WS[13:24], main_data$WS[1:8], main_data$WS[9:12], 0, main_data$WS[9:10],main_data$WS[11:12]))
ggplot(data = data, mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = NA, size = 0.9)+ 
  geom_jitter(data = subset(data, group!= 'NA'), size = 1)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.7)+ 
  theme_prism(axis_text_angle = 0)+ 
  theme(legend.direction = "vertical",
    axis.line = element_line(color = 'black', size = 0.75),
    axis.ticks = element_line(color = 'black', size = 0.75),
    axis.text = element_text(size=12, color="black", face='plain'),  
    axis.title = element_text(size=14, face='plain')) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = c('#66c2a5','#fc8d62','#8da0cb', 'transparent', '#e78ac3','#a6d854'))+ 
  scale_x_discrete(breaks = c('HC','PS','NS','BA','WD'), labels=c('HC','PS','NS','BA','WD'))+
  labs(x=NULL, y="WS")+
  geom_rect(aes(xmin=2.5, xmax=3.5,ymin=-0.02,ymax=0.65), fill = NA, color='black', linetype = 'dashed', size=0.75) + 
  geom_rect(aes(xmin=4.5, xmax=6.5,ymin=-0.02,ymax=0.65), fill = NA, color='black', linetype = 'dashed', size=0.75)
ggsave(filename = file.path(basepath, "EmpiricalPlot_WS.png"),  width = 5, height = 4, dpi = 500)

############### SingleShot_Plots, FigX ###################
file_path <- "C:/Users/XinHao/Desktop/2025_SCZ_AI/simulation/"
group_levels <- c("HC", "PS", "NS", "NA", "BA", "WD")
fill_colors <- c('#66c2a5','#fc8d62','#8da0cb', 'transparent', '#e78ac3','#a6d854')

plot_metric <- function(metric_name, empirical_values, filename, ylim_max, rect_ymax) {
  # empirical data
  emp_data <- data.frame(
    group = factor(rep(group_levels, times = c(12,8,4,1,2,2)), levels = group_levels),
    exp = empirical_values
  )
  emp_summary <- emp_data %>%
    group_by(group) %>%
    summarise(Mean = mean(exp, na.rm=TRUE), SD = sd(exp, na.rm=TRUE))
  emp_summary[emp_summary$group == "NA", "SD"] <- 0
  print(emp_summary[, c("group", "Mean", "SD")])
  
  # load simulation data
  sim_data <- read_csv(paste0(file_path, 'simulated_performance_ssApr2025.csv')) %>%
    filter(group %in% group_levels) %>%
    mutate(group = factor(group, levels = group_levels))
  
  data_summary <- sim_data %>%
    filter(group %in% c("HC", "PS", "NS", "BA", "WD")) %>%
    group_by(group) %>%
    summarise(mean_bar = mean(.data[[metric_name]], na.rm = TRUE), .groups = "drop")
  data_summary <- rbind(data_summary[1:3, ], data.frame(group = 'NA', mean_bar = 0), data_summary[4:5, ])
  data_summary$group <- factor(data_summary$group, levels = group_levels)
  
  p <- ggplot(data = data_summary, aes(x = group, y = mean_bar, fill = group)) +
    geom_bar(stat = 'identity', position = 'dodge', width = 0.7) +
    geom_point(data = subset(emp_summary, group != 'NA'), aes(x = group, y = Mean, fill = group), size = 1) +
    geom_errorbar(data = subset(emp_summary, group != 'NA'), 
                  aes(x = group, ymin = Mean - SD, ymax = Mean + SD), 
                  inherit.aes = FALSE, width = 0.4, size = 0.7, color = "black") +
    theme_prism(axis_text_angle = 0) +
    theme(
      legend.direction = "vertical",
      axis.line = element_line(color = 'black', size = 0.75),
      axis.ticks = element_line(color = 'black', size = 0.75),
      axis.text = element_text(size = 12, color = "black", face = 'plain'),
      axis.title = element_text(size = 14, face = 'plain')
    ) +
    coord_cartesian(ylim = c(0, ylim_max)) +
    scale_fill_manual(values = fill_colors) +
    scale_x_discrete(breaks = c("HC", "PS", "NS", "BA", "WD"), labels = c("HC", "PS", "NS", "BA", "WD")) +
    labs(x = NULL, y = metric_name) +
    geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = -0.02, ymax = rect_ymax), fill = NA, color = 'black', linetype = 'dashed', size = 0.75) +
    geom_rect(aes(xmin = 4.5, xmax = 6.5, ymin = -0.02, ymax = rect_ymax), fill = NA, color = 'black', linetype = 'dashed', size = 0.75)
  ggsave(filename = file.path(file_path, filename), plot = p, width = 5, height = 4, dpi = 500)
}

# 1. IC 
emp_values_IC <- c(main_data$IC[13:24], main_data$IC[1:8], main_data$IC[9:12], 0, main_data$IC[9:10],main_data$IC[11:12])
plot_metric("IC", emp_values_IC, "SS_IC.png", ylim_max = 1, rect_ymax = 0.4)
# 2. WS
emp_values_WS <- c(main_data$WS[13:24], main_data$WS[1:8], main_data$WS[9:12], 0, main_data$WS[9:10],main_data$WS[11:12])
plot_metric("WS", emp_values_WS, "SS_WS.png", ylim_max = 1, rect_ymax = 0.7)