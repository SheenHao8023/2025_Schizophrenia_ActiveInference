packages <- c("ggprism", "ggsci", "ggpubr", "ggplot2", "dplyr", "tidyr", 'readxl', 'openxlsx', 'stringr')
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

# Computing and normalizing index
raw_dir <- "C:/Users/haox8/Desktop/empirical_data/behav_rawfiles"
main_file <- "C:/Users/haox8/Desktop/empirical_data/data_behav&sympt.xlsx"
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
    IC = coalesce(IC.y, IC.x),
    WS = coalesce(WS.y, WS.x)
  ) %>%
  select(-IC.x, -IC.y, -WS.x, -WS.y)
main_data$IC <- 1 / (1 + exp(-scale(main_data$IC)))
main_data$WS <- 1 / (1 + exp(-scale(main_data$WS)))
write.xlsx(main_data, main_file, overwrite = TRUE)

# SymptomsPlot, FigX
data <- data.frame( 
  "A" = c(0.084745763,0.09375,0.075,0.136986301,0.065934066,0.090909091,0.095238095,0.12962963,0.125,0.053571429,0.055555556,0.070422535,0,0.125,0.053571429,0.055555556,0.070422535),
  "B" = c(0.06779661,0.0625,0.025,0.02739726,0.021978022,0.060606061,0.031746032,0.037037037,0.03125,0.035714286,0.111111111,0.112676056,0,0.03125,0.035714286,0.111111111,0.112676056),
  "C" = c(0.06779661,0.0625,0.025,0.02739726,0.021978022,0.060606061,0.031746032,0.074074074,0.125,0.071428571,0.111111111,0.084507042,0,0.125,0.071428571,0.111111111,0.084507042))
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
  theme_minimal() +
  theme(legend.direction = "vertical", legend.position = 'right',
        panel.grid = element_blank(),
        axis.line = element_line(color = 'black', size = 0.75),
        axis.ticks = element_line(color = 'black', size = 0.75),
        axis.text = element_text(size = 12, color = "black", face = 'plain'),  
        axis.title = element_text(size = 14, face = 'plain')) +
  geom_rect(aes(xmin=1.5, xmax=2.5,ymin=-0.004,ymax=0.145), fill = NA, color='black', linetype = 'dashed', size=0.75) + 
  geom_rect(aes(xmin=3.5, xmax=5.5,ymin=-0.004,ymax=0.145), fill = NA, color='black', linetype = 'dashed', size=0.75)
ggsave(filename = file.path("C:/Users/XinHao/Desktop/2025_SCZ_AI/SampleSymptomData", "SymptomsPlot.png"),  width = 6, height = 4, dpi = 500)

# BehaviorPlot_IC, FigX
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
ggsave(filename = file.path(raw_dir, "BehaviorPlot_IC.png"),  width = 5, height = 4, dpi = 500)


# BehaviorPlot_WS, FigX
data <- data.frame(group = factor(c(rep('HC',12),rep('PS',8),rep('NS',4),rep('NA',1),rep('BA',2),rep('WD',2)),
  levels = c('HC','PS','NS','NA','BA','WD')),
  exp = c(0.202281279176387, 0.630912927170263, 0.493304878268097, 0.918978968429336, 
          0.587325895507046, 0.637496186087674, 0.229037385500683, 0.69514316108911, 
          0.552119687441578, 0.575134538823462, 0.633376077265468, 0.736980635980614, 
          0.266276389621375, 0.34467419855378, 0.193338418384778, 0.254210502557368, 
          0.177190082140837, 0.771078858722738, 0.578044979758555, 0.709004680909732, 
          0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917, 0,
          0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917))
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
ggsave(filename = file.path("C:/Users/XinHao/Desktop/2025_SCZ_AI/BehaviorData", "BehaviorPlot_WS.png"),  width = 5, height = 4, dpi = 500)
