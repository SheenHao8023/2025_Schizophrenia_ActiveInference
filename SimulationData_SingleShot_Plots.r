packages <- c("ggprism", "dplyr", "ggplot2", "tidyr", "readr")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
})

file_path <- "C:/Users/XinHao/Desktop/2025_SCZ_AI/SimulationData/"
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
  
  # load simulation data
  sim_data <- read_csv(paste0(file_path, 'ss_sim_24jan.csv')) %>%
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
emp_values_IC <- c(0.7367, 0.7188, 0.4632, 0.8143, 0.6753, 0.7538,
                   0.3658, 0.4969, 0.8537, 0.8199, 0.4683, 0.6251,
                   0.2542, 0.5226, 0.4742, 0.6333, 0.3109, 0.3494, 
                   0.4820, 0.2584, 0.2131, 0.3259, 0.1224, 0.2386,
                   0, 0.2131, 0.3259, 0.1224, 0.2386)
plot_metric("IC", emp_values_IC, "SS_IC.png", ylim_max = 1, rect_ymax = 0.4)

# 2. WS
emp_values_WS <- c(0.2023, 0.6309, 0.4933, 0.9190, 0.5873, 0.6375,
                   0.2290, 0.6951, 0.5521, 0.5751, 0.6334, 0.7370,
                   0.2663, 0.3447, 0.1933, 0.2542, 0.1772, 0.7711,
                   0.5780, 0.7090, 0.4235, 0.2482, 0.5376, 0.5890,
                   0, 0.4235, 0.2482, 0.5376, 0.5890)
plot_metric("WS", emp_values_WS, "SS_WS.png", ylim_max = 1, rect_ymax = 0.7)
