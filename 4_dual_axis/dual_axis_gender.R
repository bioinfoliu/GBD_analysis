# Load required libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(readxl)
library(cowplot)

# Load Tuberculosis dataset
TB_KOR <- read.csv('TB_KOR_GLOBAL.csv', header = TRUE) 

# Define the 20 specific age groups for analysis
age1 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")

##### Section 1: Incidence Analysis ######

# Extract 'Number' metric for Global Incidence in 1990 and 2021
data1 <- subset(TB_KOR,
                (TB_KOR$year == 1990 | TB_KOR$year == 2021) &
                  TB_KOR$sex_name == "Both" &
                  TB_KOR$location_name == "Global" & 
                  TB_KOR$metric_name == 'Number' &
                  TB_KOR$measure_name == 'Incidence' &
                  TB_KOR$age_name %in% age1)

# Extract 'Rate' metric for Global Incidence in 1990 and 2021
data2 <- subset(TB_KOR,
                (TB_KOR$year == 1990 | TB_KOR$year == 2021) &
                  TB_KOR$sex_name == "Both" &
                  TB_KOR$location_name == "Global" &
                  TB_KOR$metric_name == 'Rate' &
                  TB_KOR$measure_name == 'Incidence' &
                  TB_KOR$age_name %in% age1)

# Merge datasets and clean age labels
data_in <- rbind(data1, data2)
data_in$age_name <- gsub(" years", "", data_in$age_name)
data_in$year <- as.factor(data_in$year)

# Reorder age groups as factors for correct plotting sequence
data_in$age_name <- factor(data_in$age_name, levels = c("<5", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+"))

# Sort data by gender and age
data_in <- data_in[order(data_in$sex_name, data_in$age_name), ]

# Calculate scale factor for dual-axis (Numbers vs Rates)
max_number_1 <- max(data_in$val[data_in$metric_name == "Number"], na.rm = TRUE)
max_rate_1 <- max(data_in$val[data_in$metric_name == "Rate"], na.rm = TRUE)
scale_factor_1 <- max_number_1 / max_rate_1  

# Generate Plot 1: Incidence
p1 <- ggplot() +
  # Bar chart for absolute Numbers (Primary Y-axis)
  geom_bar(data = subset(data_in, metric_name == "Number"),
           aes(x = age_name, y = val, fill = year),
           stat = "identity", position = position_dodge(0.8), width = 0.7) +
  # Error bars for Numbers
  geom_errorbar(data = subset(data_in, metric_name == "Number"),
                aes(x = age_name, ymin = lower, ymax = upper, group = year),
                position = position_dodge(0.8), width = 0.25, color = "black") +
  # Line chart for Rates (Secondary Y-axis, scaled)
  geom_line(data = subset(data_in, metric_name == "Rate"),
            aes(x = age_name, y = val * scale_factor_1, color = year, group = year),
            linewidth = 1) +
  # Confidence intervals (Ribbon) for Rates
  geom_ribbon(data = subset(data_in, metric_name == "Rate"),
              aes(x = age_name, ymin = lower * scale_factor_1,
                  ymax = upper * scale_factor_1, fill = year, group = year),
              alpha = 0.2) +
  # Define Dual Axes
  scale_y_continuous(
    name = "Incidence Number",
    sec.axis = sec_axis(~ . / scale_factor_1, name = "Crude Incidence rate per 100,000")
  ) +
  scale_fill_manual(values = c("#C0392B", "#1E8449"), name = "Number") +
  scale_color_manual(values = c("#C0392B", "#1E8449"), name = "Rate") +
  labs(x = "Age Group") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  )

##### Section 2: Prevalence Analysis ######
# Data extraction and scaling logic mirrors Section 1
data3 <- subset(TB_KOR, (TB_KOR$year == 1990 | TB_KOR$year == 2021) & TB_KOR$sex_name == "Both" & TB_KOR$location_name == "Global" & TB_KOR$metric_name == 'Number' & TB_KOR$measure_name == 'Prevalence' & TB_KOR$age_name %in% age1)
data4 <- subset(TB_KOR, (TB_KOR$year == 1990 | TB_KOR$year == 2021) & TB_KOR$sex_name == "Both" & TB_KOR$location_name == "Global" & TB_KOR$metric_name == 'Rate' & TB_KOR$measure_name == 'Prevalence' & TB_KOR$age_name %in% age1)

data_pr <- rbind(data3, data4)
data_pr$age_name <- gsub(" years", "", data_pr$age_name)
data_pr$year <- as.factor(data_pr$year)
data_pr$age_name <- factor(data_pr$age_name, levels = levels(data_in$age_name))
data_pr <- data_pr[order(data_pr$sex_name, data_pr$age_name), ]

max_number_2 <- max(data_pr$val[data_pr$metric_name == "Number"], na.rm = TRUE)
max_rate_2 <- max(data_pr$val[data_pr$metric_name == "Rate"], na.rm = TRUE)
scale_factor_2 <- max_number_2 / max_rate_2

p2 <- ggplot() +
  geom_bar(data = subset(data_pr, metric_name == "Number"), aes(x = age_name, y = val, fill = year), stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(data = subset(data_pr, metric_name == "Number"), aes(x = age_name, ymin = lower, ymax = upper, group = year), position = position_dodge(width = 0.8), width = 0.25, color = "black") +
  geom_line(data = subset(data_pr, metric_name == "Rate"), aes(x = age_name, y = val * scale_factor_2, color = year, group = year), linewidth = 1) +
  geom_ribbon(data = subset(data_pr, metric_name == "Rate"), aes(x = age_name, ymin = lower * scale_factor_2, ymax = upper * scale_factor_2, fill = year, group = year), alpha = 0.2) +
  scale_y_continuous(name = "Prevalence Number", labels = comma, sec.axis = sec_axis(~ . / scale_factor_2, name = "Crude Prevalence rate per 100,000")) +
  scale_fill_manual(values = c("#C0392B", "#1E8449", "blue", "red"), name = "Number") +
  scale_color_manual(values = c("#C0392B", "#1E8449"), name = "Rate") +
  labs(x = "Age Group") + theme_minimal() + theme(panel.background = element_rect(fill = "white", color = "black"), axis.text.x = element_text(angle = 90, hjust = 1))

##### Section 3: Deaths Analysis ######
data5 <- subset(TB_KOR, (TB_KOR$year == 1990 | TB_KOR$year == 2021) & TB_KOR$sex_name == "Both" & TB_KOR$location_name == "Global" & TB_KOR$metric_name == 'Number' & TB_KOR$measure_name == 'Deaths' & TB_KOR$age_name %in% age1)
data6 <- subset(TB_KOR, (TB_KOR$year == 1990 | TB_KOR$year == 2021) & TB_KOR$sex_name == "Both" & TB_KOR$location_name == "Global" & TB_KOR$metric_name == 'Rate' & TB_KOR$measure_name == 'Deaths' & TB_KOR$age_name %in% age1)

data_de <- rbind(data5, data6)
data_de$age_name <- gsub(" years", "", data_de$age_name)
data_de$year <- as.factor(data_de$year)
data_de$age_name <- factor(data_de$age_name, levels = levels(data_in$age_name))
data_de <- data_de[order(data_de$sex_name, data_de$age_name), ]

max_number_3 <- max(data_de$val[data_de$metric_name == "Number"], na.rm = TRUE)
max_rate_3 <- max(data_de$val[data_de$metric_name == "Rate"], na.rm = TRUE)
scale_factor_3 <- max_number_3 / max_rate_3

p3 <- ggplot() +
  geom_bar(data = subset(data_de, metric_name == "Number"), aes(x = age_name, y = val, fill = year), stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(data = subset(data_de, metric_name == "Number"), aes(x = age_name, ymin = lower, ymax = upper, group = year), position = position_dodge(width = 0.8), width = 0.25, color = "black") +
  geom_line(data = subset(data_de, metric_name == "Rate"), aes(x = age_name, y = val * scale_factor_3, color = year, group = year), linewidth = 1) +
  geom_ribbon(data = subset(data_de, metric_name == "Rate"), aes(x = age_name, ymin = lower * scale_factor_3, ymax = upper * scale_factor_3, fill = year, group = year), alpha = 0.2) +
  scale_y_continuous(name = "Deaths Number", sec.axis = sec_axis(~ . / scale_factor_3, name = "Crude Deaths rate per 100,000")) +
  scale_fill_manual(values = c("#C0392B", "#1E8449", "blue", "red"), name = "Number") +
  scale_color_manual(values = c("#C0392B", "#1E8449"), name = "Rate") +
  labs(x = "Age Group") + theme_minimal() + theme(panel.background = element_rect(fill = "white", color = "black"), axis.text.x = element_text(angle = 90, hjust = 1))

##### Section 4: DALYs Analysis ######
data7 <- subset(TB_KOR, (TB_KOR$year == 1990 | TB_KOR$year == 2021) & TB_KOR$sex_name == "Both" & TB_KOR$location_name == "Global" & TB_KOR$metric_name == 'Number' & TB_KOR$measure_name == 'DALYs (Disability-Adjusted Life Years)' & TB_KOR$age_name %in% age1)
data8 <- subset(TB_KOR, (TB_KOR$year == 1990 | TB_KOR$year == 2021) & TB_KOR$sex_name == "Both" & TB_KOR$location_name == "Global" & TB_KOR$metric_name == 'Rate' & TB_KOR$measure_name == 'DALYs (Disability-Adjusted Life Years)' & TB_KOR$age_name %in% age1)

data_daly <- rbind(data7, data8)
data_daly$age_name <- gsub(" years", "", data_daly$age_name)
data_daly$year <- as.factor(data_daly$year)
data_daly$age_name <- factor(data_daly$age_name, levels = levels(data_in$age_name))
data_daly <- data_daly[order(data_daly$sex_name, data_daly$age_name), ]

max_number_4 <- max(data_daly$val[data_daly$metric_name == "Number"], na.rm = TRUE)
max_rate_4 <- max(data_daly$val[data_daly$metric_name == "Rate"], na.rm = TRUE)
scale_factor_4 <- max_number_4 / max_rate_4

p4 <- ggplot() +
  geom_bar(data = subset(data_daly, metric_name == "Number"), aes(x = age_name, y = val, fill = year), stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(data = subset(data_daly, metric_name == "Number"), aes(x = age_name, ymin = lower, ymax = upper, group = year), position = position_dodge(width = 0.8), width = 0.25, color = "black") +
  geom_line(data = subset(data_daly, metric_name == "Rate"), aes(x = age_name, y = val * scale_factor_4, color = year, group = year), linewidth = 1) +
  geom_ribbon(data = subset(data_daly, metric_name == "Rate"), aes(x = age_name, ymin = lower * scale_factor_4, ymax = upper * scale_factor_4, fill = year, group = year), alpha = 0.2) +
  scale_y_continuous(name = "DALYs Number", sec.axis = sec_axis(~ . / scale_factor_4, name = "Crude DALYs rate per 100,000")) +
  scale_fill_manual(values = c("#C0392B", "#1E8449", "blue", "red"), name = "Number") +
  scale_color_manual(values = c("#C0392B", "#1E8449"), name = "Rate") +
  labs(x = "Age Group") + theme_minimal() + theme(panel.background = element_rect(fill = "white", color = "black"), axis.text.x = element_text(angle = 90, hjust = 1))

##### Combined Visualization ######

# Extract shared legend from Plot 4
legend <- get_legend(p4 + theme(legend.position = "bottom"))

# Remove legends from individual plots to prepare for grid merge
p1_nolegend <- p1 + theme(legend.position = "none")
p2_nolegend <- p2 + theme(legend.position = "none")
p3_nolegend <- p3 + theme(legend.position = "none")
p4_nolegend <- p4 + theme(legend.position = "none")

# Arrange the four plots in a 2x2 grid
main_grid <- plot_grid(
  p1_nolegend, p2_nolegend,
  p3_nolegend, p4_nolegend,
  labels = c("(a)", "(b)", "(c)", "(d)"),
  label_size = 12,
  label_x = -0.01,
  nrow = 2
)

# Combine the grid with the shared legend at the bottom
final_plot <- plot_grid(
  main_grid,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.08)
)

# Render and save the final image
print(final_plot)
ggsave("TB_Statistics_global_fixed.png", final_plot,
       width = 12, height = 8, dpi = 300, bg = "white")