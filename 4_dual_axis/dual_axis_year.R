#### Trend Analysis of Disease Burden by Year, Gender, and Age Group ####

# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(tidyr)
library(ggpubr) # Required for ggarrange

# Clear workspace
rm(list=ls())

##### Step 1: Prevalence Analysis ######
# Load Tuberculosis data for Korea
TB_Korea <- read.csv('TB_KOR_GLOBAL.csv', header = TRUE)  

# Filter necessary data for Prevalence
# Exclude 'Both' genders, select Republic of Korea, and keep Number/Rate metrics
data1 <- subset(TB_Korea,
                TB_Korea$sex_name != "Both" & 
                  TB_Korea$location_name == "Republic of Korea" & 
                  (TB_Korea$metric_name %in% c('Rate','Number')) & 
                  TB_Korea$measure_name == 'Prevalence') 

# Check unique metrics and data structure
unique(data1$metric_name)
str(data1)

# Select relevant columns for plotting
data2 <- data1[, c("sex_name", "age_name", "metric_name", "year", "val", "upper", "lower")] 

# Plot 1: Prevalence Trends (Dual Axis)
# Bar chart represents total numbers for "All ages"
# Line chart represents "Age-standardized" rates
p1 <- ggplot() +
  geom_bar(data = subset(data2, age_name == "All ages"), 
           mapping = aes(x = year, y = val, fill = sex_name), 
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           width = 0.7) +
  
  geom_errorbar(data = subset(data2, age_name == "All ages"),  
                mapping = aes(x = year, ymin = lower, ymax = upper, 
                              group = sex_name), 
                position = position_dodge(width = 0.8), 
                width = 0.25, 
                color = "black") +
  
  geom_line(data = subset(data2, age_name == "Age-standardized"),
            aes(x = year, y = val * 300, color = sex_name),
            linewidth = 1) + # Size is deprecated, use linewidth
  geom_ribbon(data = subset(data2, age_name == "Age-standardized"),
              aes(x = year, ymin = lower * 300, ymax = upper * 300,
                  fill = sex_name),
              alpha = 0.2) +
  scale_y_continuous(
    name = "Prevalence Number",
    sec.axis = sec_axis(~ . / 300, name = "Age-standardized Prevalence rate per 100,000")
  ) +
  scale_fill_manual(values = c("#ffaa00", "#1240ab", "blue", "red"), name = "Number") +
  scale_color_manual(values = c("#ffaa00", "#1240ab"), name = "Rate") +
  labs(x = "Year") +
  theme_minimal()

print(p1)

# Calculate Gender Difference for Prevalence
data_filtered <- data2 %>%
  filter(age_name == "Age-standardized" & metric_name == "Rate")

# Convert to wide format to calculate difference (Male - Female)
gender_diff <- data_filtered %>%
  select(year, sex_name, val) %>%
  spread(key = sex_name, value = val) %>%
  mutate(diff = Male - Female)

# Identify years with maximum and minimum gender disparities
max_diff <- gender_diff[which.max(gender_diff$diff), ]
min_diff <- gender_diff[which.min(gender_diff$diff), ]

# Plot Gender Difference (Prevalence)
p_diff_prev <- ggplot(gender_diff, aes(x = year, y = diff)) +
  geom_line(color = "#1f78b4", linewidth = 1.2) +
  geom_point(color = "darkorange", size = 3) +
  geom_point(data = max_diff, aes(x = year, y = diff), color = "green", shape = 17, size = 4) +
  geom_point(data = min_diff, aes(x = year, y = diff), color = "purple", shape = 15, size = 4) +
  ggtitle("Difference in TB Cases Between Genders (Prevalence)") +
  labs(x = "Year", y = "Difference (Male - Female)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5))

print(p_diff_prev)

############################################# Step 2: Incidence ####################################
# Extract data for Incidence
data3 <- subset(TB_Korea,
                TB_Korea$sex_name != "Both" & 
                  TB_Korea$location_name == "Republic of Korea" & 
                  (TB_Korea$metric_name %in% c('Rate','Number')) & 
                  TB_Korea$measure_name == 'Incidence')

data4 <- data3[, c("sex_name", "age_name", "metric_name", "year", "val", "upper", "lower")] 

# Plot 2: Incidence Trends (Dual Axis)
p2 <- ggplot() +
  geom_bar(data = subset(data4, age_name == "All ages"), 
           mapping = aes(x = year, y = val, fill = sex_name), 
           stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(data = subset(data4, age_name == "All ages"),  
                mapping = aes(x = year, ymin = lower, ymax = upper, group = sex_name), 
                position = position_dodge(width = 0.8), width = 0.25, color = "black") +
  geom_line(data = subset(data4, age_name == "Age-standardized"),
            aes(x = year, y = val * 400, color = sex_name), linewidth = 1) +
  geom_ribbon(data = subset(data4, age_name == "Age-standardized"),
              aes(x = year, ymin = lower * 400, ymax = upper * 400, fill = sex_name), alpha = 0.2) +
  scale_y_continuous(
    name = "Incidence Number",
    sec.axis = sec_axis(~ . / 400, name = "Age-standardized Incidence rate per 100,000")
  ) +
  scale_fill_manual(values = c("#ffaa00", "#1240ab", "blue", "red"), name = "Number") +
  scale_color_manual(values = c("#ffaa00", "#1240ab"), name = "Rate") +
  labs(x = "Year") +
  theme_minimal()

print(p2)

############################################# Step 3: Deaths ####################################
# Extract data for Deaths
data5 <- subset(TB_Korea,
                TB_Korea$sex_name != "Both" & 
                  TB_Korea$location_name == "Republic of Korea" & 
                  (TB_Korea$metric_name %in% c('Rate','Number')) & 
                  TB_Korea$measure_name == 'Deaths')

data6 <- data5[, c("sex_name", "age_name", "metric_name", "year", "val", "upper", "lower")] 

# Plot 3: Death Trends (Dual Axis)
p3 <- ggplot() +
  geom_bar(data = subset(data6, age_name == "All ages"), 
           mapping = aes(x = year, y = val, fill = sex_name), 
           stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(data = subset(data6, age_name == "All ages"),  
                mapping = aes(x = year, ymin = lower, ymax = upper, group = sex_name), 
                position = position_dodge(width = 0.8), width = 0.25, color = "black") +
  geom_line(data = subset(data6, age_name == "Age-standardized"),
            aes(x = year, y = val * 200, color = sex_name), linewidth = 1) +
  geom_ribbon(data = subset(data6, age_name == "Age-standardized"),
              aes(x = year, ymin = lower * 200, ymax = upper * 200, fill = sex_name), alpha = 0.2) +
  scale_y_continuous(
    name = "Deaths Number",
    limits = c(0, 16000),
    sec.axis = sec_axis(~ ./200, name = "Age-standardized Deaths rate per 100,000")
  ) +
  scale_fill_manual(values = c("#ffaa00", "#1240ab", "blue", "red"), name = "Number") +
  scale_color_manual(values = c("#ffaa00", "#1240ab"), name = "Rate") +
  labs(x = "Year") +
  theme_minimal() +
  xlim(1990, NA)

print(p3)

############################################# Step 4: DALYs ####################################
# Extract data for Disability-Adjusted Life Years (DALYs)
data7 <- subset(TB_Korea,
                TB_Korea$sex_name != "Both" & 
                  TB_Korea$location_name == "Republic of Korea" & 
                  (TB_Korea$metric_name %in% c('Rate','Number')) & 
                  TB_Korea$measure_name == 'DALYs (Disability-Adjusted Life Years)')

data8 <- data7[, c("sex_name", "age_name", "metric_name", "year", "val", "upper", "lower")] 

# Plot 4: DALYs Trends (Dual Axis)
p4 <- ggplot() +
  geom_bar(data = subset(data8, age_name == "All ages"), 
           mapping = aes(x = year, y = val, fill = sex_name), 
           stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(data = subset(data8, age_name == "All ages"),  
                mapping = aes(x = year, ymin = lower, ymax = upper, group = sex_name), 
                position = position_dodge(width = 0.8), width = 0.25, color = "black") +
  geom_line(data = subset(data8, age_name == "Age-standardized"),
            aes(x = year, y = val * 200, color = sex_name), linewidth = 1) +
  geom_ribbon(data = subset(data8, age_name == "Age-standardized"),
              aes(x = year, ymin = lower * 200, ymax = upper * 200, fill = sex_name), alpha = 0.2) +
  scale_y_continuous(
    name = "DALYs Number",
    sec.axis = sec_axis(~ . / 200, name = "Age-standardized DALYs rate per 100,000")
  ) +
  scale_fill_manual(values = c("#ffaa00", "#1240ab", "blue", "red"), name = "Number") +
  scale_color_manual(values = c("#ffaa00", "#1240ab"), name = "Rate") +
  labs(x = "Year") +
  theme_minimal() +
  xlim(1990, NA)

print(p4)

############################# Combine Subplots #########################

# Define a theme element to add black borders to plots
black_border_theme <- theme(
  panel.background = element_rect(fill = "white", color = "black"),
  panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
)

# Apply border theme to all subplots
p1 <- p1 + black_border_theme
p2 <- p2 + black_border_theme
p3 <- p3 + black_border_theme
p4 <- p4 + black_border_theme

# Combine all 4 metrics into a single multi-panel figure
combined_plot <- ggarrange(p1, p2, p3, p4, 
                           nrow = 2, 
                           ncol = 2, 
                           common.legend = TRUE, 
                           legend = "bottom", 
                           labels = c("(a)", "(b)", "(c)", "(d)"),   # Add subplot labels
                           label.x = 0, 
                           label.y = 1.02, 
                           font.label = list(size = 14, face = "plain"))

# Display and save the combined plot
print(combined_plot)
ggsave("Dual_Axis_Trends_Republic_of_Korea.png", combined_plot, 
       width = 12, height = 9, units = "in", 
       bg = "white", dpi = 300)