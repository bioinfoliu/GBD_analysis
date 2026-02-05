############################################
# Load required libraries
############################################
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(ggpubr)

############################################
# Read data
############################################
IBD_china <- read.csv("TB_KOR_GLOBAL.csv", header = TRUE)

# Check variable structure
str(IBD_china$metric_name)
table(IBD_china$metric_name)
table(IBD_china$measure_name)

############################################
# Step 1: Line plot of prevalence by age group
############################################

# Define age groups (5-year intervals)
age1 <- c(
        "<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
        "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
        "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
        "75-79 years","80-84 years","85-89 years","90-94 years","95+ years"
)

# Extract prevalence data for 2021
data1 <- subset(
        IBD_china,
        year == 2021 &                         # Select year 2021
                age_name %in% age1 &                 # Select age groups
                sex_name != "Both" &                 # Separate males and females
                location_name == "Republic of Korea" &  # Select Republic of Korea
                metric_name == "Rate" &              # Use rate metric
                measure_name == "Prevalence"         # Prevalence
)

# Select variables for analysis
data1 <- data1[, c("sex_name", "age_name", "val", "upper", "lower")]

# Clean age labels
data1$age_name <- gsub(" years", "", data1$age_name)

# Convert age to ordered factor
data1$age_name <- factor(
        data1$age_name,
        levels = c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                   "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                   "75-79","80-84","85-89","90-94","95+")
)

# Sort by sex and age
data1 <- data1[order(data1$sex_name, data1$age_name), ]

# Round values to two decimal places
data1$val <- round(data1$val, 2)

# Convert sex to factor
data1$Sex <- as.factor(data1$sex_name)

############################################
# Step 2: Line plot of incidence by age group
############################################
data2 <- subset(
        IBD_china,
        year == 2021 &
                age_name %in% age1 &
                sex_name != "Both" &
                location_name == "Republic of Korea" &
                metric_name == "Rate" &
                measure_name == "Incidence"
)

data2 <- data2[, c("sex_name", "age_name", "val", "upper", "lower")]
data2$age_name <- gsub(" years", "", data2$age_name)
data2$age_name <- factor(data2$age_name, levels = levels(data1$age_name))
data2 <- data2[order(data2$sex_name, data2$age_name), ]
data2$val <- round(data2$val, 2)
data2$Sex <- as.factor(data2$sex_name)

############################################
# Step 3: Line plot of death rate by age group
############################################
data3 <- subset(
        IBD_china,
        year == 2021 &
                age_name %in% age1 &
                sex_name != "Both" &
                location_name == "Republic of Korea" &
                metric_name == "Rate" &
                measure_name == "Deaths"
)

data3 <- data3[, c("sex_name", "age_name", "val", "upper", "lower")]
data3$age_name <- gsub(" years", "", data3$age_name)
data3$age_name <- factor(data3$age_name, levels = levels(data1$age_name))
data3 <- data3[order(data3$sex_name, data3$age_name), ]
data3$val <- round(data3$val, 2)
data3$Sex <- as.factor(data3$sex_name)

############################################
# Step 4: Line plot of DALYs rate by age group
############################################
data4 <- subset(
        IBD_china,
        year == 2021 &
                age_name %in% age1 &
                sex_name != "Both" &
                location_name == "Republic of Korea" &
                metric_name == "Rate" &
                measure_name == "DALYs (Disability-Adjusted Life Years)"
)

data4 <- data4[, c("sex_name", "age_name", "val", "upper", "lower")]
data4$age_name <- gsub(" years", "", data4$age_name)
data4$age_name <- factor(data4$age_name, levels = levels(data1$age_name))
data4 <- data4[order(data4$sex_name, data4$age_name), ]
data4$val <- round(data4$val, 2)
data4$Sex <- as.factor(data4$sex_name)

############################################
# Custom theme: white background with black border
############################################
custom_theme <- theme_minimal() +
        theme(
                panel.border = element_rect(color = "black", fill = NA, size = 1),
                axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        )

############################################
# Plotting
############################################
p1 <- ggplot(data1, aes(x = age_name, y = val, color = Sex, group = Sex)) +
        geom_line() +
        geom_point(size = 1) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = Sex),
                    alpha = 0.1, color = NA) +
        labs(x = "Age", y = "Rate of Prevalence") +
        scale_color_manual(values = c("steelblue", "#e31a1c")) +
        scale_fill_manual(values = c("steelblue", "#e31a1c")) +
        custom_theme

p2 <- ggplot(data2, aes(x = age_name, y = val, color = Sex, group = Sex)) +
        geom_line() +
        geom_point(size = 1.2) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = Sex),
                    alpha = 0.1, color = NA) +
        labs(x = "Age", y = "Rate of Incidence") +
        scale_color_manual(values = c("steelblue", "#e31a1c")) +
        scale_fill_manual(values = c("steelblue", "#e31a1c")) +
        custom_theme

p3 <- ggplot(data3, aes(x = age_name, y = val, color = Sex, group = Sex)) +
        geom_line() +
        geom_point(size = 1.2) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = Sex),
                    alpha = 0.1, color = NA) +
        labs(x = "Age", y = "Rate of Deaths") +
        scale_color_manual(values = c("steelblue", "#e31a1c")) +
        scale_fill_manual(values = c("steelblue", "#e31a1c")) +
        custom_theme

p4 <- ggplot(data4, aes(x = age_name, y = val, color = Sex, group = Sex)) +
        geom_line() +
        geom_point(size = 1.2) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = Sex),
                    alpha = 0.1, color = NA) +
        labs(x = "Age", y = "Rate of DALYs") +
        scale_color_manual(values = c("steelblue", "#e31a1c")) +
        scale_fill_manual(values = c("steelblue", "#e31a1c")) +
        custom_theme

############################################
# Arrange and save plots
############################################
arranged_plots <- ggarrange(p1, p3, p2, p4, ncol = 2, nrow = 2)

print(arranged_plots)

ggsave("arranged_plots.pdf", arranged_plots, width = 16, height = 12)
