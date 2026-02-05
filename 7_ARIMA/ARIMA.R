library(forecast)
library(ggplot2)
library(readxl)
library(ggpubr)

# Clear the workspace environment
rm(list=ls())

##### 1. Male ASIR (Age-standardized Incidence Rate) Data Import #####
# Read the specific sheet for male incidence
male_in <- read_excel("TB/ARIMA-korea.xlsx", sheet = 2)
# Convert to time series object starting from 1990
male_in_ts <- ts(male_in$val, start = 1990, frequency = 1)
plot(male_in_ts)

# Fit the ARIMA model automatically
fit1 <- auto.arima(male_in_ts)
fit1
summary(fit1)

# Generate forecast for the next 9 years and handle non-negative constraints
forecasted_values1 <- forecast(fit1, h = 9)
forecasted_values1$mean[forecasted_values1$mean < 0] <- 0
forecasted_values1$lower[forecasted_values1$lower < 0] <- 0
forecasted_values1$upper[forecasted_values1$upper < 0] <- 0

# Convert forecast results into a data frame for plotting
forecast_df1 <- data.frame(
  Year = c(time(male_in_ts), time(forecasted_values1$mean)),
  Value = c(as.numeric(male_in_ts), as.numeric(forecasted_values1$mean)),
  Type = c(rep("Actual", length(male_in_ts)), rep("Forecast", length(forecasted_values1$mean)))
)

# Visualize the forecast results
p1 <- ggplot() +
  geom_line(data = forecast_df1, aes(x = Year, y = Value, color = Type), size = 1.2) +
  geom_point(data = forecast_df1[forecast_df1$Type == "Forecast", ], 
             aes(x = Year, y = Value, color = Type), size = 2, shape = 21, 
             fill = "#FFD700", color = "black", stroke = 0.5) +
  # Add 95% Confidence Interval ribbon
  geom_ribbon(data = data.frame(
    Year = time(forecasted_values1$mean),
    ymin = forecasted_values1$lower[,2],
    ymax = forecasted_values1$upper[,2]
  ), aes(x = Year, ymin = ymin, ymax = ymax), fill = "#FFD700", alpha = 0.2) +
  # Add a vertical line to indicate the start of the forecast period
  geom_vline(xintercept = 2021, linetype = "dashed", color = "grey", size = 1) +
  scale_color_manual(values = c("Actual" = "#8B0000", "Forecast" = "#FFD700")) +
  labs(title = "Age-standardized Incidence Rate of Male", x = "Year", y = "ASIR") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0, vjust = 1, face = "bold", size = 14)) +
  # Annotate forecast points with values
  geom_text(data = forecast_df1[forecast_df1$Type == "Forecast", ],
            aes(x = Year, y = Value, label = sprintf("%.2f", Value)),
            vjust = -1, color = "black", size = 3)

##### 2. Female ASIR Data Import #####
female_in <- read_excel("TB/ARIMA-korea.xlsx", sheet = 1)
female_in_ts <- ts(female_in$val, start = 1990, frequency = 1)

fit2 <- auto.arima(female_in_ts)
forecasted_values2 <- forecast(fit2, h = 9)

# Post-process forecast to ensure non-negative values
forecasted_values2$mean[forecasted_values2$mean < 0] <- 0
forecasted_values2$lower[forecasted_values2$lower < 0] <- 0
forecasted_values2$upper[forecasted_values2$upper < 0] <- 0

forecast_df2 <- data.frame(
  Year = c(time(female_in_ts), time(forecasted_values2$mean)),
  Value = c(as.numeric(female_in_ts), as.numeric(forecasted_values2$mean)),
  Type = c(rep("Actual", length(female_in_ts)), rep("Forecast", length(forecasted_values2$mean)))
)

p2 <- ggplot() +
  geom_line(data = forecast_df2, aes(x = Year, y = Value, color = Type), size = 1.2) +
  geom_point(data = forecast_df2[forecast_df2$Type == "Forecast", ], aes(x = Year, y = Value, color = Type), size = 2, shape = 21, fill = "yellow", color = "black", stroke = 0.5) +
  geom_ribbon(data = data.frame(
    Year = time(forecasted_values2$mean),
    ymin = forecasted_values2$lower[,2],
    ymax = forecasted_values2$upper[,2]
  ), aes(x = Year, ymin = ymin, ymax = ymax), fill = "#FFD700", alpha = 0.2) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "grey", size = 1) +
  scale_color_manual(values = c("Actual" = "#8B0000", "Forecast" = "#FFD700")) +
  labs(title = "Age-standardized Incidence Rate of Female", x = "Year", y = "ASIR") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0, vjust = 1, face = "bold", size = 14)) +
  geom_text(data = forecast_df2[forecast_df2$Type == "Forecast", ], # Fixed logic error in original label reference
            aes(x = Year, y = Value, label = sprintf("%.2f", Value)),
            vjust = -1, color = "black", size = 3)

##### 3. Male ASDR (Age-standardized Death Rate) Data Import #####
male_de <- read_excel("TB/ARIMA-korea.xlsx", sheet = 4)
male_de_ts <- ts(male_de$val, start = 1990, frequency = 1)

fit3 <- auto.arima(male_de_ts)
forecasted_values3 <- forecast(fit3, h = 9)
forecasted_values3$mean[forecasted_values3$mean < 0] <- 0
forecasted_values3$lower[forecasted_values3$lower < 0] <- 0
forecasted_values3$upper[forecasted_values3$upper < 0] <- 0

forecast_df3 <- data.frame(
  Year = c(time(male_de_ts), time(forecasted_values3$mean)),
  Value = c(as.numeric(male_de_ts), as.numeric(forecasted_values3$mean)),
  Type = c(rep("Actual", length(male_de_ts)), rep("Forecast", length(forecasted_values3$mean)))
)

p3 <- ggplot() +
  geom_line(data = forecast_df3, aes(x = Year, y = Value, color = Type), size = 1.2) +
  geom_point(data = forecast_df3[forecast_df3$Type == "Forecast", ], aes(x = Year, y = Value, color = Type), size = 2, shape = 21, fill = "yellow", color = "black", stroke = 0.5) +
  geom_ribbon(data = data.frame(
    Year = time(forecasted_values3$mean),
    ymin = forecasted_values3$lower[,2],
    ymax = forecasted_values3$upper[,2]
  ), aes(x = Year, ymin = ymin, ymax = ymax), fill = "#FFD700", alpha = 0.2) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "grey", size = 1) +
  scale_color_manual(values = c("Actual" = "#006400", "Forecast" = "#FFD700")) +
  labs(title = "Age-standardized Death Rate of Male", x = "Year", y = "ASDR") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0, vjust = 1, face = "bold", size = 14))

##### 4. Female ASDR Data Import #####
female_de <- read_excel("TB/ARIMA-korea.xlsx", sheet = 3)
female_de_ts <- ts(female_de$val, start = 1990, frequency = 1)

fit4 <- auto.arima(female_de_ts)
forecasted_values4 <- forecast(fit4, h = 9)
forecasted_values4$mean[forecasted_values4$mean < 0] <- 0
forecasted_values4$lower[forecasted_values4$lower < 0] <- 0
forecasted_values4$upper[forecasted_values4$upper < 0] <- 0

forecast_df4 <- data.frame(
  Year = c(time(female_de_ts), time(forecasted_values4$mean)),
  Value = c(as.numeric(female_de_ts), as.numeric(forecasted_values4$mean)),
  Type = c(rep("Actual", length(female_de_ts)), rep("Forecast", length(forecasted_values4$mean)))
)

p4 <- ggplot() +
  geom_line(data = forecast_df4, aes(x = Year, y = Value, color = Type), size = 1.2) +
  geom_point(data = forecast_df4[forecast_df4$Type == "Forecast", ], aes(x = Year, y = Value, color = Type), size = 2, shape = 21, fill = "yellow", color = "black", stroke = 0.5) +
  geom_ribbon(data = data.frame(
    Year = time(forecasted_values4$mean),
    ymin = forecasted_values4$lower[,2],
    ymax = forecasted_values4$upper[,2]
  ), aes(x = Year, ymin = ymin, ymax = ymax), fill = "#FFD700", alpha = 0.2) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "grey", size = 1) +
  scale_color_manual(values = c("Actual" = "#006400", "Forecast" = "#FFD700")) +
  labs(title = "Age-standardized Death Rate of Female", x = "Year", y = "ASDR") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0, vjust = 1, face = "bold", size = 14))

# Combine all four plots into one multi-panel figure
ggarrange(p1, p2, p3, p4, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)


############################# Supplementary Table Generation ################################

library(dplyr)

# Summary: Combine all forecast results into a single table for export
# (Processing steps for fits 1-4 are repeated here to create the combined data frame)

all_forecasts <- bind_rows(forecast_df1, forecast_df2, forecast_df3, forecast_df4) %>%
  mutate(Gender = rep(c("Male", "Female", "Male", "Female"), each = nrow(forecast_df1)),
         Indicator = rep(c("ASIR", "ASIR", "ASDR", "ASDR"), each = nrow(forecast_df1)))

# Export the combined actual and forecast results to a CSV file
write.csv(all_forecasts, "korea_forecast_results.csv", row.names = FALSE)