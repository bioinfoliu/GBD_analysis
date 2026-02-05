# Tuberculosis Burden Analysis (Korea vs. Global, 1990–2021)

This repository contains the source code and data datasets used in the study: **"Tuberculosis burden in Korea compared with global trends, 1990–2021: a comparative ecological time-trend study."**

The analysis includes temporal trend assessment using Joinpoint regression, Age-Period-Cohort (APC) analysis, and future forecasting using ARIMA models.

## 📂 Repository Structure

The project is organized into the following directories corresponding to the analysis steps described in the manuscript:

* **`1_data_merge/`**: Scripts for data cleaning and merging raw datasets extracted from the GBD 2021 results tool.
* **`2_Table1/`**: Code for generating the baseline characteristics and summary statistics (Table 1 in the manuscript).
* **`3_age_gender/`**: Scripts for analyzing and visualizing age- and gender-specific TB burden (related to Figures 2 & 3).
* **`4_dual_axis/`**: Visualization scripts for dual-axis plots used in the study.
* **`5_Joinpoint/`**: Data preparation and settings for the Joinpoint Regression Program.
* **`6_APC/`**: R scripts for the Age-Period-Cohort analysis (Wald test, model fitting).
* **`7_ARIMA/`**: **R scripts for the ARIMA forecasting models** (forecasting TB incidence and mortality through 2030).
* **`GBD_population/`** & **`tuberculosis_dalys_data.xlsx`**: Raw data files used as input for the analyses.

## 🛠️ Software & Requirements

The analysis was performed using the following software:

* **R Statistical Software** (for APC analysis, ARIMA forecasting, and data visualization).
    * Key packages: `forecast`, `tseries`, `ggplot2`, `apc`, etc.
* **Joinpoint Regression Program** (Version 5.2.0, National Cancer Institute).

## 🚀 How to Use

1.  **Data Preparation**: Ensure the raw data files (`.xlsx` or `.csv`) are located in the root directory or the respective folders.
2.  **Running the Analysis**:
    * For forecasting results, navigate to the `7_ARIMA` folder and run the R scripts.
    * For cohort effects, refer to the `6_APC` folder.

## 📞 Contact

For any questions regarding the code or data, please contact the corresponding author of the manuscript.