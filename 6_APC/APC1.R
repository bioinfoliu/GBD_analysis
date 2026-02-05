# install.packages("prepare_rates")
# Clear the workspace
rm(list=ls())

# Load required libraries
library(magrittr)
library(dplyr)
library(data.table)

# Load custom APC functions (Ensure these files exist in your working directory)
source('source_apc.R')
source('function_year5.R')

# Step 1: Load and inspect the main dataset
TB_KOR <- fread('TB_KOR_GLOBAL.csv')
str(TB_KOR)

# Define age group lists for filtering
# Note: Added " years" to 80-84, 85-89, 90-94 to ensure consistency with the dataset
age_groups <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
                "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
                "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
                "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")

#### Section 2: Data Extraction for APC Analysis (e.g., Mortality/Incidence/DALYs) ####
# Logic Check: Your comment said "Incidence," but the code below filters for "Deaths". 
# Ensure measure_name matches your research target.
TB_subset <- subset(TB_KOR,
                    (TB_KOR$age_name %in% age_groups) &
                      TB_KOR$sex_name == "Both" &
                      TB_KOR$location_name == "Global" &
                      TB_KOR$metric_name == 'Number' &
                      TB_KOR$measure_name == "Deaths") # Change this to "Incidence" if needed

# Clean age group names by removing the " years" suffix
TB_subset$age_name <- gsub(" years", "", TB_subset$age_name)

# Convert age_name to factor with specific levels to maintain correct chronological order
TB_subset$age_name <- factor(TB_subset$age_name, levels = c("<5", "5-9", "10-14", "15-19",
                                                            "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                            "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                                            "90-94", "95+"))

# Select required columns: Age ID, Age Name, Year, and Value
TB_subset <- TB_subset[, c("age_id", "age_name", "year", "val")]

# Reshape data from Long to Wide format (Years as columns)
TB_wide <- dcast(data = TB_subset, age_id + age_name ~ year)

# Aggregate data into 5-year intervals (e.g., 1992-2021)
# Using the custom function 'function_year5'
TB_5year_groups <- function_year5(TB_wide, 1992, 2021, 2021)

# Set row names for consistency
rownames(TB_5year_groups) <- TB_wide$age_name


#### Section 3: Import and Process Population Data ####
# Set path to population data folder
pop_path = "./GBD_population/" # Using relative path for better compatibility
file_list = dir(pop_path)

# Initialize data frame and define column selection
var_selection <- c("location_name", "sex_name", "year", "age_id", "age_name", "val") 
population_raw <- data.frame()

# Loop through files in directory and merge them
for(k in 1:length(file_list)){
  temp_data = read.csv(file = file.path(pop_path, file_list[k]),
                       header = TRUE, stringsAsFactors = FALSE)
  population_raw = rbind(population_raw, temp_data)
}

# Filter population data for Global, specific age groups, and both sexes
population_filtered <- population_raw %>% 
  dplyr::select(all_of(var_selection)) %>% 
  filter(location_name == "Global" & age_name %in% age_groups & sex_name == 'Both')

# Remove " years" suffix for consistency
population_filtered$age_name <- gsub(" years", "", population_filtered$age_name)

# Reshape population data to Wide format
pop_wide <- dcast(data = population_filtered, age_id + age_name ~ year, value.var = "val") 

# Aggregate population into 5-year intervals
pop_5year_groups <- function_year5(pop_wide, 1992, 2021, 2021)
rownames(pop_5year_groups) <- pop_wide$age_name


#### Section 4: Data Synchronization and Final Formatting ####
# Identify common age groups present in both datasets
common_ages <- intersect(rownames(pop_5year_groups), rownames(TB_5year_groups))

# Keep only the overlapping age groups
pop_5year_groups <- pop_5year_groups[rownames(pop_5year_groups) %in% common_ages, ]
TB_5year_groups <- TB_5year_groups[rownames(TB_5year_groups) %in% common_ages, ]

# Rename population columns by adding "p" suffix (required by APC package)
pop_names <- paste0(names(pop_5year_groups), "p")
pop_5year_groups <- pop_5year_groups %>% stats::setNames(pop_names)

# Combine disease counts and population counts side-by-side
final_apc_data <- tibble(cbind(TB_5year_groups, pop_5year_groups)) %>% 
  dplyr::select(`1992-1996`, `1992-1996p`, `1997-2001`, `1997-2001p`,
                `2002-2006`, `2002-2006p`, `2007-2011`, `2007-2011p`, `2012-2016`, `2012-2016p`,
                `2017-2021`, `2017-2021p`)

# Export formatted data for APC model
write.table(final_apc_data, 'TB_APC_input_data.csv', row.names = FALSE, col.names = FALSE, sep = ',')


#### Section 5: APC Model Estimation and Plotting ####
# Pre-process rates for APC modeling
# StartAge should match the first age group in your dataset (e.g., 15)
R <- prepare_rates(final_apc_data,
                   StartYear = 1992, StartAge = 15, Interval = 5,
                   fullname = 'Global TB Analysis', description = 'Analysis by 5-year intervals') 

# Fit the APC model
APC_Model <- apc2fit(R)

# Generate APC visualization plots
plot.apc1(APC_Model)