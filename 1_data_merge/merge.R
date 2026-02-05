# Set workspace and list files
# Get all file names in the specified path
fileName = dir(path)
fileName

# Initialize an empty data frame to store merged results
newdata <- data.frame()

# Loop through files and merge them into one data frame
for(k in 1:length(fileName)){
  # Use file.path for better cross-platform compatibility instead of manual "\\"
  data = read.csv(file = file.path(path, fileName[k]), 
                  header = TRUE, stringsAsFactors = FALSE)
  newdata = rbind(newdata, data)
}
# Optional: Save the merged raw data
# write.csv(newdata, "total.csv")

# Inspect unique values in location_name to verify data import and identify categories
unique(newdata$location_name)

#### Section 1: Specific Country Data (e.g., Korea)
# Extract subset for Korea
IBD_Korea <- subset(newdata, newdata$location_name == 'Korea') 
write.csv(IBD_Korea, "IBD_Korea.csv")

#### Section 2: 7 Super Regions + Global Data
# Define the list of super regions for filtering
super_regions_list <- c(
  'Central Europe, Eastern Europe, and Central Asia',
  'High-income',
  'Latin America and Caribbean',
  'North Africa and Middle East',
  'South Asia',
  'Southeast Asia, East Asia, and Oceania',
  'Sub-Saharan Africa',
  'Global'
)

# Extract subset using the %in% operator for cleaner code
IBD_super_region <- subset(newdata, location_name %in% super_regions_list)

# Verify the extracted super regions
unique(IBD_super_region$location_name)
write.csv(IBD_super_region, "IBD_super_region.csv")

#### Section 3: 21 Regions
# Define the list of the 21 specific regions
regions_list <- c(
  'High-income Asia Pacific', 'Central Asia', 'Southeast Asia', 'East Asia',
  'Central Europe', 'Eastern Europe', 'North Africa and Middle East',
  'Australasia', 'Western Europe', 'Andean Latin America', 'Caribbean',
  'High-income North America', 'Western Sub-Saharan Africa', 'South Asia',
  'Oceania', 'Central Sub-Saharan Africa', 'Central Latin America',
  'Southern Latin America', 'Tropical Latin America', 'Eastern Sub-Saharan Africa',
  'Southern Sub-Saharan Africa'
)

IBD_region <- subset(newdata, location_name %in% regions_list)

# Verify the extracted regions
unique(IBD_region$location_name)
write.csv(IBD_region, "IBD_region.csv")

#### Section 4: 204 Countries/Territories
# Exclude Super Regions, Regions, and Global to isolate individual country data
IBD_country <- subset(newdata, 
                      !(location_name %in% super_regions_list) & 
                        !(location_name %in% regions_list))

# Verify that only individual countries remain
unique(IBD_country$location_name)
write.csv(IBD_country, "IBD_country.csv")