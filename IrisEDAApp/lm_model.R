# Load required libraries
library(readr)
library(dplyr)

# Load datasets
delhi_data <- read_csv("C:/gurman/Gurman IMP/R LAB/IrisEDAApp/delhi house data.csv")
pune_data <- read_csv("C:/gurman/Gurman IMP/R LAB/IrisEDAApp/pune house data.csv")
bangalore_data <- read_csv("C:/gurman/Gurman IMP/R LAB/IrisEDAApp/bangalore house data.csv")

# Check for non-numeric values in Pune and Bangalore datasets
# For Pune
non_numeric_pune <- pune_data %>% 
  filter(!grepl("^[0-9]*\\.?[0-9]+$", total_sqft))  # Regex to match numeric values
print("Non-numeric values in Pune total_sqft:")
print(non_numeric_pune)

# For Bangalore
non_numeric_bangalore <- bangalore_data %>% 
  filter(!grepl("^[0-9]*\\.?[0-9]+$", total_sqft))
print("Non-numeric values in Bangalore total_sqft:")
print(non_numeric_bangalore)

# Clean the total_sqft column by removing non-numeric rows if needed
pune_data <- pune_data %>% 
  filter(grepl("^[0-9]*\\.?[0-9]+$", total_sqft)) %>%
  mutate(Area = as.numeric(total_sqft))  # Convert to numeric after filtering

bangalore_data <- bangalore_data %>% 
  filter(grepl("^[0-9]*\\.?[0-9]+$", total_sqft)) %>%
  mutate(Area = as.numeric(total_sqft))  # Convert to numeric after filtering

# Combine and preprocess the data
combined_data <- bind_rows(
    delhi_data %>% select(Price, BHK, Bathroom, Area),
    pune_data %>% select(price = price, BHK = bath, Bathroom = bath, Area),
    bangalore_data %>% select(price = price, BHK = bath, Bathroom = bath, Area)
)

# Check the combined data structure
print("Combined Data Structure:")
print(head(combined_data))

# Preprocess the combined data
combined_data <- combined_data %>%
    mutate(across(where(is.character), as.factor))  # Convert character columns to factors if necessary

# Fit the linear regression model
lm_model <- lm(Price ~ BHK + Bathroom + Area, data = combined_data)

# Save the model to an .rds file
saveRDS(lm_model, "C:/gurman/Gurman IMP/R LAB/IrisEDAApp/lm_model.rds")
