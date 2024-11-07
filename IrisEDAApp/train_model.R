# Load necessary libraries
library(readr)
library(dplyr)

# Load the datasets
delhi_data <- read_csv("C:/gurman/Gurman IMP/R LAB/IrisEDAApp/delhi house data.csv")
pune_data <- read_csv("C:/gurman/Gurman IMP/R LAB/IrisEDAApp/pune house data.csv")
bangalore_data <- read_csv("C:/gurman/Gurman IMP/R LAB/IrisEDAApp/bangalore house data.csv")

# Combine the datasets
combined_data <- bind_rows(delhi_data, pune_data, bangalore_data)

# Example preprocessing (modify according to your dataset)
combined_data <- combined_data %>%
    select(price, income, rooms, bedrooms, population) %>%
    mutate(across(where(is.character), as.factor))  # Convert character columns to factors if needed

# Fit the linear regression model
lm_model <- lm(price ~ income + rooms + bedrooms + population, data = combined_data)

# Save the model to an .rds file
saveRDS(lm_model, "C:/gurman/Gurman IMP/R LAB/IrisEDAApp/lm_model.rds")
