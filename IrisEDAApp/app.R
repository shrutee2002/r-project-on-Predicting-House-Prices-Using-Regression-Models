# Load required libraries
library(shiny)
library(readr)
library(dplyr)

# Load datasets
delhi_data <- read_csv("C:/gurman/Gurman IMP/R LAB/IrisEDAApp/delhi house data.csv")
pune_data <- read_csv("C:/gurman/Gurman IMP/R LAB/IrisEDAApp/pune house data.csv")
bangalore_data <- read_csv("C:/gurman/Gurman IMP/R LAB/IrisEDAApp/bangalore house data.csv")

# Preprocess the datasets
delhi_data <- delhi_data %>%
  mutate(Area = as.numeric(Area))  # Ensure Area is numeric

pune_data <- pune_data %>%
  mutate(Area = as.numeric(total_sqft))  # Convert 'total_sqft' to numeric

bangalore_data <- bangalore_data %>%
  mutate(Area = as.numeric(total_sqft))  # Convert 'total_sqft' to numeric

# Combine datasets
combined_data <- bind_rows(
  delhi_data %>% mutate(City = "Delhi"),
  pune_data %>% mutate(City = "Pune"),
  bangalore_data %>% mutate(City = "Bangalore")
)

# Preprocess combined data
combined_data <- combined_data %>%
  mutate(across(where(is.character), as.factor))  # Convert character columns to factors

# Fit the linear regression model
lm_model <- lm(Price ~ BHK + Bathroom + Area, data = combined_data)

# Save the model to an .rds file
saveRDS(lm_model, "C:/gurman/Gurman IMP/R LAB/IrisEDAApp/lm_model.rds")  # Ensure the path is correct

# Define the UI
ui <- fluidPage(
  titlePanel("Bangalore House Price Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("rooms", "Total Rooms:", 5, min = 1, max = 20),
      numericInput("bedrooms", "Total Bedrooms (BHK):", 2, min = 1, max = 10),
      numericInput("bathrooms", "Total Bathrooms:", 2, min = 1, max = 10),
      numericInput("area", "Total Area (in sqft):", 1000, min = 100, max = 10000),  # New area input
      actionButton("predict", "Predict House Price")
    ),
    
    mainPanel(
      h3("Predicted House Price:"),
      textOutput("prediction")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Load the pre-trained model
  lm_model <- readRDS("C:/gurman/Gurman IMP/R LAB/IrisEDAApp/lm_model.rds")
  
  observeEvent(input$predict, {
    tryCatch({
      # Create a new data frame for prediction
      new_data <- data.frame(
        Area = input$area,
        BHK = input$bedrooms,
        Bathroom = input$bathrooms,
        Total_Rooms = input$rooms  # Include total rooms in prediction
      )
      
      # Make the prediction
      predicted_price <- predict(lm_model, newdata = new_data)
      
      # Set custom price logic
      base_price_per_bhk <- 5000000  # Base price for 1 BHK
      additional_price_per_bathroom <- 2000000  # Additional price for each bathroom
      price_adjustment <- (input$bedrooms - 1) * base_price_per_bhk + 
                          (input$bathrooms) * additional_price_per_bathroom

      # Final price
      custom_price <- price_adjustment + (input$area * 1000)  # Adjust by Area too

      # Use the custom price if it's higher than the predicted price
      final_price <- max(predicted_price, custom_price)

      # Check if the final price is negative
      output$prediction <- renderText({
        if (final_price < 0) {
          "Estimated House Price: Not Available"
        } else {
          price_formatted <- formatC(final_price, format = "f", big.mark = ",", digits = 2)
          paste("Estimated House Price: ₹", price_formatted)
        }
      })
      
    }, error = function(e) {
      # Catch any error and print the error message
      output$prediction <- renderText({
        paste("Error: ", e$message)
      })
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
