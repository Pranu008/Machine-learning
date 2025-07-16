# Load required libraries
library(tidyverse)
library(modelr)
library(broom)

# Create data
data <- tibble(
  ads = c(10, 15, 20, 25),
  sales = c(100, 130, 190, 233)
)

# Fit linear model (sales = f(ads))
model <- lm(sales ~ ads, data = data)

# Tidy model output
model %>% tidy()
model %>% glance()

# Predict and calculate residuals
data_with_preds <- data %>%
  add_predictions(model) %>%
  mutate(residual = sales - pred)

# Calculate Mean Squared Error (MSE)
mse <- data_with_preds %>%
  summarise(mse = mean(residual^2))

print(mse)

# Predict new values
new_data <- tibble(ads = c(12, 18))
predictions <- predict(model, newdata = new_data)

# Bind predictions to new_data
pr_df <- bind_cols(new_data, tibble(predicted_sales = predictions))

# Plot original data, regression line, and predicted points
data %>%
  ggplot(aes(x = ads, y = sales)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(data = pr_df, aes(x = ads, y = predicted_sales), size = 5, color = "green") +
  labs(
    title = "Linear Regression Example (tidyverse)",
    x = "Ad Budget",
    y = "Sales"
  )


#Create a model for recover and property stolen for both vales and cases
library (tidyverse)
library(broom)
library(modelr)
library(plotly)

read.csv("10_Property_stolen_and_recovered.csv") ->df
view()
model <-lm(Cases_Property_Stolen~Cases_Property_Recovered,data = df)
model

df %>%
  ggplot(aes(x = Cases_Property_Stolen, y = Cases_Property_Recovered,color=Area_Name)) +
  geom_point( size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Linear Regression Example (Tidyverse)",
    x = "Cases_Property_Stolen",
    y= "Cases_Property_Recovered"
  )->df1
ggplotly(df1)

