#Installing and reading data/packages
install.packages('tidyverse')
library('tidyverse')
install.packages('readxl')
library('readxl')
data <- read_excel ('C:/Users/nilab/Documents/datainsamling_grupparbete.xlsx')
scb_data <- read_excel('C:/Users/nilab/Documents/2021_bilar.xlsx')
data <- read_excel('C:/Users/nilab/Documents/datainsamling_grupparbete.xlsx', col_names = TRUE)
scb_data <- read_excel('C:/Users/nilab/Documents/2021_bilar.xlsx', col_names = TRUE)

#Storing data in a dataframe
hästkrafter <- data.frame
bränsle = c(0, 0, 0, 2, 2, 2, 0, 0, 1, 0, 0,2, 1, 0, 0, 1, 2, 1, 2,2, 0, 0, 1, 0, 1, 0, 0, 1, 2, 2, 2, 2, 0, 2, 0, 1, 2, 0, 0, 1, 0, 0, 2, 1, 2, 0, 0, 2, 0, 1, 2, 2, 0, 0, 0, 2, 2, 2, 0, 2, 2, 2, 0, 0, 0,2,0,0, 1,0)
hästkrafter = c(344, 396, 396, 198, 198, 198, 391, 344, 200, 344, 344, 191, 164, 391, 344, 165, 198, 200, 200, 191, 253,391, 164, 340, 200, 340, 340, 200, 198, 198, 198, 200, 253, 192, 406, 198, 198, 253, 342, 198, 344, 344, 198, 165, 198, 344, 198, 198, 396, 165, 198, 192, 344, 340, 344, 191, 198, 198, 235, 191, 198, 198, 344, 344, 344, 191, 406, 253, 198,340)

data <- data.frame(bränsle = bränsle, hästkrafter = hästkrafter)

#View data
print(data)
print(scb_data)

# Create a linear regression model
model <- lm(hästkrafter ~ bränsle, data = data)

#Summary of model
summary(model)

# Predict hästkrafter based on bränsle  
intercept <- coef(model)["(Intercept)"]
coefficient_bränsle <- coef(model)["bränsle"]
predicted_hästkrafter <- intercept + coefficient_bränsle * bränsle
print(predicted_hästkrafter)

#Scatter plot 
library(ggplot2)
y <- data$bränsle
x <- data$hästkrafter

par(mar = c(4, 4, 1, 1))
plot(x, y, main = "Bränsle vs Hästkrafter",
     xlab = "Hästkrafter", ylab = "Bränsle",
     pch = 19)
abline(lm(y ~ x, data = data), col = 'blue' )

print(paste("R-squared:", summary(model)$r.squared))
print(paste("Adjusted R-squared:", summary(model)$adj.r.squared))

# Obtain residuals
residuals <- residuals(model)

# Plot residuals against fitted values
plot(fitted(model), residuals, xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0

# Plot normal Q-Q plot
qqnorm(residuals(model))
qqline(residuals(model))

# Cross validation
install.packages('caret')
library(caret)
set.seed(123)  # For reproducibility
cv_model <- train(hästkrafter ~ bränsle, data = data, method = "lm", trControl = trainControl(method = "cv"))
print(cv_model$results)

# Calculate square root of absolute residuals
sqrt_abs_residuals <- sqrt(abs(residuals))
print(sqrt_abs_residuals)

# Scale-location plot
plot(fitted(model), sqrt_abs_residuals, xlab = "Fitted values", ylab = "√|Residuals|",
     main = "Scale-Location Plot")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0

