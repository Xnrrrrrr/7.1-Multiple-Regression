# Install and load necessary packages
install.packages("leaps")
install.packages("car")

# Load required packages
library(leaps)
library(car)

# Read data into a dataframe named cyberData
cyberData <- read.delim("lab 7 sra 365 wc-1.dat")

# Read the data into a table
cyberData <- read.table("lab 7 sra 365 wc-1.dat", header = TRUE, sep = "\t")

# Display the structure of the dataframe
str(cyberData)

# Convert selected variables to factors, tells R it is categorical
cyberData$event_ID <- factor(cyberData$event_ID)
cyberData$data_type <- factor(cyberData$data_type)
cyberData$num_people_v2 <- factor(cyberData$num_people_v2)
cyberData$per_sensitive_v2 <- factor(cyberData$per_sensitive_v2)


# Use the best subsets approach to determine the best predictors for cost_controls
bestsubsets <- regsubsets(cost_controls ~ num_people + num_records + per_sensitive + dys_impact + dys_detect, data = cyberData, nbest = 1)
subsets(bestsubsets, statistic = "adjr2")

# Print summary of bestsubsets
summary(bestsubsets)

# Plot the best subsets graph
plot(summary(bestsubsets)$adjr2, xlab = "Number of Predictors", ylab = "Adjusted R-squared", type = "l")

# Identify the point with the maximum Adjusted R-squared
best_one_variable <- which.max(summary(bestsubsets)$adjr2)
points(best_one_variable, summary(bestsubsets)$adjr2[best_one_variable], col = "red", pch = 16, cex = 1.5)

# Run separate regression models for each subset size (Model 1 to Model 5)
best_models <- regsubsets(cost_controls ~ num_people + num_records + per_sensitive + dys_impact + dys_detect, data = cyberData, nbest = 5)

# Running separate regression models and printing Adjusted R-squared
# Model 1:
model1Reg <- lm(cost_controls ~ num_people, data = cyberData)
summary(model1Reg)

# Model 2:
model2Reg <- lm(cost_controls ~ num_people + num_records, data = cyberData)
summary(model2Reg)

# Model 3:
model3Reg <- lm(cost_controls ~ num_people + num_records + per_sensitive, data = cyberData)
summary(model3Reg)

# Model 4:
model4Reg <- lm(cost_controls ~ num_people + num_records + per_sensitive + dys_impact, data = cyberData)
summary(model4Reg)

# Model 5:
model5Reg <- lm(cost_controls ~ num_people + num_records + per_sensitive + dys_impact + dys_detect, data = cyberData)
summary(model5Reg)

# QUEST 7

# Model 5 because model 5 has the highest value, 
# therfore would be the best fit for predicting the cost of controls

# Evaluate multicollinerity for model 5
# Assuming you have already fitted Model 5
model5 <- lm(cost_controls ~ num_people + num_records + per_sensitive + dys_impact + dys_detect, data = cyberData)

# Load the car package for VIF calculation
library(car)

# Calculate VIF
vif_values <- car::vif(model5)

# Report the highest VIF and lowest tolerance
highest_vif <- max(vif_values)
lowest_tolerance <- 1/highest_vif

# Print the results
cat("The highest VIF among predictors is:", highest_vif, "\n")
cat("The lowest tolerance among predictors is:", lowest_tolerance, "\n")

#QUEST 9 
# Lower tolerance vlaue indicates a potential concern for multicollneity, since lower
# tolerance is below 2, it suggests a potential concern ( 0.1552424)
# A VIF exceeding 5 or 10 is considered an indication of multicolllenrity

#Use R to generate a correlation matrix for the predictors used in Model 5.
# Assuming you have a data frame named 'cyberData' with the relevant predictors
predictors_model_5 <- cyberData[, c("num_people", "num_records", "per_sensitive", "dys_impact", "dys_detect", "cost_controls")]

# Calculate the correlation matrix
cor_matrix <- cor(predictors_model_5)

# Find the pair of predictors with the strongest correlation
max_cor <- max(cor_matrix[upper.tri(cor_matrix, diag = FALSE)])
indices <- which(cor_matrix == max_cor, arr.ind = TRUE)

# Extract variable names
variable_names <- rownames(cor_matrix)

# Display the result
strongest_correlation <- variable_names[indices[, 1]]
cat("The strongest correlation is between", strongest_correlation[1], "and", strongest_correlation[2], "with a correlation value of", max_cor, "\n")

