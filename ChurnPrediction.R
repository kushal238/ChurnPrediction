churn_data = read.csv('/Users/kushalagarwal/Downloads/customer_churn.csv')
# Look at the first 6 observations
head(churn_data)
# Check the dimension
dim(churn_data)

# Change the column names
names(churn_data) = gsub(" ", "", names(churn_data))
head(churn_data)

# Fit the multiple linear regression model
cust_value_model = lm(formula = Customer.Value ~ Call.Failure + 
                        Complaints + Subscription.Length + Charge.Amount +    
                        Seconds.of.Use +Frequency.of.use + Frequency.of.SMS +
                        Distinct.Called.Numbers + Age.Group + Tariff.Plan +
                        Status + Age,data = churn_data)
summary(cust_value_model)

par(mfrow= c(2,2))
plot(cust_value_model)



library(ggcorrplot)

# Remove the Customer Value column
reduced_data <- subset(churn_data, select = -Customer.Value)

# Compute correlation at 2 decimal places
corr_matrix = round(cor(reduced_data), 2)

# Compute and show the  result
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

# removing predictors with high multi colinearity 
# (age group & seconds of use)
colinearity_model = lm(formula = Customer.Value ~ Call.Failure + 
                         Complaints + Subscription.Length + Charge.Amount +    
                         Seconds.of.Use + Frequency.of.SMS +
                         Distinct.Called.Numbers + Tariff.Plan +
                         Status + Age,data = churn_data)
summary(colinearity_model)

# Anova test
anova(cust_value_model, colinearity_model)

# removing predictors that do not habve a high individual statistical signficance
significant_only_model = lm(formula = Customer.Value ~ 
                              Subscription.Length + Charge.Amount +    
                              Seconds.of.Use +Frequency.of.use + Frequency.of.SMS +
                              Distinct.Called.Numbers + Tariff.Plan +
                              Status + Age,data = churn_data)
summary(significant_only_model)
# F-statistic: 1.907e+04 on 9 and 3140 DF,  p-value: < 2.2e-16

anova(cust_value_model, significant_only_model)
# 0.1201, so reduced better

par(mfrow= c(2,2))
plot(significant_only_model)

#### prediction

# Convert Churn to a factor
churn_data$Churn <- as.factor(churn_data$Churn)

# Splitting the dataset into training and testing sets
set.seed(123) # For reproducibility
train_indices <- sample(1:nrow(churn_data), size = 0.8*nrow(churn_data))
train_data <- churn_data[train_indices, ]
test_data <- churn_data[-train_indices, ]

# Fitting a logistic regression model
churn_model <- glm(Churn ~ Call.Failure + Complaints + Subscription.Length + Charge.Amount +    
                     Seconds.of.Use + Frequency.of.use + Frequency.of.SMS +
                     Distinct.Called.Numbers + Age.Group + Tariff.Plan +
                     Status + Age, 
                   data = train_data, family = binomial)

# Summary of the model to check coefficients and overall fit
summary(churn_model)

# Predicting on the test set
predictions <- predict(churn_model, test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Evaluating the model
library(caret)
confusionMatrix(factor(predicted_classes), test_data$Churn)
