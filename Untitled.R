library(ggplot2)
library(dplyr)
library(glmnet)
library(randomForest)

heart_disease = read.csv("/Users/ayushkumar/Documents/practical/CHS/R/archive/heart.csv")

head(heart_disease, 10)

# check that only numeric variables
lapply(heart_disease, class)
summary(heart_disease)

# scaling data and saving as a data frame
scaled = scale(heart_disease)
summary(scaled)



# Filter the data for cases where target is 1
target_1_data <- filter(heart_disease, output == 1)
head(target_1_data)
target_0_data <- filter(heart_disease, output == 0)
head(target_0_data)




#Pie chart for people having heart attack due to chest pain 
target_1_data %>%
group_by(cp) %>%
summarise(count = n()) %>%
ggplot(aes(x = "", y = count, fill = as.factor(cp), label = paste0(round(count/sum(count)*100), "%"))) +
geom_bar(stat = "identity", width = 1, color = "white") +
coord_polar("y") +
geom_text(aes(label = paste0(round(count/sum(count)*100), "%")), position = position_stack(vjust = 0.5)) +
theme_void() +
theme(legend.position = "bottom") +
labs(title = "Distribution of Chest Pain Types (target = 1)") +
scale_fill_discrete(labels = c("Chest Pain Level 1", "Chest Pain Level 2", "Chest Pain Level 3", "Chest Pain Level 4"))


#Age vs Resting BP plot graph
ggplot(heart_disease, aes(x = age, y = trtbps)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Age vs. Resting Blood Pressure",
       x = "Age",
       y = "Resting Blood Pressure")

#Creating histograms for heart attack changes based on age

# Create age groups
age_groups <- cut(target_1_data$age, breaks = seq(0, max(target_1_data$age) + 10, by = 10), include.lowest = TRUE, right = FALSE)

histogram_data <- table(age_groups)

barplot(histogram_data, col = "lightblue", main = "Histogram of Age Groups with Output 1", xlab = "Age Groups", ylab = "Frequency")
axis(1, at = seq(5, max(data_output_1$age), by = 10), labels = seq(0, max(data_output_1$age) - 5, by = 10))




boxplot(chol ~ output, data = heart_disease, 
        xlab = "Output (0 = less chance of heart attack, 1 = more chance of heart attack)", 
        ylab = "Cholesterol",
        main = "Box Plot of Cholesterol by Output")

grid()



ggplot(heart_disease, aes(x = age, y = thalachh)) +
  geom_density_2d() +
  labs(title = "Density Plot: Age vs. Heart Rate",
       x = "Age",
       y = "Heart Rate (thalach)")

























#feature extraction using Linear Regression
full_model <- lm(output ~ ., data = heart_disease)

step_model <- step(full_model)
summary(step_model)

selected_features <- names(step_model$coefficients[which(step_model$coefficients != 0)])
top5_features <- names(coefficients(step_model)[order(abs(coefficients(step_model)), decreasing = TRUE)][1:5])
least5_features <- names(coefficients(step_model)[order(abs(coefficients(step_model)), decreasing = FALSE)][1:5])
cat("Top 5 features:", top5_features, "\n")
cat("Least 5 features:", least5_features, "\n")


#feature extraction using RandomForest

importance_feature <- randomForest(as.factor(output)~ age+ sex+ cp+ trtbps+ chol+ fbs+ restecg+ thalachh+ exng +oldpeak+ slp+ caa+ thall,data = heart_disease ,ntree=500 )
importance(importance_feature)
varImpPlot(importance_feature)

