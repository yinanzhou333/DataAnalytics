#Apple Quality
library("rpart")
library("caret") #Classification And REgression Training
library("ggplot2")
library("corrplot")
library("rpart.plot")

data <- read.csv("apple_quality.csv", header = TRUE)
summary(data)
# convert the categorical variable "Quality" into a binary numeric variable.
data["Quality"] <- ifelse(data$Quality == "good", 1, 0)

# remove the NA's
data_clean <- data[!is.na(data$A_id),]
# convert the character "Acidity" to numeric
data_clean['Acidity'] <- as.numeric(data_clean$Acidity) 

# split in folds
set.seed(321)  # Repeatability
index <- createDataPartition(y = data_clean$Quality, p = 0.8, list = FALSE) # 80% of the data will be used for training

test_data <- data_clean[-index,]
train_data <- data_clean[index,]

# Assuming 'train_data' is your dataset
cor_matrix <- cor(train_data, use = "complete.obs")
# Create a correlation plot
corrplot(cor_matrix, method = "circle")

barplot(table(train_data$Quality), main = "Quality distribution", xlab = "Quality", ylab = "Frequency")

model <-rpart(Quality~., train_data, method="class", control = rpart.control(cp = 0.01, minsplit = 50, minbucket = 50))
#model <-rpart(Quality~., train_data, method="class")
printcp(model)
rpart.plot(model)

train_predictions <- predict(model, newdata = train_data, type = "class")

# Make predictions on the testing data
test_predictions <- predict(model, newdata = test_data, type = "class")

# Calculate training accuracy
train_accuracy <- sum(train_predictions == train_data$Quality) / length(train_data$Quality)
cat("Training Accuracy:", train_accuracy, "\n")

# Calculate testing accuracy
test_accuracy <- sum(test_predictions == test_data$Quality) / length(test_data$Quality)
cat("Testing Accuracy:", test_accuracy, "\n")

selected_data <- train_data[train_data$Juiciness>=-0.42 & train_data$Ripeness<1.7 & train_data$Weight>=2.1, ]
cor_matrix1 <- cor(selected_data, use = "complete.obs")
corrplot(cor_matrix1, method = "circle")

test_data1 <- test_data[1:5,1:8]
test_predictions <- predict(model, newdata = test_data1, type = "class")

