
ol = read.csv("/Users/berlybrigith/Downloads/SEM 2 FALLL/MGSC 661/FINAL PROJECT R/Dataset2—Olympic_events_data.csv")
attach(ol)


library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(rpart)

# Check for missing values in the dataset
colSums(is.na(ol))
#ID   Name    Sex    Age Height Weight   Team    NOC  Games   Year Season   City 
#Sport  Event  Medal 

# impute missing values for Height, Weight, and Age using median
ol$Height[is.na(ol$Height)] <- median(ol$Height, na.rm = TRUE)
ol$Weight[is.na(ol$Weight)] <- median(ol$Weight, na.rm = TRUE)
ol$Age[is.na(ol$Age)] <- median(ol$Age, na.rm = TRUE)


colSums(is.na(ol))

# medal target variable
ol$Medal[is.na(ol$Medal)] <- "None"

ol$Medal <- ifelse(ol$Medal == "Gold" | ol$Medal == "Silver" | ol$Medal == "Bronze", 1, 0)

# EDA
ggplot(ol, aes(x = Sex)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Sex", x = "Sex", y = "Count")

ggplot(ol, aes(x = Season)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribution of Season", x = "Season", y = "Count")

ggplot(ol, aes(x = Sport)) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Distribution of Sport", x = "Sport", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# age distribution by Medal Status
ggplot(ol, aes(x = Age, fill = factor(Medal))) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Age Distribution by Medal Status", fill = "Having a Medal or Not") +
  theme_minimal()

# correlation matrix for numeric variables
cor_data <- ol %>%
  select(Age, Height, Weight) %>%
  na.omit()

cor_matrix <- cor(cor_data)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black")


#making sure no NA data
missing_counts <- colSums(is.na(ol))

# Display the counts
print(missing_counts)

# data split
ol_model <- subset(ol, select = c('Medal', 'Age', 'Height', 'Weight', 'Sex', 'NOC', 'Season', 'City'))


str(ol_model)


unique(ol_model$'Medal')
unique(ol_model$'Sex')
unique(ol_model$'NOC')
unique(ol_model$'Season')
unique(ol_model$'City')

#make new column BMI
# convert Height from cm to meters
ol_model$Height_m <- ol_model$Height / 100

# calculate BMI
ol_model$BMI <- ol_model$Weight / (ol_model$Height_m^2)

head(ol_model)

ol_model <- ol_model[, !(names(ol_model) %in% c("Height","Height_m", "Weight"))]

str(ol_model)

set.seed(42)  
train_indices <- sample(1:nrow(ol_model), size = 0.7 * nrow(ol_model))
train_data <- ol_model[train_indices, ]
test_data <- ol_model[-train_indices, ]

sum(is.na(train_data))  
sum(is.na(test_data))  

nrow(train_data)
nrow(test_data)

#too many occurances, replacing less frequent ones as Other
# rare categories in other
threshold <- 100  # threshhold
ol_model$NOC <- ifelse(table(ol_model$NOC)[ol$NOC] < threshold, "Other", ol_model$NOC)


ol_model$Sex <- factor(ol_model$Sex)
ol_model$Season <- factor(ol_model$Season)
ol_model$NOC <- factor(ol_model$NOC)
table(ol_model$NOC)







# Load necessary library
library(randomForest)
#RANDOM FOREST

model_rf <- randomForest(Medal ~ BMI*NOC +Age+Sex+ Season, 
                         data = train_data, 
                         ntree = 100)

# Model summary
print(model_rf) #:/

model_rf2 <- randomForest(Medal ~ BMI +Age+Sex+ Season, 
                         data = train_data, 
                         ntree = 100)

# Model summary
print(model_rf2) #lol even worse

#LOGISTIC REGRESSION


#logistic_model <- glm(Medal ~ BMI* NOC + Age + Sex + Season, 
             #         data = ol_model, 
                  #    family = binomial)#taking wayyyy too long

#observation- interaction between BMI and NOC introduced singularities
smaller_data <- ol_model[1:5000, ]  # select a subset of the first 5000 rows
logistic_model <- glm(Medal ~ BMI* NOC + Age + Sex + Season, 
                      data = smaller_data, 
                      family = binomial)

# View model summary
summary(logistic_model)
library(car)

logistic_model2 <- glm(Medal ~ BMI   + Age+Sex + Season, 
                      data = smaller_data, 
                      family = binomial)




vif(logistic_model2)#uh oh

names(ol_model)

cor(ol_model[, c("BMI", "Age")])




# Predict on test data
pred_rf <- predict(model_rf, newdata = test_data)

# Evaluate the model (accuracy)

#CLassification trees

#install.packages("rpart")
#install.packages("rpart.plot")  # For plotting the tree

# Load libraries
library(rpart)
library(rpart.plot)





tree_model2 <- rpart(Medal ~ BMI + Age + Sex + Season, 
                    data = smaller_data, 
                    method = "class", 
                    control = rpart.control(cp = 0.001, minsplit = 20))

summary(tree_model2)

par(mar = c(1, 1, 1, 1))  # Set margins to smaller values
rpart.plot(tree_model2, type = 3, extra = 101)




# Make predictions with the model
predictions <- predict(tree_model2, newdata = smaller_data, type = "class")

# Check the length of predictions and true_labels to ensure they match
length(predictions)
length(smaller_data$Medal)

# Create the confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = smaller_data$Medal)

# Print the confusion matrix
print(confusion_matrix)


# Confusion matrix values
TN <- 4455  # True Negatives
FP <- 18    # False Positives
FN <- 483   # False Negatives
TP <- 44    # True Positives


accuracy <- (TP + TN) / (TP + TN + FP + FN)


precision <- TP / (TP + FP)


recall <- TP / (TP + FN)


f1_score <- 2 * (precision * recall) / (precision + recall)


specificity <- TN / (TN + FP)


balanced_accuracy <- (recall + specificity) / 2


cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")
cat("Specificity:", specificity, "\n")
cat("Balanced Accuracy:", balanced_accuracy, "\n")

##CLASSIFICATION TREE IS THE BEST :D

#trying with NOC
tree_model3 <- rpart(Medal ~ BMI + Age + Sex + Season+NOC, 
                     data = smaller_data, 
                     method = "class", 
                     control = rpart.control(cp = 0.001, minsplit = 20))

summary(tree_model3)

par(mar = c(1, 1, 1, 1))  
rpart.plot(tree_model3, type = 3, extra = 101)

predictions2 <- predict(tree_model3, newdata = smaller_data, type = "class")


length(predictions2)
length(smaller_data$Medal)


confusion_matrix2 <- table(Predicted = predictions2, Actual = smaller_data$Medal)


print(confusion_matrix2)


TN2 <- 4421  
FP2 <- 407  
FN2 <- 52    
TP2 <- 120   


accuracy <- (TP2 + TN2) / (TP2 + TN2 + FP2 + FN2)
print(paste("Accuracy: ", accuracy))


precision <- TP2 / (TP2 + FP2)
print(paste("Precision: ", precision))


recall <- TP2 / (TP2 + FN2)
print(paste("Recall: ", recall))


f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1 Score: ", f1_score))


specificity <- TN2 / (TN2 + FP2)
print(paste("Specificity: ", specificity))


fpr <- FP2 / (FP2 + TN2)
print(paste("False Positive Rate: ", fpr))


fnr <- FN2 / (FN2 + TP2)
print(paste("False Negative Rate: ", fnr))






  #let's try PCA and Clustering :)

olc = read.csv("/Users/berlybrigith/Downloads/SEM 2 FALLL/MGSC 661/FINAL PROJECT R/Dataset2—Olympic_events_data.csv")
attach(olc)

summary(olc)




##pca not relevant here because it is done for numerical variables and 
#there's not many numerical variables here



#gender and years?
# Add 'Sex' to the aesthetics for differentiation
plot <- ggplot(olc, aes(x = Year, y = Age, color = Sex))
plot + geom_point()
#clear increase in participation of women in olympics



#clustering again 

numerical_data <- olc[, sapply(olc, is.numeric)]  


binary_columns <- sapply(olc, function(col) length(unique(col)) == 2 && !is.numeric(col))
binary_data <- olc[, binary_columns]  


binary_data <- as.data.frame(lapply(binary_data, function(x) as.numeric(factor(x))))


clustering_data <- cbind(numerical_data, binary_data)

clustering_data <- as.data.frame(lapply(clustering_data, function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
}))

clustering_data_scaled <- scale(clustering_data)

km_2 <- kmeans(clustering_data_scaled, centers = 4, nstart = 25)

olc$cluster <- as.factor(km_2$cluster)

library(ggplot2)
ggplot(olc, aes(x = Year, y = Age, color = cluster)) +
  geom_point() +
  labs(title = "Clustering with K-means", x = "Year", y = "Age") +
  theme_minimal()


km_3 <- kmeans(clustering_data_scaled, centers = 3, nstart = 25)

olc$cluster <- as.factor(km_3$cluster)

library(ggplot2)
ggplot(olc, aes(x = Year, y = Age, color = cluster)) +
  geom_point() +
  labs(title = "Clustering with K-means", x = "Year", y = "Age") +
  theme_minimal()


wss_km_2 <- km_2$tot.withinss
cat("WSS for K-means with 4 clusters:", wss_km_2, "\n")
#WSS for K-means with 4 clusters: 1081101 

wss_km_3 <- km_3$tot.withinss
cat("WSS for K-means with 3 clusters:", wss_km_3, "\n")
#WSS for K-means with 3 clusters: 1255301 



# Elbow Method to find the optimal number of clusters
wss_values <- sapply(1:10, function(k) {
  kmeans_result <- kmeans(clustering_data_scaled, centers = k, nstart = 25)
  return(kmeans_result$tot.withinss)
})

plot(1:10, wss_values, 
     type = "b", 
     xlab = "Number of Clusters (k)", 
     ylab = "WSS", 
     main = "Elbow Method for WSS",
     col = "#FF6F61", # Coral pink color hex code
     pch = 19,        # Solid circle for points
     lwd = 2)         # Line width





















#######################
##APPENDIX##
#######################


#REALLY BAD CLUSTERING EXAMPLE

set.seed(123)  
olc_subset <- olc[sample(1:nrow(olc), 10000), ]


olc_subset[] <- lapply(olc_subset, function(col) {
  if (is.character(col)) as.factor(col) else col
})


str(olc_subset) 


library(cluster)
gower_dist <- daisy(olc_subset, metric = "gower")


hc <- hclust(as.dist(gower_dist), method = "complete")


olc_subset$cluster <- cutree(hc, k = 3)  


library(ggplot2)
plot <- ggplot(olc_subset, aes(x = Year, y = Age, color = as.factor(cluster)))
plot + geom_point() +
  labs(
    title = "Clustering with Gower's Distance (Subset)",
    x = "Year",
    y = "Age"
  ) +
  theme_minimal()

numeric_cols <- sapply(olc_subset, is.numeric)
numeric_data <- olc_subset[, numeric_cols]

wcss <- sum(sapply(unique(olc_subset$cluster), function(cluster) {
  cluster_data <- numeric_data[olc_subset$cluster == cluster, ]
  sum((cluster_data - colMeans(cluster_data, na.rm = TRUE))^2, na.rm = TRUE)
}))

print(paste("Within-Cluster Sum of Squares (WCSS):", wcss))
#k=3 "Within-Cluster Sum of Squares (WCSS): 86539470329513 :(







