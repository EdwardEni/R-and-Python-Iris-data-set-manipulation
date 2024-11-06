#Load the iris data set
data(iris)

#Display the first few rows of the data set
#use head() function to see first six rows

head(iris)

#Explore the data set

#summary() to get quick summary
summary(iris)

#str() to observe the structure of data set
str(iris)


#DATA VISUALIZATIONS

#In R, the GGally package (an extension of ggplot2) provides a function ggpairs to create a pair plot.
#we create a scatterplot matrix to visualize the relationships between different features and how they correlate across species.

#Pair plot
# Install and load necessary packages
install.packages("GGally")
library(GGally)

# Create a pair plot (scatterplot matrix)
ggpairs(iris, aes(color = Species, alpha = 0.5))

#Box plot

# Load ggplot2 package
library(ggplot2)

# Boxplot for Sepal Length
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  ggtitle("Boxplot of Sepal Length by Species") +
  theme_minimal()

# Boxplot for Sepal Width
ggplot(iris, aes(x = Species, y = Sepal.Width, fill = Species)) +
  geom_boxplot() +
  ggtitle("Boxplot of Sepal Width by Species") +
  theme_minimal()

# Boxplot for Petal Length
ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_boxplot() +
  ggtitle("Boxplot of Petal Length by Species") +
  theme_minimal()

# Boxplot for Petal Width
ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  ggtitle("Boxplot of Petal Width by Species") +
  theme_minimal()


#Histogram

# Histogram for Sepal Length
ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
  ggtitle("Histogram of Sepal Length by Species") +
  theme_minimal()

# Histogram for Sepal Width
ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
  ggtitle("Histogram of Sepal Width by Species") +
  theme_minimal()

# Histogram for Petal Length
ggplot(iris, aes(x = Petal.Length, fill = Species)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
  ggtitle("Histogram of Petal Length by Species") +
  theme_minimal()

# Histogram for Petal Width
ggplot(iris, aes(x = Petal.Width, fill = Species)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
  ggtitle("Histogram of Petal Width by Species") +
  theme_minimal()

#Additional plot here i picked a density plot

# Density plot for Sepal Length
ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.6) +
  ggtitle("Density Plot of Sepal Length by Species") +
  theme_minimal()

# Density plot for Petal Length
ggplot(iris, aes(x = Petal.Length, fill = Species)) +
  geom_density(alpha = 0.6) +
  ggtitle("Density Plot of Petal Length by Species") +
  theme_minimal()


#Statistical insights and summary

#Calculate Basic Statistical Metrics (Mean, Median, Standard Deviation)
#we use mean(), median(), and sd() functions to calculate these statistics for each numeric variable.


# Calculate mean, median, and standard deviation for each feature
stats_summary <- data.frame(
  Feature = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  
  Mean = c(mean(iris$Sepal.Length), 
           mean(iris$Sepal.Width), 
           mean(iris$Petal.Length), 
           mean(iris$Petal.Width)),
  
  Median = c(median(iris$Sepal.Length), 
             median(iris$Sepal.Width), 
             median(iris$Petal.Length), 
             median(iris$Petal.Width)),
  
  Std_Dev = c(sd(iris$Sepal.Length), 
              sd(iris$Sepal.Width), 
              sd(iris$Petal.Length), 
              sd(iris$Petal.Width))
)

print(stats_summary)


#sIMPLE CLASSIFICATION MODEL

#Split the data set into training and test sets
#we will use the caTools library to split the dataset into 70% trainng and 30% test sets

# Install and load required packages
install.packages("caTools")
install.packages("class")
install.packages("caret")

library(caTools)
library(class)
library(caret)

# Set a random seed for reproducibility
set.seed(123)

# Split the dataset (70% training, 30% testing)
split <- sample.split(iris$Species, SplitRatio = 0.7)

# Create training and test sets
train_set <- subset(iris, split == TRUE)
test_set <- subset(iris, split == FALSE)

# Create training and test feature matrices (remove the Species column)
train_features <- train_set[, 1:4]
test_features <- test_set[, 1:4]

# Create training and test labels (the Species column)
train_labels <- train_set$Species
test_labels <- test_set$Species



#Implement k-Nearest Neighbors (k-NN) classifier
#we use the knn() function from the class package

# Implement k-NN classifier (choose k = 3 for this example)
k <- 3
knn_predictions <- knn(train = train_features, test = test_features, cl = train_labels, k = k)


#Evaluate model accuracy

# Evaluate the accuracy of the model
accuracy <- mean(knn_predictions == test_labels)
print(paste("Accuracy of the k-NN model:", round(accuracy * 100, 2), "%"))

# Generate and display the confusion matrix
confusion_matrix <- confusionMatrix(knn_predictions, test_labels)
print(confusion_matrix)

library(report)
report
