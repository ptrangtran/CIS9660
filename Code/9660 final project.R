# install.packages(c("readr","stringr","corrplot", "ggplot2", "texreg", "class", "caret", "rpart", "rpart.plot", "knitr", "factoextra"))
library(readr); library(stringr); library(corrplot); library(ggplot2); library(texreg); library(class); 
library(caret); library(rpart); library(rpart.plot); library(knitr); library(factoextra)

data <- read.csv("adult_train.csv", header = T, na.strings = "?") 
names(data)

#cleaning data
data <- data[, !(names(data) %in% c("capital.gain", "capital.loss", "X"))] 

for (col in names(data)) {   
  data[[col]][str_trim(data[[col]]) == "?"] <- NA 
} 
data <- na.omit(data) 

# fix(data) 
# head(data,10)
# summary(data)

data$workclass <- as.factor(data$workclass) 
data$education <- as.factor(data$education)
data$marital.status <- as.factor(data$marital.status) 
data$occupation <- as.factor(data$occupation) 
data$relationship <- as.factor(data$relationship) 
data$race <- as.factor(data$race) 
data$sex <- as.factor(data$sex) 
data$native.country <- as.factor(data$native.country) 
data$Income <- as.factor(data$Income) 

# creating workclass.num
data$workclass.num <- as.numeric(data$workclass)
table(data$workclass); table(data$workclass.num)

#creating marital.status.num
data$marital.status.num <- as.numeric(data$marital.status)
table(data$marital.status); table(data$marital.status.num)

# creating occupation.num
data$occupation.num <- as.numeric(data$occupation)
table(data$occupation); table(data$occupation.num)

# creating relationship.num
data$relationship.num <- as.numeric(data$relationship)
table(data$relationship); table(data$relationship.num)

# creating race.num
data$race.num <- as.numeric(data$race)
table(data$race); table(data$race.num)

# creating sex.num
data$sex.num <- as.numeric(data$sex)
table(data$sex); table(data$sex.num)

# creating native.country.num
data$native.country.num <- as.numeric(data$native.country)
table(data$native.country); table(data$native.country.num)

# creating Income.num
levels(data$Income) <- trimws(levels(data$Income)) 
data$Income.num <- ifelse(data$Income == ">50K", 1, 0) 
table(data$Income); table(data$Income.num)

attach(data)
names(data)

# summary
summary(data)
numbers <- data[c("age", "workclass.num", "education.num", "marital.status.num", "occupation.num", 
               "relationship.num", "race.num", "sex.num", "native.country.num", "Income.num")]

cor(numbers,Income.num)
corr_matrix = cor(numbers) 

hist(age)
hist(workclass.num)
hist(education.num)
hist(marital.status.num)
hist(occupation.num)
hist(relationship.num)
hist(race.num)
hist(sex.num)
hist(native.country.num)
hist(Income.num)


# variable summary plots
ggplot(data, aes(x = age, fill = Income)) + geom_density(alpha = 0.8) + 
  scale_fill_manual(values=c("#ace8d1","#385b64")) + ggtitle("Age distribution") + theme_minimal()
# the older generation tends to have a higher income than the young

ggplot(data, aes(x = workclass, fill = Income)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values=c("#ace8d1","#385b64")) + ggtitle("Workclass distribution") + theme_minimal()
# the majority of the data is in a private workclass, with a disparity within private

ggplot(data = data, aes(x = Income, y = education.num, fill = Income)) + geom_boxplot() + 
  scale_fill_manual(values=c("#ace8d1","#385b64")) +  ggtitle("Education distribution") + theme_minimal() 
# people with a higher education level typically earn above 50 thousand a year

ggplot(data, aes(x = sex, fill = Income)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values=c("#ace8d1","#385b64")) + ggtitle("Sex distribution") + theme_minimal()
# males seem to have a higher >50k ratio


# logistic regression
# model 1
glm.fits=glm(Income~age+race+sex,data=data,family=binomial) 
summary(glm.fits)

# for each one-unit increase in age, the log odds of having a incomer higher than 50k increases by 0.0419, while holding the variable constant 
# exp(0.041907) = 1.042, each one-year increase in age, odds having greater income than 50k increase by 4.2% 
# coefficent of Asian, Pac, Islander individuals have 2.71 times higher odds of having income greater than 50K compare to other categories 
# male have 3.36 times higher odds of having income <50K compare to females 
# race Asian-Pac-Islander p-value is very small, indicates variable is statistically significant in predicting income grater than 50K 
# p-values for race Black and Other is not statistically significant of the predictors where income is greater than 50K

glm.probs = predict(glm.fits, type = "response")
glm.pred = rep("0", 30162)
glm.pred[glm.probs > 0.5] = "1"
table(glm.pred, Income)

# accuracy rate ((681 + 21480) / 30162)
mean(glm.pred == Income.num)
# 0.7347324

# precision rate: true positive / (true positive + false positive)
681/(681+1174)
# 0.3671159

# recall rate: true positive / (true positive + false negative)
681/(681+6827)
# 0.09070325


# training data
set.seed(2) 
indices = sample(1:nrow(data), 0.75 *nrow(data)) 
train=data[indices, ]
test =data[-indices, ] 

glm.fits.train=glm(Income~age+race+sex,data=train,family=binomial) 
summary(glm.fits.train)

glm.probs=predict(glm.fits.train, type = "response") 
glm.pred=rep("0", nrow(data)) 
glm.pred[glm.probs > 0.5] = "1" 
table(glm.pred, Income.num)

# accuracy rate: (true positive + true negative) / total
(476 + 21244) / 30162
mean(glm.pred == Income.num)
# 0.7201114
print(paste("Accuracy:", round((476 + 21244) / 30162 * 100, 2), "%"))

# precision rate: true positive / (true positive + false positive)
476/(476+1410)
# 0.252386

# recall rate: true positive / (true positive + false negative)
476/(476+7032)
# 0.06339904

# model comparison
screenreg(list(glm.fits, glm.fits.train), 
          custom.model.names = c("Untrained Model", "Trained Model"),
          digits = 3,
          stars = c(0.05, 0.01, 0.001),
          single.row = TRUE
)


# KNN
# Splitting the dataset into training and testing sets
# used createDatePartition() to split dataset into 75% training and 25$ testing subsets
set.seed(123)
indices <- createDataPartition(data$Income.num, p = 0.75, list = FALSE)
train <- data[indices, ]
test <- data[-indices, ]
# Selecting features for KNN and the target variable
# age, hours.per.week, education.num as the predictors and income.num as target variable
train_features <- train[, c("age", "hours.per.week", "education.num")]
test_features <- test[, c("age", "hours.per.week", "education.num")]

#seperated target cariable income.num for both training and testing 
train_labels <- train$Income.num
test_labels <- test$Income.num

head(train_features)
head(test_features)
#picked education num bc known thaat higher education attainment leads to higher paying jobs
#picked age bc its often said that more experience and career progession could affect income
#picked hours per week bc how much people invest in their work could correlate with their income level

# Normalize the feature sets
# have to notmalize to scale the values of each feature between 0 and 1, so all features can contribut equally to the distance calculation using knn
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

train_normalized <- as.data.frame(lapply(train_features, normalize))
test_normalized <- as.data.frame(lapply(test_features, normalize))

head(train_normalized)
head(test_normalized)

#setting k to 5 so it will check for 5 nearest neighbors when making predictions
k <- 5

#knn() to train the model on normalized training set and predict on the normlized testing set
knn_pred <- knn(train_normalized, test_normalized, train_labels, k)
confusionMatrix <- table(Predicted = knn_pred, Actual = test_labels)
print(confusionMatrix)
#True Negatives (5131): Predicted income <= 50K and actually <= 50K.
#True Positives (762): Predicted income > 50K and actually > 50K.
#False Negatives (1136): Predicted income <= 50K, but actually > 50K.
#False Positives (511): Predicted income > 50K, but actually <= 50K.
#need to compare pedicted vs actual labels in test set & calculate accurance based on correct predicted samples over total number of samples

accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

#so 78.16% is the overall % of correct predictions in the test set

# insights
# Theres an imbalance in the dataset, the model seems to struggle more with >50k, a real life reason could be that fewer people earn high incomes compared to those who earn less, which based on the summary with <=50K:22654 & >50K : 7508
# to support my knn model, here is a dataplot to see how income is distributed between my features, as you can see, higher education is a strong predictor and for age, it seems pretty distrubuted.
# education.num, age and income.num
ggplot(data, aes(x = age, y = education.num, color = factor(Income.num))) +
  geom_point(alpha = 0.5) +
  labs(color = "Income Level") +
  scale_color_manual(values=c("#ace8d1","#385b64")) +
  theme_minimal()

#hours per week and age
ggplot(data, aes(x =hours.per.week , y = age, color = factor(Income.num))) +
  geom_point(alpha = 0.5) +
  labs(color = "Income Level") +
  scale_color_manual(values=c("#ace8d1","#385b64")) +
  theme_minimal()

#education level and hpw
ggplot(data, aes(x = education.num, y = hours.per.week, color = factor(Income.num))) +
  geom_point(alpha = 0.5) +
  labs(color = "Income Level") +
  scale_color_manual(values=c("#ace8d1","#385b64")) +
  theme_minimal()


# for hours per week, we see both income levels vary, but typically people work 40 hours a week so by the plot it seems like people who work less than 25 hours per week often make less than 50k since we see a bit of clustering around 
head(data)


#Tree Method
#fit decision tree model with adjusted parameters
tree_model <- rpart(Income.num ~ age + workclass + education + marital.status + occupation + relationship + hours.per.week + native.country + race + sex, data = train, method = "class")

#summary tree model
summary(tree_model)

#plot tree model
rpart.plot(tree_model)

#predicted income 
predicted_income <- predict(tree_model, type = "class")

#count the number of observations for each predicted income 
prediction_counts <- table(predicted_income)


# kmean clustering
set.seed(123)
sampled_data <- data
sampled_data <- sampled_data[, !grepl("^education\\.num$", names(sampled_data))]
sampled_data <- sampled_data[, !grepl("^Income$", names(sampled_data))]

# perform k-mean
numeric_data <- model.matrix(~.-1, data = sampled_data)
kmeans_result <- kmeans(numeric_data, centers = 20)
pca_result <- prcomp(numeric_data)

column_names <- names(sampled_data)

# plot
cluster_colors <- rainbow(n = length(unique(kmeans_result$cluster)))

pc1_range <- range(pca_result$x[, 1])
pc2_range <- range(pca_result$x[, 2])

pc1_median <- median(pca_result$x[, 1])
pc2_median <- median(pca_result$x[, 2])

plot(pca_result$x[,1], pca_result$x[,2], col = cluster_colors[kmeans_result$cluster], pch = 19,
     main = "K-Means Clustering (PCA)", xlab = "PC1", ylab = "PC2")

legend("topleft", legend = column_names, col = 1:length(column_names),
       pch = 19, cex = .75, xjust = 1.5, yjust = 1.5  )

#line across 0
abline(h = pc2_median, col = "red", lty = 1) 
abline(v = pc1_median, col = "red", lty = 1)

#variance of each PC
variance <- summary(pca_result)$importance["Proportion of Variance", ]
print(variance)

#find top variables for each PC
loadings_pc1 <- pca_result$rotation[, 1]  
loadings_pc2 <- pca_result$rotation[, 2]  
loadings_pc3 <- pca_result$rotation[, 3]  

top_pc1 <- names(sort(abs(loadings_pc1), decreasing = TRUE)[1:10])  
top_pc2 <- names(sort(abs(loadings_pc2), decreasing = TRUE)[1:10])  
top_pc3 <- names(sort(abs(loadings_pc3), decreasing = TRUE)[1:10])  

cat("PC1:", top_pc1, "\n")
cat("PC2:", top_pc2, "\n")


# pca
project_data <- data[,13:20]
data_normalized <- scale(project_data)
kable(head(data_normalized), format = "markdown")
corr_matrix <- cor(data_normalized)
corrplot(corr_matrix, method = "color")

data.pca <- princomp(corr_matrix)
summary(data.pca)
fviz_cos2(data.pca, choice = "var", axes = 1:2, fill = "#5f8a88", col = "#385b64")
