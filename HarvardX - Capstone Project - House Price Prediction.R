#title: "Harvard Capstone Project - House Price Prediction"
#author: "Leandro Rodrigues Carvalho"
#date: "07/03/2021"
  
#Missing packages are automatically installed with external link

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

#Loading packages

library(tidyverse)
library(caret)
library(data.table)
library(randomForest)
library(gbm)
library(rpart)
library(knitr)

#Retrieving train and test sets

df <- read.csv("/Users/leandrocarvalho/Documents/Education/HarvardX - Data Science/HarvardX9 - Capstone Project/HarvardX9 - House Price Prediction/house-prices-advanced-regression-techniques/train.csv")

row.names(df) <- df$Id
df <- df[,-1]
df[is.na(df)] <- 0
for(i in colnames(df[,sapply(df, is.character)])){
  df[,i] <- as.factor(df[,i])
}

#Test values
test.n <- sample(1:nrow(df), nrow(df)/3, replace = F)

#Test dataset
test <- df[test.n,]

#Train dataset
train <- df[-test.n,]

rm(test.n, df)

#Combined Data Frame
combined.df <- rbind(test, train)

#Method and Analyses

#Exploring multiple statistical methods

#Understanding of the data set
glimpse(combined.df)

#Checking for duplicates

#Property ID
sum(duplicated(combined.df))
cat("The number of duplicated rows are", nrow(combined.df) - nrow(unique(combined.df)))

#Missing Values

#The percentage of data missing in train
sum(is.na(train)) / (nrow(train) *ncol(train))

#The percentage of data missing in test
sum(is.na(test)) / (nrow(test) * ncol(test))

#The percentage of data missing in combined data set
sum(is.na(combined.df)) / (nrow(combined.df) * ncol(combined.df))

#Summary and Distribution of the variable "SalePrice"
summary(combined.df$SalePrice)

options(scipen=20000)
ggplot(combined.df, aes(x = SalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 3000) +
  ggtitle("Distribution of Sales Price") +
  ylab("Houses") +
  xlab("Price") + 
  theme(plot.title = element_text(hjust = 0.7))

#Log term of SalePrice
combined.df$lSalePrice <- log(combined.df$SalePrice)
test$lSalePrice <- log(test$SalePrice)
train$lSalePrice <- log(train$SalePrice)

#Distribution of log SalePrice
ggplot(combined.df, aes(x = lSalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 0.03) +
  ggtitle("SalePrice (log) Distribution") +
  ylab("Houses") +
  xlab("Price") + 
  theme(plot.title = element_text(hjust = 0.3))

#Visualizing Sales Price in relation to Neighborhood
p <- ggplot(combined.df, aes(x = lSalePrice, fill = Neighborhood))
p + stat_bin()

#Different perspectives
q <- ggplot(combined.df, aes(x = lSalePrice, y = Neighborhood, color = YearBuilt))
q + geom_point()

sales_lot <- ggplot(combined.df, aes(x = lSalePrice, y = LotArea, color = Neighborhood))
sales_lot + geom_point()

#Log transformed x axis
sales_lot2 <- ggplot(combined.df, aes(LotArea, y = 1, color = Neighborhood))
sales_lot2 + geom_jitter() + 
coord_trans(x = "log10")

#Ground Living Area filled with Sale Condition
combined.df %>%
  ggplot(aes(x = GrLivArea, fill = SaleCondition)) +
geom_bar(position = "fill") +
ylab("proportion")

#Residual Mean Squared Error (RMSE)
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

#Linear Regression
model <- lm(lSalePrice ~ GrLivArea, data = train)
predict <- predict(model, test)

#RMSE
RMSE0 <- RMSE(predict, test$lSalePrice)
RMSE0 <- round(RMSE0, digits = 3)
rmse_project_results <- data_frame(Method = "Linear Regression", RMSE = RMSE0)
rmse_project_results %>% knitr::kable()

#Regression Trees
model <- rpart(lSalePrice ~., data = train, method = "anova")
predict <- predict(model, test)
RMSE1 <- RMSE(predict, test$lSalePrice)
RMSE1 <- round(RMSE1, digits = 3)
rmse_project_results <- bind_rows(rmse_project_results, data_frame(Method = "Regression Tree", RMSE = RMSE1))
rmse_project_results%>% knitr::kable()

#Random Forest
model <- randomForest(lSalePrice ~., data = train, method = "anova",
                      ntree = 300,
                      mtry = 26,
                      replace = F,
                      nodesize = 1,
                      importance = T)

#Visualizing the number of trees
plot(model)

predict <- predict(model, test)
RMSE2 <- RMSE(predict, test$lSalePrice)
RMSE2 <- round(RMSE2, digits = 3)
rmse_project_results <- bind_rows(rmse_project_results, data_frame(Method = "Random Forest", RMSE = RMSE2))
rmse_project_results %>% knitr::kable()

#Gradient Boosting Machine
model <- gbm(lSalePrice ~., data = train, distribution = "laplace",
             shrinkage = 0.05,
             interaction.depth = 5,
             bag.fraction = 0.66,
             n.minobsinnode = 1,
             cv.folds = 100,
             keep.data = F,
             verbose = F,
             n.trees = 300)

predict <- predict(model, test, n.trees = 300)

RMSE3 <- RMSE(predict, test$lSalePrice)
RMSE3 <- round(RMSE3, digits = 3)
rmse_project_results <- bind_rows(rmse_project_results, data_frame(Method = "Gradient Boosting Machine", RMSE = RMSE3))
rmse_project_results %>% knitr::kable()

#Results and Conclusion

#RMSE Project Results
rmse_project_results %>% knitr::kable()
