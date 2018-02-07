# this is for price challenge in kaggle
path <- "C:/Users/c_ymelpati/Desktop/data science/data/kaggle/pricechallange"
setwd(path)


tr <- read.csv("train.tsv",sep="\t")
nrow(tr)
ncol(tr)

# as the rows are very large 
# we will split the data using index method
ind <- sample(2, nrow(tr), replace = TRUE, prob=c(0.5, 0.5))
head(ind)
# to train the data
train_data <- tr[ind == 1,]
# to cross check the model
test_data <- tr[ind == 2,]
head(train_data)
nrow(train_data)
ncol(train_data)
# the data is shuffled and split into half

# lets try knn with k = 3
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)
# required libraries
library(class)
library(caret)
require(mlbench)
library(e1071)
library(base)
require(base)
  
  
X_train <- subset(train_data, select=-price) # we remove class column for train
y_train <- train_data$price
# for test data
X_test <- subset(test_data, select=-price) # exclude Class for prediction
y_test <- test_data$price

# performing the model for k = 3
model_knn_3 <- knn(train=train_data,
                 test=train_data,
                 cl=train_data$price,  # class labels
                 k=3)
  
  
  
  