# this is for price challenge in kaggle with reference to datacamp
path <- "C:/Users/c_ymelpati/Desktop/data science/data/kaggle/pricechallange"
setwd(path)

#loading the data
tr <- read.csv("train.tsv",sep="\t")
nrow(tr)
ncol(tr)

# as the rows are very large 
# we will split the data using index method
ind <- sample(2, nrow(tr), replace = TRUE, prob=c(0.2, 0.8))
head(ind)

# to train the data
train_data <- tr[ind == 1,]
head(train_data,0)
simple_linear <- lm(price~shipping+item_condition_id,data = train_data)
simple_linear
summary(simple_linear)
plot(simple_linear)

# as the above model is ready we will predict

train_data$pred <- predict(simple_linear,data = train_data)
head(train_data$pred)
library(ggplot2)

ggplot(train_data , aes(x = price , y = pred))+geom_point()+geom_abline(color = "blue")

options(scipen = 999)
par(mfrow=c(1,2))
plot(train_data$train_id,train_data$price)
plot(train_data$train_id,train_data$pred)
# the above model is only with item_condition_id and shipping factor values
