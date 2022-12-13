library(dplyr)     #data wrangling
library(ggplot2)   #plotting
library(earth)     #fitting MARS models
library(caret)     #tuning model parameters
#导入数据#install.packages('earth')

data=read.csv('insurance.csv')
x=data[,1:6]
y=data$charges
index= createDataPartition(y, p = 0.75, list = FALSE)
x_train = data[index,1:6]
x_test = data[-index,1:6]
y_train = data[index,7]
y_test = data[-index,7]

hyper_grid <- expand.grid(degree = 1:4, nprune = seq(2, 20, length.out = 16) %>%floor())
set.seed(1)
cv_mars <- train(x_train,y_train,method = "earth",metric = "RMSE",trControl = trainControl(method = "cv", number = 10),tuneGrid = hyper_grid)
cv_mars$results %>%filter(nprune == cv_mars$bestTune$nprune, degree == cv_mars$bestTune$degree)
y_pred <- predict(cv_mars, newdata=x_test)
RMSE=sqrt(sum((y_pred - y_test) ^ 2)/length(y_test))
rss <- sum((y_pred - y_test) ^ 2)  ## residual sum of squares
tss <- sum((y_test - mean(y_test)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
ggplot(cv_mars)
#使用 k 折叠交叉验证拟合 MARS 模型
model<- train(x_train,y_train,method = "earth",metric = "RMSE",trControl = trainControl(method = "cv", number = 10),tuneGrid =hyper_grid)

#返回最好的参数情况
model$results %>%filter(nprune == cv_mars$bestTune$nprune, degree == cv_mars$bestTune$degree)
y_pred <- predict(model, newdata=x_test)
RMSE=sqrt(sum((y_pred - y_test) ^ 2)/length(y_test))
rss <- sum((y_pred - y_test) ^ 2)  ## residual sum of squares
tss <- sum((y_test - mean(y_test)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
ggplot(cv_mars)
