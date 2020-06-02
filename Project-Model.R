install.packages("forecast")
library(forecast)
install.packages("fpp")
library(fpp)
install.packages("smooth")
library(smooth) # forsmoothing and MAPE
install.packages("tseries")
library(tseries)
library(readxl)
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
library(plyr)
Train <- read.csv(file.choose())
View(Train)

#Train data preprocessing

summary(Train)
str(Train)
Train <- Train[,c(-1,-2,-4)]
View(Train)

Train_data <- tidyr::separate(Train, Date.Of.Invoice, c("date", "time"), sep = " ")
View(Train_data)

Train_data <- Train_data[which(Train_data$Purchase_price > 0),]

# Plotting time series data
plot(Train_data$Purchase_price) 
#There is no trend or seasonality in the data.

retail <- aggregate(Train_data$Purchase_price~Train_data$date,data=Train_data,FUN="sum")
View(retail)
class(retail$`Train_data$date`) 

retail$`Train_data$date` <- lubridate::mdy(retail$`Train_data$date`)
Daily_Sales <- dplyr::arrange(retail,`Train_data$date`)

colnames(Daily_Sales) <- c("date","Total_price")
# input t
Daily_Sales["t"] <- c(1:73)
View(Daily_Sales)
str(Daily_Sales)

#Daily_Sales$Total_price <- as.numeric(as.character(Daily_Sales$Total_price))
Daily_Sales["log_price"] <- log(Daily_Sales["Total_price"])
Daily_Sales["t_square"] <- Daily_Sales["t"]*Daily_Sales["t"]
View(Daily_Sales)
## Preprocessing completed

Train <- Daily_Sales[1:50,]
Test <- Daily_Sales[51:73,]

attach(Train)
########################### LINEAR MODEL #############################

linear_model <- lm(Train$Total_price ~ Train$t, data = Train)
summary(linear_model)
?predict
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =Test))
rmse_linear <- sqrt(mean((Test$Total_price - linear_pred$fit)^2, na.rm = T))
rmse_linear
#rmse - 6533.562

######################### Exponential #################################

expo_model <- lm(Train$log_price ~ Train$t, data = Train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = Test))
rmse_expo <- sqrt(mean((Test$Total_price - exp(expo_pred$fit))^2, na.rm = T))
rmse_expo
#rmse - 4249.151

######################### Quadratic ####################################

Quad_model <- lm(Train$Total_price ~ Train$t + Train$t_square, data = Train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=Test))
rmse_Quad <- sqrt(mean((Test$Total_price - Quad_pred$fit)^2, na.rm=T))
rmse_Quad
#rmse - 6719.633

#expo_model has lowest rmse value. So we consider it as efficient model.
pred_new <- predict(expo_model, newdata = Test, interval = 'predict')
pred_new <- as.data.frame(exp(pred_new))

plot(expo_model)
acf(expo_model$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot

#A <- auto.arima(expo_model$residuals)
#plot(forecast(A,h=2))
A <- arima(expo_model$residuals, order = c(1,0,0))#AR(1) is significant.
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)

# predicting next 12 months errors using arima( order =c(1,0,0))

library(forecast)
?forecast
errors_12 <- forecast(A, h = 34)

View(errors_12)

future_errors <- data.frame(errors_12)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# predicted values for new data + future error values 

predicted_new_values <- pred_new + future_errors
plot(forecast(A,h=2))
write.csv(predicted_new_values, file = "predicted_new_values.csv", row.names = F)
getwd()

#Test Dataset Preprocessing
Test_data <- read.csv(file.choose())
View(Test_data)

#Test data preprocessing

summary(Test_data)
str(Test_data)
Test_data <- Test_data[,c(-1,-2,-4)]
View(Test_data)

Test_data <- tidyr::separate(Test_data, Date.Of.Invoice, c("date", "time"), sep = " ")
View(Test_data)

Date <- unique(Test_data$date)
Date

Daily_Sales_Test <- as.data.frame(Date)
Daily_Sales_Test
Daily_Sales_Test <- as.data.frame(cbind(Date,Purchase_price = 0))
class(Daily_Sales_Test)

#Apply the expo model on this Daily_Sales_Test data.
pred_new <- predict(expo_model, newdata = Daily_Sales_Test, interval = 'predict')
pred_new <- as.data.frame(exp(pred_new))
library(forecast)
plot(expo_model)
acf(expo_model$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot

#A <- auto.arima(expo_model$residuals)
#plot(forecast(A,h=2))
A <- arima(expo_model$residuals, order = c(1,0,0))#AR(1) is significant.
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)

# predicting next 12 months errors using arima( order =c(1,0,0))

library(forecast)
?forecast
errors_12 <- forecast(A, h = 12)

View(errors_12)

future_errors <- data.frame(errors_12)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# predicted values for new data + future error values 

predicted_new_test_values <- pred_new + future_errors
plot(forecast(A,h=2))
setwd('F:/VANI FOLDER/Data Science/Project')
write.csv(predicted_new_test_values, file = "predicted_new_test_values_sample.csv", row.names = F)
getwd()

