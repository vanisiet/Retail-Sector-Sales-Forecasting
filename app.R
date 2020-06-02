
library(shiny)
library(DT)

options(shiny.maxRequestSize = 30*1024^2)
#Train <- read.csv("F:/VANI FOLDER/Data Science/Project/appdir/data/Train.csv")
ui <- fluidPage(
  titlePanel("Retail Sales Forecast"),
  sidebarLayout(
    sidebarPanel(
      fileInput("Train","Choose Train Dataset",multiple=FALSE,
                accept=c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),
      tags$hr(),
      checkboxInput("header","Header",TRUE),
 #     basicPage( actionButton("go", "Predict-Train")),
      
      fileInput("Test_data","Choose Test Dataset",multiple=FALSE,
                accept=c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header_Test","Header",TRUE),
      basicPage( actionButton("go", "Predict")),
    ),
    
    mainPanel(
      DTOutput("Predict")
   )
  )
)

server <- function(input,output)
{
  observeEvent(input$go, {
    output$Predict <- renderDT({
      #Predicting Train dataset
      Train_data <- input$Train
      if(is.null(Train_data))
        return(NULL)
      
      Train_data <- read.csv(Train_data$datapath,header=input$header)
      Train_data <- Train_data[,c(-1,-2,-4)]
      Train_data <- tidyr::separate(Train_data, Date.Of.Invoice, c("date", "time"), sep = " ")
      
      Train_data <- Train_data[which(Train_data$Purchase_price > 0),]
      retail <- aggregate(Train_data$Purchase_price~Train_data$date,data=Train_data,FUN="sum")
      
      retail$`Train_data$date` <- lubridate::mdy(retail$`Train_data$date`)
      Daily_Sales <- dplyr::arrange(retail,`Train_data$date`)
      
      colnames(Daily_Sales) <- c("date","Total_price")
      Daily_Sales["t"] <- c(1:73)
      Daily_Sales["log_price"] <- log(Daily_Sales["Total_price"])
      Daily_Sales["t_square"] <- Daily_Sales["t"]*Daily_Sales["t"]
      Daily_Sales
      ## Preprocessing completed
      
      Train <- Daily_Sales[1:50,]
      Test <- Daily_Sales[51:73,]
      Train
      
      expo_model <- lm(Train$log_price ~ Train$t, data = Train)
      summary(expo_model)
      expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = Test))
      rmse_expo <- sqrt(mean((Test$Total_price - exp(expo_pred$fit))^2, na.rm = T))
      rmse_expo
      #rmse - 4249.151
      
      #expo_model has lowest rmse value. So we consider it as efficient model.
      pred_new <- predict(expo_model, newdata = Test, interval = 'predict')
      pred_new <- as.data.frame(exp(pred_new))
      pred_new
      
      acf(expo_model$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot
      
      A <- arima(expo_model$residuals, order = c(1,0,0))#AR(1) is significant.
      A$residuals
      
      ARerrors <- A$residuals
      
      acf(ARerrors, lag.max = 10)
      
      # predicting next 12 months errors using arima( order =c(1,0,0))
      library(forecast)
      
      errors_12 <- forecast(A, h = 50)
      
      future_errors <- data.frame(errors_12)
      future_errors
      future_errors <- future_errors$Point.Forecast
      
      predicted_new_values <- pred_new + future_errors
      
      predicted_new_values
      
      ###################################################################
      #Based on the model training by Train dataset, we predict for Test dataset
      Test_data <- input$Test_data
      if(is.null(Test_data))
        return(NULL)
      
      Test_data <- read.csv(Test_data$datapath,header=input$header)
      Test_data <- Test_data[,c(-1,-2,-4)]
      Test_data <- tidyr::separate(Test_data, Date.Of.Invoice, c("date", "time"), sep = " ")
      Date <- unique(Test_data$date)

      Daily_Sales_Test <- as.data.frame(Date)
      
      Daily_Sales_Test <- as.data.frame(cbind(Date,Purchase_price = 0))
      Daily_Sales_Test
      
      pred_new1 <- predict(expo_model, newdata = Daily_Sales_Test, interval = 'predict')
      pred_new1 <- as.data.frame(exp(pred_new1))
      pred_new1
      
      acf(expo_model$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot
      
      A <- arima(expo_model$residuals, order = c(1,0,0))#AR(1) is significant.
      A$residuals
      
      ARerrors <- A$residuals
      
      acf(ARerrors, lag.max = 10)
      
      # predicting next 12 months errors using arima( order =c(1,0,0))
      
      errors_12 <- forecast(A, h = 34)
      
      future_errors_test <- data.frame(errors_12)
      future_errors_test <- future_errors_test$Point.Forecast
      
      # predicted values for new data + future error values 
      
      predicted_new_test_values <- pred_new1 + future_errors_test

    })
    
  })

 
}

shinyApp(ui=ui,server=server)