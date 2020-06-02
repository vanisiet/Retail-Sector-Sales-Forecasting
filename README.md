# Retail-Sector-Sales-Forecasting
The project is to predict the daily sales of Retail sector

In this project, first we are preparing train the data (given by client) by removing the time factor from date column. 
We are considering only the records with purchase_price >0. 
Then we create a dataframe by aggregating the purchase_price for each day. 
Then we split the dataframe as test and train dataset. 
Apply different models to train dataset and find the exponential model to be best since it has least RMSE value. 
Then we predict for the test dataframe. 
Then apply ARIMA model and predict the sales including the residual errors.
The same model we then apply to the test dataset given by the client and predict the sales by using ARIMA model.
