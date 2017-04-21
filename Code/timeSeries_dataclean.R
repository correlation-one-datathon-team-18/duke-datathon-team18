library(forecast)
setwd('/Users/ADITYA/Desktop/Datathon/git-repo/duke-datathon-team18/')
data <- read.csv('econ_state.csv')
data_2 <- read.csv('econ_federal.csv')
real_estate <- read.csv('real_estate.csv')


timeSeries <- function(datatype){
  if(datatype == 'Unemployement'){
    myts <- ts(data_2$unemployment_rate, start=c(1954, 8), end=c(2017, 01), frequency=12) 
    fit <- HoltWinters(myts)
    fit.forecast <- forecast(fit$x, 10)
  }
  if(datatype == 'cpi'){
    myts <- ts(data_2$cpi_index, start=c(1954, 8), end=c(2017, 01), frequency=12) 
    fit <- HoltWinters(myts)
    fit.forecast <- forecast(fit$x, 10)
  }
  if(datatype == '10 year'){
    myts <- ts(data_2$X10y_rate, start=c(1954, 8), end=c(2017, 01), frequency=12) 
    fit <- HoltWinters(myts)
    fit.forecast <- forecast(fit$x, 10)
  }
  if(datatype == 'nonfarm payroll'){
    myts <- ts(data_2$nonfarm_payrolls, start=c(1954, 8), end=c(2017, 01), frequency=12) 
    fit <- HoltWinters(myts)
    fit.forecast <- forecast(fit$x, 10)
  }
  if(datatype == 'Fed Funds'){
    myts <- ts(data_2$fed_funds_rate, start=c(1954, 8), end=c(2017, 01), frequency=12) 
    fit <- HoltWinters(myts)
    fit.forecast <- forecast(fit$x, 10)
  }
  
  plot(fit.forecast, main = paste('Time Series Forcasting - ', datatype))
  
}

cityData <- read.csv('NY_predict.csv', header = TRUE)
datacity <- function(city){
  df1 <- data.frame(t(data.frame(cityData$real_2005[which(cityData$city==city)], cityData$real_2006[which(cityData$city==city)], cityData$real_2007[which(cityData$city==city)], cityData$real_2008[which(cityData$city==city)], cityData$real_2010[which(cityData$city==city)], cityData$real_2011[which(cityData$city==city)], cityData$real_2012[which(cityData$city==city)], cityData$real_2013[which(cityData$city==city)], cityData$real_2014[which(cityData$city==city)], cityData$real_2015[which(cityData$city==city)], cityData$real_2016[which(cityData$city==city)])), 
                    t(data.frame(cityData$gdp_2005[which(cityData$city==city)], cityData$gdp_2006[which(cityData$city==city)], cityData$gdp_2007[which(cityData$city==city)], cityData$gdp_2008[which(cityData$city==city)], cityData$gdp_2010[which(cityData$city==city)], cityData$gdp_2011[which(cityData$city==city)], cityData$gdp_2012[which(cityData$city==city)], cityData$gdp_2013[which(cityData$city==city)], cityData$gdp_2014[which(cityData$city==city)], cityData$gdp_2015[which(cityData$city==city)], cityData$gdp_2016[which(cityData$city==city)])), 
                    t(data.frame(cityData$per_capita_2005[which(cityData$city==city)], cityData$per_capita_2006[which(cityData$city==city)], cityData$per_capita_2007[which(cityData$city==city)], cityData$per_capita_2008[which(cityData$city==city)], cityData$per_capita_2010[which(cityData$city==city)], cityData$per_capita_2011[which(cityData$city==city)], cityData$per_capita_2012[which(cityData$city==city)], cityData$per_capita_2013[which(cityData$city==city)], cityData$per_capita_2014[which(cityData$city==city)], cityData$per_capita_2015[which(cityData$city==city)], cityData$per_capita_2016[which(cityData$city==city)])),
                    t(data.frame(cityData$ur_2005[which(cityData$city==city)], cityData$ur_2006[which(cityData$city==city)], cityData$ur_2007[which(cityData$city==city)], cityData$ur_2008[which(cityData$city==city)], cityData$ur_2010[which(cityData$city==city)], cityData$ur_2011[which(cityData$city==city)], cityData$ur_2012[which(cityData$city==city)], cityData$ur_2013[which(cityData$city==city)], cityData$ur_2014[which(cityData$city==city)], cityData$ur_2015[which(cityData$city==city)], cityData$ur_2016[which(cityData$city==city)])),
                    t(data.frame(cityData$er_2005[which(cityData$city==city)], cityData$er_2006[which(cityData$city==city)], cityData$er_2007[which(cityData$city==city)], cityData$er_2008[which(cityData$city==city)], cityData$er_2010[which(cityData$city==city)], cityData$er_2011[which(cityData$city==city)], cityData$er_2012[which(cityData$city==city)], cityData$er_2013[which(cityData$city==city)], cityData$er_2014[which(cityData$city==city)], cityData$er_2015[which(cityData$city==city)], cityData$er_2016[which(cityData$city==city)])))
  names(df1) <- c('Real_Estate', 'GDP', 'Per_Capita', 'Unemployement', 'Employement')
  write.csv(df1, file = paste(city, 'csv', sep = '.'), row.names = TRUE)
}

datastate <- function(state){
  df <- data.frame(real_estate$city[which(real_estate$state == state)], real_estate$X2005[which(real_estate$state == state)], real_estate$X2006[which(real_estate$state == state)], real_estate$X2007[which(real_estate$state == state)], real_estate$X2008[which(real_estate$state == state)], real_estate$X2009[which(real_estate$state == state)], real_estate$X2010[which(real_estate$state == state)], real_estate$X2011[which(real_estate$state == state)], real_estate$X2012[which(real_estate$state == state)], real_estate$X2013[which(real_estate$state == state)], real_estate$X2014[which(real_estate$state == state)], real_estate$X2015[which(real_estate$state == state)], real_estate$X2016[which(real_estate$state == state)], data$X2005_gdp[which(data$state==state)], data$X2006_gdp[which(data$state==state)], data$X2007_gdp[which(data$state==state)], data$X2008_gdp[which(data$state==state)], data$X2009_gdp[which(data$state==state)], data$X2010_gdp[which(data$state==state)], data$X2011_gdp[which(data$state==state)], data$X2012_gdp[which(data$state==state)], data$X2013_gdp[which(data$state==state)], data$X2014_gdp[which(data$state==state)], data$X2015_gdp[which(data$state==state)], data$X2016_gdp[which(data$state==state)], data$X2005_per_capita[which(data$state==state)], data$X2006_per_capita[which(data$state==state)], data$X2007_per_capita[which(data$state==state)], data$X2008_per_capita[which(data$state==state)], data$X2009_per_capita[which(data$state==state)], data$X2010_per_capita[which(data$state==state)], data$X2011_per_capita[which(data$state==state)], data$X2012_per_capita[which(data$state==state)], data$X2013_per_capita[which(data$state==state)], data$X2014_per_capita[which(data$state==state)], data$X2015_per_capita[which(data$state==state)], data$X2016_per_capita[which(data$state==state)], data$X2005_ur[which(data$state==state)], data$X2006_ur[which(data$state==state)], data$X2007_ur[which(data$state==state)], data$X2008_ur[which(data$state==state)], data$X2009_ur[which(data$state==state)], data$X2010_ur[which(data$state==state)], data$X2011_ur[which(data$state==state)], data$X2012_ur[which(data$state==state)], data$X2013_ur[which(data$state==state)], data$X2014_ur[which(data$state==state)], data$X2015_ur[which(data$state==state)], data$X2016_ur[which(data$state==state)], 100 - data$X2005_ur[which(data$state==state)], 100 - data$X2006_ur[which(data$state==state)], 100 - data$X2007_ur[which(data$state==state)], 100 - data$X2008_ur[which(data$state==state)], 100 - data$X2009_ur[which(data$state==state)], 100 - data$X2010_ur[which(data$state==state)], 100 - data$X2011_ur[which(data$state==state)], 100 - data$X2012_ur[which(data$state==state)], 100 - data$X2013_ur[which(data$state==state)], 100 - data$X2014_ur[which(data$state==state)], 100 - data$X2015_ur[which(data$state==state)], 100 - data$X2016_ur[which(data$state==state)])
  
  names(df) <- c('city', 
                 'real_2005','real_2006', 'real_2007', 'real_2008', 'real_2009', 'real_2010', 'real_2011', 'real_2012', 'real_2013', 'real_2014', 'real_2015', 'real_2016', 
                 'gdp_2005', 'gdp_2006', 'gdp_2007', 'gdp_2008', 'gdp_2009', 'gdp_2010', 'gdp_2011', 'gdp_2012', 'gdp_2013', 'gdp_2014', 'gdp_2015', 'gdp_2016',
                 'per_capita_2005', 'per_capita_2006', 'per_capita_2007', 'per_capita_2008', 'per_capita_2009', 'per_capita_2010', 'per_capita_2011', 'per_capita_2012', 'per_capita_2013', 'per_capita_2014', 'per_capita_2015', 'per_capita_2016',
                 'ur_2005', 'ur_2006', 'ur_2007', 'ur_2008', 'ur_2009', 'ur_2010', 'ur_2011', 'ur_2012', 'ur_2013', 'ur_2014', 'ur_2015', 'ur_2016',
                 'er_2005', 'er_2006', 'er_2007', 'er_2008', 'er_2009', 'er_2010', 'er_2011', 'er_2012', 'er_2013', 'er_2014', 'er_2015', 'er_2016')
  
  write.csv(df, file = "NY_predict.csv", row.names = TRUE)  
  
}


for (i in df$real_estate.city.which.real_estate.state.....NY...){
  datacity(i)
}

timeSeries('Unemployement')
