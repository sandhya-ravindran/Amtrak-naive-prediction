
# Title

# Installing Packages

install.packages('forecast')
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('ggfortify')

# Calling library

library('forecast')
library('ggplot2')
library('ggthemes')
library('ggfortify')

# Read 

Amtrak.data <- read.csv("C:/Users/Sandhya Ravindran/Desktop/r_wd/Amtrak data.csv")

# Converting to time series

ridership.ts <- ts(Amtrak.data$Ridership, 
                   start = c(1991, 1), end = c(2004, 3), freq = 12)

# Plot ts

autoplot(ridership.ts, ts.colour = '#2676ba', ts.linetype ='dashed' ) 

# 3 years later

ridership.ts.3yrs <- window(ridership.ts, start = c(1997, 1), end = c(1999, 12))

# Fit linear regression to ts

ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2))

# Plot ts for next three years

autoplot(ridership.ts.3yrs,ts.linetype = 'dashed', ts.colour= '#cd661d')

# Variables

nValid <- 36
nTrain <- length(ridership.ts) - nValid

# partition the data

train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))

#  naive and seasonal naive forecasts

naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)

# plot

autoplot(train.ts, ts.linetype= 'dashed', ts.colour = '#008000')

autoplot(train.ts, ts.colour = '#868f98', xlab="time",ylab = "Ridership",xlim = c(1991,2006.25))+autolayer(naive.pred$mean)+autolayer(snaive.pred$mean)

accuracy(naive.pred, valid.ts)
accuracy(snaive.pred, valid.ts)


