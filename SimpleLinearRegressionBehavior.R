
# File:           SimpleLinearRegressionBehavior.R
# Purpose:        Monitor The Behavior Of Linear Regression 
# Author:         MMS, mamdouh.al.shamy@gmail.com
# Product:        Practicing Machine Learning


# Suppose you have a dataset (0,0), (1,1), (2,2), (3,3)
# And want to get linear equation in a form H(x) = Theta * x that could best satisfy the dataset
# And J(Theta) = (1/(2*m)) Sigma( ( H(x) - y )^2) is the function that trys to tune Theta so that
# H(x) fits the dataset as close as possible, while stepping 

xData = 0:3 # x values of given dataset
yData = 0:3 # y values of given dataset
m = length(xData) # size of dataset
rangeOfTheta = seq(0,3,0.2) # starting from 0 till 3 with step 0.2
colorPalete = c('red', 'green', 'blue', "dark magenta")

# Plotting All lines of Hypotheses
plotHypotheses = function(){
for( x in seq(xData[1],length(xData))){
  for(theta in rangeOfTheta){
    abline(0, x*theta, col=colorPalete[x])
    }
  }
}

# Getting values of J(Theta)
JofTheta = function(rangeOfTheta){
  JTheta = vector()
  i = 1
  for(theta in rangeOfTheta )
  {
    y1 = ( (xData[1] * theta) - yData[1] )^2
    y2 = ( (xData[2] * theta) - yData[2] )^2
    y3 = ( (xData[3] * theta) - yData[3] )^2
    y4 = ( (xData[4] * theta) - yData[4] )^2
    yT =  y1 + y2 + y3 + y4
    JTheta[i] = yT/(2*m)
    i =i+1
  }
  return (JTheta)
}

plot(xData,yData, main = "Given Data Points vs Hypotheses", xlab = "X, Theta", ylab="Y, J(Theta)") # Plotting given dataset 

plotHypotheses() # Plotting All lines of Hypotheses

points(rangeOfTheta, JofTheta(rangeOfTheta), pch=16) # Plotting The values of J(Theta)
