#For this project we will be running a Polynomial Regression Model
#This is a NON-linear regression model


#Here we are reading in our dataset
#We have 10 observations of three variables
#We have a person's position, level, and salary
salaries = read.csv('positions.csv')


#Here we will remove the Position column since it's redundant with Level
salaries =  salaries[2:3]



#Normally we would split the data into a training and test set
#For this simple exercise we are just going to use all of the data

#First, we will run a linear regression. This is because we want to compare it to the polynomial regression
lin_reg = lm(formula = Salary ~ ., data = salaries)


#Here is a quick summary of our linear regression model
#We have an adjusted R-Squared of .63, which is good
#The Level variable is also deemed statistically significant 
summary(lin_reg)



#Now we are setting up our Polynomial Regression
#First we need to add a column for Level, but squared. Then Level cubed
#So I guess there's not a special polynomial regressor. You set it up as linear, but with squared independent variables
salaries$Level2 = salaries$Level^2
salaries$Level3 = salaries$Level^3
poly_reg = lm(formula = Salary ~ ., data = salaries)


#Here is a summary of our polynomial regression
#All variables are statistically significant, but especially Level3
#Also, the R-Squared has increased to 0.97
summary(poly_reg)



#To further highlight the difference between linear and polynomial regression, we will visualize our results
#First we will import the ggplot2 library 
library(ggplot2)

#Here we will initialize the plot for our linear regression
ggplot() + 
  #Here we are stating that we are plotting points/observed variables with the color red
  geom_point(aes(x = salaries$Level, y = salaries$Salary), color = 'red') +
  #Here we are adding the predictions from the linear regression via a line plot
  #Remember that the salaries must be predicted so we use the predict function via our lin_reg regressor
  #The color of our prediction line will be blue
  geom_line(aes(x = salaries$Level, y = predict(lin_reg, newdata = salaries)), color = 'blue') +
  #Here is simple graph formatting with titles and axes
  ggtitle("Salaries vs. Level (Linear Regression") +
  xlab('Level') +
  ylab('Salary')




#Here we will initialize the plot for our polynomial regression
ggplot() + 
  #Here we are stating that we are plotting points/observed variables with the color red
  geom_point(aes(x = salaries$Level, y = salaries$Salary), color = 'red') +
  #Here we are adding the predictions from the linear regression via a line plot
  #Remember that the salaries must be predicted so we use the predict function via our poly_reg regressor
  #The color of our prediction line will be blue
  geom_line(aes(x = salaries$Level, y = predict(poly_reg, newdata = salaries)), color = 'blue') +
  #Here is simple graph formatting with titles and axes
  ggtitle("Salaries vs. Level (Polynomial Regression") +
  xlab('Level') +
  ylab('Salary')
