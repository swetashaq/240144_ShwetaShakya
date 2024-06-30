#Libraries used in the assignment
#Installing necessary libraries

install.packages('matlib')
library(matlib)

install.packages("ggplot2")
library(ggplot2)

install.packages("rsample")
library(rsample)

#Import data(gene_data.csv)
gene_data = (read.csv(file = "C:/Users/Asus/Desktop/assignment/gene_data1.csv", header = T))

#Convert gene_data to Matrix
Xdata = as.matrix(gene_data)

# Set Column names of Xdata to X, X1, X2, X3, X4, X5
colnames(Xdata) = c("X", "X1", "X2", "X3", "X4", "X5")

#Task 1: Preliminary data analysis
#--------------------------------------------------------------------------------

##Time series plots of Gene Expression)
GeneExp = ts(Xdata) #ts function create time series object
plot(GeneExp, main = "Time Series Plot of Each Gene", xlab = "time", ylab ="Expression")

##Distribution for each gene (time-series)
#Histogram and Density Plot of Xdata
par(mfrow=c(1,2)) #plots 2x2=4 plots in same display
#Density plot of Gene Expression Levels
plot(density(Xdata), type = "l", xlab = "Expression Level", main = "Density curve of All Genes")
#Creating histogram of Xdata 
hist(Xdata, freq = FALSE, xlab = "Expression Level", main = "Histogram  and Density curve of All Genes") #plot Histogram of Xdata
#Plot density in histogram
lines(density(Xdata), col = "black", lwd = 1) #Plot density


par(mfrow=c(2,2)) #plots 2x2=4 plots in same display

#Histogram and Density Plot of X1 Gene Expression
#Creating histogram of X1 Signal
hist(Xdata[,"X1"], freq = FALSE, xlab = "X1", main = "Histogram and density curve of X1 Gene")
#Plot density in histogram
lines(density(Xdata[,"X1"]),col = "brown", lwd = 1, xlab = "X1")

#Histogram and Density Plot of X2 Gene Expression
#Creating histogram of X2 Signal
hist(Xdata[,"X2"], freq = FALSE, xlab = "X2", main = "Histogram and density curve of X2 Gene")
#Plot density in histogram
lines(density(Xdata[,"X2"]), col = "brown", lwd = 1, xlab = "X2")

#Histogram and Density Plot of X3 Gene Expression
#Creating histogram of X3 Signal
hist(Xdata[,"X3"], freq = FALSE, xlab = "X3", main = "Histogram and density curve of X3 Gene")
#Plot density in histogram
lines(density(Xdata[,"X3"]),col = "brown", lwd = 1, xlab = "X3")

#Histogram and Density Plot of X4 Gene Expression
#Creating histogram of X4 Signal
hist(Xdata[,"X4"], freq = FALSE, xlab = "X4", main = "Histogram and density curve of X4 Gene")
#Plot density in histogram
lines(density(Xdata[,"X4"]),col = "brown", lwd = 1, xlab = "X4")

#Histogram and Density Plot of X5 Gene Expression
#Creating histogram of X5 Signal
hist(Xdata[,"X5"], freq = FALSE, xlab = "X5", main = "Histogram and density curve of X5 Gene")
#Plot density in histogram
lines(density(Xdata[,"X5"]),col = "brown", lwd = 1, xlab = "X5")

##Correlation and scatter plots (between different combination of two genes) to examine their dependencies
# to arrange multiple plots in a single screen
par(mfrow=c(2,2)) #plots 2x2=4 plots in same display
#Creating Plot

#Correlation between X1 and X2
plot(Xdata[,"X1"], Xdata[,"X2"], pch = 1, col="blue", main = "Correlation between X1 and X2", xlab = "X1", ylab = "X2")

#Correlation between X1 and X3
plot(Xdata[,"X1"], Xdata[,"X3"], pch = 1, col="blue", main = "Correlation between X1 and X3", xlab = "X1", ylab = "X3")

#Correlation between X1 and X4
plot(Xdata[,"X1"], Xdata[,"X4"], pch = 1, col="blue", main = "Correlation between X1 and X4", xlab = "X1", ylab = "X4")

#Correlation between X1 and X5
plot(Xdata[,"X1"], Xdata[,"X5"], pch = 1, col="blue", main = "Correlation between X1 and X5", xlab = "X1", ylab = "X5")

#Correlation between X2 and X1
plot(Xdata[,"X2"], Xdata[,"X1"], pch = 1, col="blue", main = "Correlation between X2 and X1", xlab = "X2", ylab = "X1")

#Correlation between X2 and X3
plot(Xdata[,"X2"], Xdata[,"X3"], pch = 1, col="blue", main = "Correlation between X2 and X3", xlab = "X2", ylab = "X3")

#Correlation between X2 and X4
plot(Xdata[,"X2"], Xdata[,"X4"], pch = 1, col="blue", main = "Correlation between X2 and X4", xlab = "X2", ylab = "X4")

#Correlation between X2 and X5
plot(Xdata[,"X2"], Xdata[,"X5"], pch = 1, col="blue", main = "Correlation between X2 and X5", xlab = "X2", ylab = "X5")

#Correlation between X3 and X1
plot(Xdata[,"X3"], Xdata[,"X1"], pch = 1, col="blue", main = "Correlation between X3 and X1", xlab = "X3", ylab = "X1")

#Correlation between X3 and X2
plot(Xdata[,"X3"], Xdata[,"X2"], pch = 1, col="blue", main = "Correlation between X3 and X2", xlab = "X3", ylab = "X2")

#Correlation between X3 and X4
plot(Xdata[,"X3"], Xdata[,"X4"], pch = 1, col="blue", main = "Correlation between X3 and X4", xlab = "X3", ylab = "X4")

#Correlation between X3 and X5
plot(Xdata[,"X3"], Xdata[,"X5"], pch = 1, col="blue", main = "Correlation between X3 and X5", xlab = "X3", ylab = "X5")

#Correlation between X4 and X1
plot(Xdata[,"X4"], Xdata[,"X1"], pch = 1, col="blue", main = "Correlation between X4 and X1", xlab = "X4", ylab = "X1")

#Correlation between X4 and X2
plot(Xdata[,"X4"], Xdata[,"X2"], pch = 1, col="blue", main = "Correlation between X4 and X2", xlab = "X4", ylab = "X2")

#Correlation between X4 and X3
plot(Xdata[,"X4"], Xdata[,"X3"], pch = 1, col="blue", main = "Correlation between X4 and X3", xlab = "X4", ylab = "X3")

#Correlation between X4 and X5
plot(Xdata[,"X4"], Xdata[,"X5"], pch = 1, col="blue", main = "Correlation between X4 and X5", xlab = "X4", ylab = "X5")

#Correlation between X5 and X1
plot(Xdata[,"X5"], Xdata[,"X1"], pch = 1, col="blue", main = "Correlation between X5 and X1", xlab = "X5", ylab = "X1")

#Correlation between X5 and X2
plot(Xdata[,"X5"], Xdata[,"X2"], pch = 1, col="blue", main = "Correlation between X5 and X2", xlab = "X5", ylab = "X2")

#Correlation between X5 and X3
plot(Xdata[,"X5"], Xdata[,"X3"], pch = 1, col="blue", main = "Correlation between X5 and X3", xlab = "X5", ylab = "X3")

#Correlation between X5 and X4
plot(Xdata[,"X5"], Xdata[,"X4"], pch = 1, col="blue", main = "Correlation between X5 and X4", xlab = "X5", ylab = "X4")

#Task 2: Regression – modelling the relationship between gene expression

# Calculating ones for binding the data
onesMatrix = matrix(1 , length(Xdata)/6,1) # Creating matrix of ones
onesMatrix

# Creating Data for model 1 from given equation
X_model1 = cbind(onesMatrix, Xdata[,"X4"], Xdata[,"X3"]^2)
X_model1

# Creating Data for model 2 from given equation
X_model2 = cbind(onesMatrix, Xdata[,"X4"], Xdata[,"X3"]^2, Xdata[,"X5"])
X_model2

# Creating Data for model 3 from given equation
X_model3 = cbind(onesMatrix, Xdata[,"X3"], Xdata[,"X4"], Xdata[,"X5"]^3)
X_model3

# Creating Data for model 4 from given equation
X_model4 = cbind(onesMatrix, Xdata[,"X4"], Xdata[,"X3"]^2, Xdata[,"X5"]^3)
X_model4

# Creating Data for model 5 from given equation
X_model5 = cbind(onesMatrix, Xdata[,"X4"], Xdata[,"X1"]^2, Xdata[,"X3"]^2)
X_model5

x2 = Xdata[, "X2"]
x2 = matrix(x2, ncol=1)
# Print x2 to see the extracted column
print(x2)

#Task 2.1: 
#--------------------------------------------------------------------------------
#Calculation the least square (thetaHat)
#for model1
model1_Thetahat = solve(t(X_model1) %*% X_model1) %*% t(X_model1) %*% x2
model1_Thetahat

#for model2
model2_Thetahat = solve(t(X_model2) %*% X_model2) %*% t(X_model2) %*% x2
model2_Thetahat

#for model3
model3_Thetahat = solve(t(X_model3) %*% X_model3) %*% t(X_model3) %*% x2
model3_Thetahat

#for model4
model4_Thetahat = solve(t(X_model4) %*% X_model4) %*% t(X_model4) %*% x2
model4_Thetahat

#for model5
model5_Thetahat = solve(t(X_model5) %*% X_model5) %*% t(X_model5) %*% x2
model5_Thetahat


#-------------------------------------------------------------------------------- 
#Task 2.2: Model Residual Error
#--------------------------------------------------------------------------------
# Calculating y-hat
# Model 1
model1_YHat = X_model1 %*% model1_Thetahat
model1_YHat

# Model 2
model2_YHat = X_model2 %*% model2_Thetahat
model2_YHat

# Model 3
model3_YHat = X_model3 %*% model3_Thetahat
model3_YHat

# Model 4
model4_YHat = X_model4 %*% model4_Thetahat
model4_YHat

# Model 5
model5_YHat = X_model5 %*% model5_Thetahat
model5_YHat

# Calculating RSS
# Model 1
RSS_model1 = sum((Y-model1_YHat)^2)
RSS_model1

# Model 2
RSS_model2 = sum((Y-model2_YHat)^2)
RSS_model2

# Model 3
RSS_model3 = sum((Y-model3_YHat)^2)
RSS_model3

# Model 4
RSS_model4 = sum((Y-model4_YHat)^2)
RSS_model4

# Model 5
RSS_model5 = sum((Y-model5_YHat)^2)
RSS_model5


#-------------------------------------------------------------------------------- 
#Task 2.3: Calculating Likelihood and Variance for all models
#--------------------------------------------------------------------------------
n = length(x2) #Calculating length of Y
# Calculating Variance for
# Model 1
VAR_model1 = RSS_model1/(n-1)
VAR_model1

# Model 2
VAR_model2 = RSS_model2/(n-1)
VAR_model2

# Model 3
VAR_model3 = RSS_model3/(n-1)
VAR_model3

# Model 4
VAR_model4 = RSS_model4/(n-1)
VAR_model4

# Model 5
VAR_model5 = RSS_model5/(n-1)
VAR_model5

# Calculating likelihood for
# Model 1
Likelihood_model1 = -(n/2)*(log(2*pi))-(n/2)*(log(VAR_model1))-(1/(2*VAR_model1))*RSS_model1
Likelihood_model1

# Model 2
Likelihood_model2 = -(n/2)*(log(2*pi))-(n/2)*(log(VAR_model2))-(1/(2*VAR_model2))*RSS_model2
Likelihood_model2

# Model 3
Likelihood_model3 = -(n/2)*(log(2*pi))-(n/2)*(log(VAR_model3))-(1/(2*VAR_model3))*RSS_model3
Likelihood_model3

# Model 4
Likelihood_model4 = -(n/2)*(log(2*pi))-(n/2)*(log(VAR_model4))-(1/(2*VAR_model4))*RSS_model4
Likelihood_model4

# Model 5
Likelihood_model5 = -(n/2)*(log(2*pi))-(n/2)*(log(VAR_model5))-(1/(2*VAR_model5))*RSS_model5
Likelihood_model5


#-------------------------------------------------------------------------------- 
#Task 2.4: Calculating AIC and BIC for all models
#--------------------------------------------------------------------------------
# Calculating AIC
# Model 1
AIC_model1 = 2*(length(model1_Thetahat))-2*Likelihood_model1
AIC_model1

# Model 2
AIC_model2 = 2*(length(model2_Thetahat))-2*Likelihood_model2
AIC_model2

# Model 3
AIC_model3 = 2*(length(model3_Thetahat))-2*Likelihood_model3
AIC_model3

# Model 4
AIC_model4 = 2*(length(model4_Thetahat))-2*Likelihood_model4
AIC_model4

# Model 5
AIC_model5 = 2*(length(model1_Thetahat))-2*Likelihood_model5
AIC_model5

# Calculating BIC
# Model 1
BIC_model1 = length(model1_Thetahat)*log(n)-2*Likelihood_model1
BIC_model1

# Model 2
BIC_model2 = length(model2_Thetahat)*log(n)-2*Likelihood_model2
BIC_model2

# Model 3
BIC_model3 = length(model3_Thetahat)*log(n)-2*Likelihood_model3
BIC_model3

# Model 4
BIC_model4 = length(model4_Thetahat)*log(n)-2*Likelihood_model4
BIC_model4

# Model 5
BIC_model5 = length(model5_Thetahat)*log(n)-2*Likelihood_model5
BIC_model5


#-------------------------------------------------------------------------------- 
#Task 2.5: Calculating Error for all models and Plotting Q-Q plot with Q-Q line for them
#--------------------------------------------------------------------------------

par(mfrow = c(3, 2))
# Model 1
Error_model1 = x2 - model1_YHat #Error
qqnorm(Error_model1, col = "#336600", main = "Q-Q plot of Model 1") # Plots Graph
qqline(Error_model1, col = "#e60000", lwd = 1) # Adds Q-Q line on graph

# Model 2
Error_model2 = x2 - model2_YHat #Error
qqnorm(Error_model2, col = "#336600", main = "Q-Q plot of Model 2") # Plots Graph
qqline(Error_model2, col = "#e60000", lwd = 1) # Adds Q-Q line on graph

# Model 3
Error_model3 = x2 - model3_YHat #Error
qqnorm(Error_model3, col = "#336600", main = "Q-Q plot of Model 3") # Plots Graph
qqline(Error_model3, col = "#e60000", lwd = 1) # Adds Q-Q line on graph

# Model 4
Error_model4 = x2 - model4_YHat #Error
qqnorm(Error_model4, col = "#336600", main = "Q-Q plot of Model 4") # Plots Graph
qqline(Error_model4, col = "#e60000", lwd = 1) # Adds Q-Q line on graph

# Model 5
Error_model5 = x2 - model5_YHat #Error
qqnorm(Error_model5, col = "#336600", main = "Q-Q plot of Model 5") # Plots Graph
qqline(Error_model5, col = "#e60000", lwd = 1) # Adds Q-Q line on graph


#-------------------------------------------------------------------------------- 
#Task 2.6: Selecting a model
#--------------------------------------------------------------------------------
# Calculating Mean Squared Error (MSE)
# Model 1
MSE_model1 = sum(Error_model1^2)/length(Error_model1)
MSE_model1

# Model 2
MSE_model2 = sum(Error_model2^2)/length(Error_model2)
MSE_model2

# Model 3
MSE_model3 = sum(Error_model3^2)/length(Error_model3)
MSE_model3

# Model 4
MSE_model4 = sum(Error_model4^2)/length(Error_model4)
MSE_model4

# Model 5
MSE_model5 = sum(Error_model5^2)/length(Error_model5)
MSE_model5


#-------------------------------------------------------------------------------- 
#Task 2.7: Splitting input and output data sets into training and testing sets in ratio of 7:3
#--------------------------------------------------------------------------------

# Splitting the data (Training Data set)
XSplit = initial_split(data = as.data.frame(Xdata), prop = .7)
YSplit = initial_split(data = as.data.frame(x2), prop = .7)

# Training Data set for output x2
Y_Training_Set = training(YSplit) # Y Training data set
Y_Training_Data = as.matrix(Y_Training_Set) # Y Training data
Y_Training_Data

# Training Data set for input gene expressions
X_Training_Set = training(XSplit) # X Training data set
X_Training_Data = as.matrix(X_Training_Set)# X Training data
X_Training_Data

# Testing Data for output gene x2
Y_Testing_Set = testing(YSplit) # Y Testing data set
Y_Testing_Data = as.matrix(Y_Testing_Set) # Y Testing data
Y_Testing_Data

# Testing Data for input gene expressions
X_Testing_Set = testing(XSplit) # X Testing data set
X_Testing_Data = as.matrix(X_Testing_Set) # X Testing data
X_Testing_Data

TestingOneXMatrix = matrix(1, nrow(X_Testing_Data), 1) # 73 rows of ones

# Step 3: Recreate the model matrix for the testing set
X_Testing_Model = cbind(TestingOneXMatrix, X_Testing_Data[, "X4"], (X_Testing_Data[, "X1"])^2, (X_Testing_Data[, "X3"])^2)

# Verify the dimensions of the model matrix
cat("Dimensions of X_Testing_Model:", dim(X_Testing_Model), "\n")


# Selecting Model 5, estimating model parameters using training data
TrainingOneXMatrix = matrix(1, length(X_Training_Set$X1), 1) # ones matrix for training set
TrainingXModel = cbind(TrainingOneXMatrix, X_Training_Set[, "X4"], (X_Training_Set[, "X1"])^2, (X_Training_Set[, "X3"])^2) #Training Model
TrainingXModel
Y_Training_Data <- as.matrix(Y_Training_Data)cat("Dimensions of X_Testing_Data:", dim(X_Testing_Data), "\n") 

TrainingThetaHat = solve(t(TrainingXModel) %*% TrainingXModel) %*% t(TrainingXModel) %*% Y_Training_Data
TrainingThetaHat <- as.matrix(TrainingThetaHat, ncol = 1)
TrainingThetaHat


# Computing output/prediction of Model5 using testing data set
X_Testing_Model
cat("Dimensions of X_Testing_Data:", dim(X_Testing_Model), "\n")  # Should print (170, 4)
TestingYHat = X_Testing_Model %*% TrainingThetaHat
TestingYHat
RSStesting = sum((Y_Testing_Set - TestingYHat)^2)
RSStesting

#used t-test as sample size is less than 30 i.e 4 and varriance is know (calculated)
t.test(Y_Training_Data, mu = 500, alternative = "two.sided", conf.level = 0.95)
C_I1 = -0.06331197
C_I2 = 0.51992124
meu = 0.2283046

# With 95% of confidence interval, predicting the model and plotting them with testing data and error bars
par(mfrow = c(1, 1))
TrainingDensity = density(Y_Training_Data) # Density of training data of output gene
TrainingDensity
plot(TrainingDensity, col="#336600", lwd = 2, main="Distribution of Output Gene")
abline(v = C_I1, col = "#e60000", lty=2)
abline(v = C_I2, col = "#e60000", lty=2)
abline(v = meu, col = "#1a1a1a", lty=2)

residual = ((Y_Testing_Set - TestingYHat)) # Calculating Error
residual

# plotting Error Bars
# Calculating Standard Deviation (Sigma)
Sigma = sqrt(VAR_model5) #Variance of model5 from task 2.3
Sigma
X_model5 #Data model 5 from task 2.1

dataFrame = data.frame(
  xAxis = X_model5,
  yAxis = x2
)
dataFrame

ggplot(dataFrame) +
  geom_bar( aes(x=xAxis.1, y=x2), stat="identity", fill="#336600", alpha=0.7) +
  geom_errorbar( aes(x=xAxis.1, ymin=x2-Sigma, ymax=x2+Sigma), width=0.4, colour="#e60000", alpha=0.9, linewidth=1) +
  labs(title="Error Bar (Model 5 - X1)", x="Model 5 - X1", y = "Output Gene")

ggplot(dataFrame) +
  geom_bar( aes(x=xAxis.2, y=x2), stat="identity", fill="#336600", alpha=0.7) +
  geom_errorbar( aes(x=xAxis.2, ymin=x2-Sigma, ymax=x2+Sigma), width=0.4, colour="#e60000", alpha=0.9, linewidth=1) +
  labs(title="Error Bar (Model 5 - X2)", x="Model 5 - X2", y = "Output Gene")

ggplot(dataFrame) +
  geom_bar( aes(x=xAxis.3, y=x2), stat="identity", fill="#336600", alpha=0.7) +
  geom_errorbar( aes(x=xAxis.3, ymin=x2-Sigma, ymax=x2+Sigma), width=0.4, colour="#e60000", alpha=0.9, linewidth=1) +
  labs(title="Error Bar (Model 5 - X3)", x="Model 5 - X3", y = "Output Gene")

ggplot(dataFrame) +
  geom_bar( aes(x=xAxis.4, y=x2), stat="identity", fill="#336600", alpha=0.7) +
  geom_errorbar( aes(x=xAxis.4, ymin=x2-Sigma, ymax=x2+Sigma), width=0.4, colour="#e60000", alpha=0.9, linewidth=1) +
  labs(title="Error Bar (Model 5 - X4)", x="Model 5 - X4", y = "Output Gene")

ggplot(dataFrame) +
  geom_bar( aes(x=xAxis.4, y=x2), stat="identity", fill="#336600", alpha=0.7) +
  geom_errorbar( aes(x=xAxis.4, ymin=x2-Sigma, ymax=x2+Sigma), width=0.4, colour="#e60000", alpha=0.9, linewidth=1) +
  labs(title="Error Bar (Model 5 - X5)", x="Model 5 - X5", y = "Output Gene")


#-------------------------------------------------------------------------------- 
#Task 3: Approximate Bayesian Computation (ABC)
#--------------------------------------------------------------------------------
array1 = 0
array2 = 0
f_value = 0
s_value = 0

# Model 5 thetahat values from Task 2.1
ThetaBias = 1.63372350 # chosen parameter
ThetaA = 1.12262816 # chosen parameter
ThetaB = 0.55442704 # set constant
ThetaC = 0.06344803 # set constant
Epsilon = RSS_model5 * 2 ## fixing value of epsilon, RSS_model5 from task 2.2
num = 100 # number of iteration
##Calculating Y-hat for performing rejection ABC
counter <- 0
for (i in 1:num) {
  range1 = runif(1, -1.63372350, 1.63372350) # calculating the range
  range1
  range2 = runif(1, -1.12262816, 1.12262816)
  range2
  NewThetahat = matrix(c(range1, range2, ThetaB, ThetaC))
  NewYHat = X_model5 %*% NewThetahat ## New Y hat and model2_Thetahat from task2.1
  NewRSS = sum((x2 - NewYHat)^2)
  NewRSS
  if (NewRSS > Epsilon){ #Performing rejection ABC
    array1[i] = range1
    array2[i] = range2
    counter = counter + 1
    Fvalue = matrix(array1)
    Svalue = matrix(array2)
  }
}
# Plotting the graph
plot(Fvalue, Svalue, col = c("#336600", "#3366ff"), main = "Joint and Marginal Posterior Distribution Model 2")