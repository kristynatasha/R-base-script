#Linear Regression

library(readxl)
data <- read_excel("C:/Users/krist/OneDrive/Desktop/Files/Assignments/Praktikum Statdas/Praktikum ke-7/regresilinier.xlsx")

#1, Scatter Plot
# plot(x, y, main="judul", xlab="judul sumbu x", ylab="judul sumbu y")
plot(data$persenkadar, data$arus, main="Persen Kadar ~ Arus", xlab="Persen Kadar", ylab="Arus") 

#2. Linear Model
linearMod <- lm(persenkadar ~ arus, data=data) # build linear regression model on full data 
print(linearMod)

##########

library(stats)
data <- read.delim("C:/Users/krist/OneDrive/Desktop/Files/Analisis Regresi.txt")

#Correlation
cor(data$Pressure, data$Formation_Volume)  # calculate correlation between speed and distance 

#Scatter Plot
scatter.smooth(x=data$Pressure, y=data$Formation_Volume, main="Pressure ~ Formation Volume Factor") # scatterplot 

#Box Plot
par(mfrow=c(1, 2)) # divide graph area in 2 columns 
boxplot(data$Pressure, main="Pressure", sub=paste("Outlier rows: ", boxplot.stats(data$Pressure)$out)) # box plot for 'speed' 
boxplot(data$Formation_Volume, main="Formation Volume Factor", sub=paste("Outlier rows: ", boxplot.stats(data$Formation_Volume)$out))
# box plot for 'distance' 

#Density Plot
library(e1071) 
par(mfrow=c(1, 2)) # divide graph area in 2 columns 
plot(density(data$Pressure), main="Density Plot: Pressure", ylab="Frequency", sub=paste("Skewness:",
                        round(e1071::skewness(data$Pressure), 2))) # density plot for 'speed' 

polygon(density(data$Pressure), col="red") 
plot(density(data$Formation_Volume), main="Density Plot: Formation Volume Factor", ylab="Frequency",
                        sub=paste("Skewness:", round(e1071::skewness(data$Formation_Volume), 2))) # density plot for 'dist' 
polygon(density(data$Formation_Volume), col="red") 

#Linear Model
linearMod <- lm(Pressure ~ Formation_Volume, data=data) # build linear regression model on full data 
print(linearMod)

summary(linearMod) # model summary 


#t-statistics and p-values
(modelSummary <- summary(linearMod)) # capture model summary as an object 
(modelCoeffs <- modelSummary$coefficients) # model coefficients 
(beta.estimate <- modelCoeffs["Formation_Volume", "Estimate"]) # get beta estimate for speed 
(std.error <- modelCoeffs["Formation_Volume", "Std. Error"]) # get std.error for speed 
(t_value <- beta.estimate/std.error) # calc t statistic 
(p_value <- 2*pt(-abs(t_value), df=nrow(data)-ncol(data))) # calc p Value 
(f_statistic <- linearMod$fstatistic[1]) # fstatistic 
(f <- summary(linearMod)$fstatistic) # parameters for model p-value calc 
(model_p <- pf(f[1], f[2], f[3], lower=FALSE))

#AIC BIC
(AIC(linearMod))
(BIC(linearMod))


#########################Asumsi Klasik######################
# Assume that we are fitting a multiple linear regression
# on the MTCARS data
linearMod <- lm(Pressure ~ Formation_Volume, data = data)


# Normality of Residuals
# qq plot for studentized resid
library(CircStats)
qqPlot(linearMod , main="QQ Plot")

# distribution of studentized residuals
library(MASS)
sresid <- studres(linearMod)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(linearMod)
# plot studentized residuals vs. fitted values
spreadLevelPlot(linearMod)

# Evaluate Collinearity
vif(linearMod) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot
crPlots(linearMod)
# Ceres plots
ceresPlots(linearMod)

# Test for Autocorrelated Errors
durbinWatsonTest(linearMod)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(linearMod)
summary(gvmodel)
