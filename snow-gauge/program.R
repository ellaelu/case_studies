gauge <- read.csv("~/Documents/gauge.txt", sep="") library(ggplot2)
#Investigation: Fitting Linear Model
############## Fitting Gain and Density ##############
#y-axis: density #x-axis: gain
reg <- lm(density ~ gain, data = gauge) reg
#figure 4.1.1 scatterplot of data ggplot(gauge, aes(x=gain, y=density)) +
geom_point() +
  geom_abline(intercept = 0.549, slope = -0.00153, color = "red") + labs(y="Density",
                                                                         x="Gain",
                                                                         title="Linear Regression")
gauge.res = resid(reg) gauge$residuals <- gauge.res gauge$index <- c(1:90)
#figure 4.1.2 residual plot
ggplot(gauge, aes(x=index, y=gauge.res)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") + labs(y="Residual",
                                                   x="Index", title="Residual Plot")
#figure 4.1.3 histogram of residuals ggplot(gauge, aes(gauge.res)) +
geom_histogram(binwidth = 0.1, fill = "mediumturquoise",
               col="white",
               size=0.1) + # change binwidth labs(title="Histogram of Residuals",
  x = "Residuals",
y = "Frequency")
#figure 4.1.4 qqplot of residuals ggplot(gauge, aes(sample = gauge.res)) +
stat_qq() +
  stat_qq_line(color = "red") + labs(title = "Normal Q-Q Plot",
                                     x = "Theoretical Quantiles", y = "Sample Quantiles")
############## Fitting Log(Gain) and Density ##############
#y-axis: density #x-axis: log(gain)
reg.log <- lm(density ~ log(gain), data = gauge) reg.log
#figure 4.1.5 scatterplot of transformed data ggplot(gauge, aes(x=log(gain), y=density)) +
geom_point() +
  geom_abline(intercept = 1.2980, slope = -0.2162, color = "red") + labs(y="Density",
                                                                         x="Log(Gain)",
                                                                         title="Transformed Linear Regression")
gauge.reslog = resid(reg.log) gauge$residuals_log <- gauge.reslog
#figure 4.1.6 transformed residual plot ggplot(gauge, aes(x=index, y=residuals_log)) +
geom_point() +
  geom_hline(yintercept = 0, color = "red") + labs(y="Residual",
                                                   x="Index",
                                                   title="Transformed Residual Plot")
#figure 4.1.7 histogram of transformed residuals ggplot(gauge, aes(residuals_log)) +
geom_histogram(binwidth = 0.01, fill = "mediumturquoise",
               col="white",
               size=0.01) + # change binwidth
  
  labs(title="Histogram of Transformed Residuals", x = "Residuals",
       y = "Frequency")
#figure 4.1.8 qqplot of transformed residuals ggplot(gauge, aes(sample = residuals_log)) +
stat_qq() +
  stat_qq_line(color = "red") + labs(title = "Normal Q-Q Plot",
                                     x = "Theoretical Quantiles", y = "Sample Quantiles")
~~~~Talal~~~~
  gauge <- read.csv("~/gauge.txt", sep="") library(ggplot2)
#excluding density 0.508
test <- gauge[which(gauge$density != 0.508),]
#Mean Squared Error function mse <- function(sm)
mean(sm$residuals^2)
#regular simple linear regression
reg <- lm(density ~ gain, data = gauge) reg
#test simple linear regression
reg_test <- lm(density ~ gain, data = test)
reg_test # (intercept = 0.545383, gain = -0.001519) summary(reg_test)# (R-squared = 0.7986) mse(reg_test) #mse = 0.01045116
ggplot(test, aes(x=gain, y=density)) +
  geom_point() +
  geom_abline(intercept = 0.545383, slope = -0.001519, color = "red") + labs(y="Density",
                                                                             
                                                                             x="Gain",
                                                                             title="Simple Linear Model")
#test log of gain linear regression
log_test <- lm(density ~ log(gain), data = test) log_test # (intercept = 1.2984, gain = -0.2163) summary(log_test)# (R-squared = 0.9955) mse(log_test)# mse = 0.0002333849
ggplot(test, aes(x=log(gain), y=density)) +
  geom_point() +
  geom_abline(intercept = 1.2984, slope = -0.2163, color = "purple") + labs(y="Density",
                                                                            x="log(Gain)",
                                                                            title="Log Linear Model")
#Polynomial regression of higher orders
model2 <- lm(density ~ poly(gain,2, raw=TRUE), data = test) model2 # (intercept = 6.790e-01, gain = 5.603e-06) summary(model2)# (R-squared = 0.9469)
mse(model2)# mse = 0.002718294
#manual test of third order
poly_test <- lm(density ~ gain + I(gain^2) + I(gain^3), data = test) poly_test
mse(summary(poly_test))
model3 <- lm(density ~ poly(gain,3), data = test) model3# (intercept = 0.3090, gain = 0.4241) summary(model3)# (R-squared = 0.9907) mse(summary(model3)# mse = 0.0004700278
#weighted linear regression
weight <- lm(density ~ gain, data = test, weights = 1/gain) weight # (intercept = 0.657966, gain = -0.002243) summary(weight)# (R-squared = 0.7742)

mse(summary(weight))# mse = 0.0001431709
#Plot of Simple linear model and weighted regression model ggplot(test, aes(x=gain, y=density)) +
geom_point() +
  geom_abline(intercept = 0.545383, slope = -0.001519, color = "red") + geom_abline(intercept = 0.657966, slope = -0.002243, color = "blue") + labs(y="Density",
                                                                                                                                                    x="Gain",
                                                                                                                                                    title="Simple Linear Model and Weighted Regression")
#Local Regression
loc_test <- loess(density ~ gain, data = test) loc_test
summary(loc_test)
mse(loc_test) #mse = 0.0001400276
ss.dist <- sum(scale(test$density, scale=FALSE)^2) ss.resid <- sum(resid(loc_test)^2)
1-ss.resid/ss.dist # (R-squared = 0.9973358)
ggplot(test, aes(x=gain, y=density)) + geom_point() +
  geom_smooth(method = "loess", se = FALSE)+ labs(y="Density",
                                                  x="Gain",
                                                  title="Local Regression Model")
#Emma’s Code
##Cross-Validation
data <- read.table("https://math189.edublogs.org/files/2019/03/gauge-1wug38c-2d15noj.txt", header= TRUE)
gain <- data$gain
density <- data$density
#excluding data at 0.508
crossgain <- c(gain[1:20],gain[31:90]) crossdensity <- c(density[1:20],density[31:90]) loggain = log(crossgain)

plot(loggain, crossdensity, xlab = 'Gain', ylab = 'Density', main = 'Scatter Excluding Data at Density 0.508')
data.cross <- data.frame(density = crossdensity, log.gain = loggain)
crossfit <- lm(formula = density~log.gain, data = data.cross)
plot(loggain, crossdensity, xlab = 'Gain', ylab = 'Density', main = 'Scatter Excluding Data at Density 0.508')
abline(crossfit, col="red")
plot(crossfit$residuals, xlab = 'Index', ylab = 'Residuals', main = 'Scatter of Residuals Excluding Data at Density 0.508')
abline(0, 0, col="red")
hist(crossfit$residuals, xlab = 'Residuals', ylab = 'Counts', main = 'Histogram of Residuals Excluding Data at Density 0.508')
qqnorm(crossfit$residuals, main = "QQ Plot", xlab = "Theoretical", ylab = "Sample") qqline(crossfit$residuals, col="red", main = 'Q-Q plot against normal distribution') data.predict <- data.frame(log.gain = log(38.6))
predict(crossfit, newdata = data.predict, interval = 'prediction')
#excluding data at 0.001 data=subset(data,data$density!=0.001) log_gain=log(data$gain)
fit=lm(formula = density~log_gain,data=data) summary(fit)
attach(data)
plot(log_gain,data$density, xlab = "Gain", ylab = "Density", main = "Scatter Excluding Data at Density 0.0010")
abline(fit,col="red")
res=resid(fit)
b <- hist(res, xlab = "Residuals", ylab = "Frequency", main = "Histogram of Residuals Excluding Data at Density 0.0010")
a <- plot(res, xlab = "Index", ylab = "Residuals", main = "Scatter of Residuals Excluding Data at Density 0.0010")
abline(h=0,col="red")
qqnorm(res, main = "QQ Plot", xlab = "Theoretical", ylab = "Sample")
qqline(res, col="red", main = 'Q-Q plot against normal distribution') newdata=data.frame(log_gain=log(426.7))
predict(fit,newdata,interval="prediction")
#Erin
##################### Prediction Intervals ########################
reg.log <- lm(density ~ log(gain), data = gauge) reg.log

pred.int <- predict(reg.log, interval = "prediction") mydata <- cbind(gauge,pred.int)
ggplot(mydata, aes(x=log(gain), y=density)) +
  geom_point() +
  #geom_abline(intercept = 1.2980, slope = -0.2162, color = "red") + stat_smooth(method = lm) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") + geom_line(aes(y = upr), color = "red", linetype = "dashed") + labs(y="Density",
                                                                                                                                   x="Log(Gain)",
                                                                                                                                   title="Prediction Interval of Density given Log(Gain)")