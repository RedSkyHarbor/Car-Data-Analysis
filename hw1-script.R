mydata <- read.csv("data_akbilgic.csv")

#Problem 1
ISE_data <- c(mean(mydata$ISE), median(mydata$ISE), mode(mydata$ISE), sd(mydata$ISE), var(mydata$ISE))
SP_data <- c(mean(mydata$SP), median(mydata$SP), mode(mydata$SP), sd(mydata$SP), var(mydata$SP))
DAX_data <- c(mean(mydata$DAX), median(mydata$DAX), mode(mydata$DAX), sd(mydata$DAX), var(mydata$DAX))
FTSE_data <- c(mean(mydata$FTSE), median(mydata$FTSE), mode(mydata$FTSE), sd(mydata$FTSE), var(mydata$FTSE))
NIKKEI_data <- c(mean(mydata$NIKKEI), median(mydata$NIKKEI), mode(mydata$NIKKEI), sd(mydata$NIKKEI), var(mydata$NIKKEI))
BOVESPA_data <- c(mean(mydata$BOVESPA), median(mydata$BOVESPA), mode(mydata$BOVESPA), sd(mydata$BOVESPA), var(mydata$BOVESPA))
EU_data <- c(mean(mydata$EU), median(mydata$EU), mode(mydata$EU), sd(mydata$EU), var(mydata$EU))
EM_data <- c(mean(mydata$EM), median(mydata$EM), mode(mydata$EM), sd(mydata$EM), var(mydata$EM))

#Commented prints for cleaner output
cat(ISE_data)
cat(SP_data)
cat(DAX_data)
cat(FTSE_data)
cat(NIKKEI_data)
cat(BOVESPA_data)
cat(EU_data)
cat(EM_data)

#Problem 2
#Draw box plot of each stock and calculate outliers
boxplot(mydata[1], main="ISE Stocks")
boxplot(mydata[2], main="SP Stocks")
boxplot(mydata[3], main="DAX Stocks")
boxplot(mydata[4], main="FTSE Stocks")
boxplot(mydata[5], main="NIKKEI Stocks")
boxplot(mydata[6], main="BOVESPA Stocks")
boxplot(mydata[7], main="EU Stocks")
boxplot(mydata[8], main="EM Stocks")

# outlier
quantile(mydata$ISE)

summary(mydata$ISE)
iqr = quantile(mydata$ISE, .75) - quantile(mydata$ISE, .25)
print(iqr)

upper_outlier <- quantile(mydata$ISE, .75) + (iqr * 1.5)
lower_outlier <- quantile(mydata$ISE, .25) - (iqr * 1.5)

print(upper_outlier)
print(lower_outlier)

outliers <- mydata[mydata[1] > upper_outlier | mydata[1] < lower_outlier]
print(outliers)


#Problem 3
#Q-Q plot between ISE and EM
ISE_quartiles = c(quantile(mydata$ISE))
EM_quartiles = c(quantile(mydata$EM))
qqplot(ISE_quartiles, EM_quartiles, xlab="ISE stock exchange national 100 index", ylab="MCSI emerging markeys index", main="Q-Q PLOT")

#Problem 4
#Show correlation between two columns
cor(mydata[1], mydata[8])

#Problem 5
#Check for correlation between brazil and japan.
cor(mydata[5], mydata[6])
