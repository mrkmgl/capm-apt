library(psych)
library(lmtest)
library(quantreg)
library(HH)
library(corrplot)
library(olsrr)

#EXERCISE: CAPM ESTIMATION

#(1)
#loading the interested dataset
dataset=read.csv("/Users/marco/Desktop/Education/Skema/Semester 1/Econometrics/Group Project/capm.csv")
View(dataset)

#Modifying the dataset with the unique columns we need
#just for making it more clear
cnames = c("Date", "SANDP", "FORD", "USTB3M")
data_sort = dataset[cnames]
View(data_sort)

par(mfrow=c(2,1))
plot(data_sort$FORD, type = "l", ylab = "Ford")
plot(data_sort$SANDP, type = "l", ylab = "SP&500") 

#(2)
#We need returns not prices
ford = dataset["FORD"]
sandp = dataset["SANDP"]
ustb3m = dataset["USTB3M"]

n = nrow(ford) #calculating the number of rows

#Calculating the returns
ford_ret = log(ford[2:n, 1]/ford[1:(n-1), 1])
sandp_ret = log(sandp[2:n, 1]/sandp[1:(n-1), 1])
ustb3m_ret = log(ustb3m[2:n, 1]/ustb3m[1:(n-1), 1])

#Creating the dataset of returns to visualize the new table
dataset_ret = data.frame(dataset[2:nrow(dataset),1], ford_ret, sandp_ret, ustb3m_ret)
View(dataset_ret)

par(mfrow=c(2,1))
plot(ford_ret, type = "l", ylab = "Ford Returns")
plot(sandp_ret, type = "l", ylab = "SP&500 Returns") 

#(3)
#CAPM regression, excess return and explanations
market_premium = sandp_ret - ustb3m_ret #initializing the variable
ford_premium = ford_ret - ustb3m_ret #initializing the variable

ford_regr = lm(ford_premium~market_premium)
summary(ford_regr) #alpha = -0.001055, beta = 0.963681
par(mfrow=c(2,2))
plot(ford_regr) #normal errors, no anomalies in data
bptest(ford_regr) #we cannot reject, no problem of heteroskedasticity


#EXERCISE: APT ESTIMATION
#(1)
#Importing the dataset and visualizing the data available
apt_dataset=read.csv("/Users/marco/Desktop/Education/Skema/Semester 1/Econometrics/Group Project/macro.csv")
View(apt_dataset)

#(2)
#Calculating returns and variations of variables
m = nrow(apt_dataset)
stock_ret = NULL #I add
for (i in c(2:3)) #just for stock returns, ln(x/x-1)
{
  stock_logret = log((apt_dataset[2:m, i])/(apt_dataset[1:(m-1), i]))
  stock_ret = as.data.frame(cbind(stock_ret,stock_logret)) #I add
}

macro_ret = NULL
for (i in c(4:5)) #just for macro variations, simple variation
{
  macro_var = ((apt_dataset[2:m, i])/(apt_dataset[1:(m-1), i])-1)
  macro_ret = as.data.frame(cbind(macro_ret,macro_var)) #I add
}

bond_ret = NULL 
for (i in c(6:11)) #just for bond returns, ln(x/x-1)
{
  bond_logret = log((apt_dataset[2:m, i])/(apt_dataset[1:(m-1), i]))
  bond_ret = as.data.frame(cbind(bond_ret,bond_logret)) #I add
}

macro1_ret = NULL 
for (i in c(12:14)) #just for macro variations, simple variation
{
  macro1_var = ((apt_dataset[2:m, i])/(apt_dataset[1:(m-1), i])-1)
  macro1_ret = as.data.frame(cbind(macro1_ret,macro1_var)) #I add
}

apt_ready = cbind(apt_dataset[2:m,1],stock_ret, macro_ret, bond_ret, macro1_ret)
names(apt_ready)[1] = "Date"
names(apt_ready)[2] = "microsoft_ret"
names(apt_ready)[3] = "sp500_ret"
names(apt_ready)[4] = "CPI_var"
names(apt_ready)[5] = "PPI_var"
names(apt_ready)[6] = "USTB3M_ret"
names(apt_ready)[7] = "USTB6M_ret"
names(apt_ready)[8] = "USTB1Y_ret"
names(apt_ready)[9] = "USTB3Y_ret"
names(apt_ready)[10] = "USTB5Y_ret"
names(apt_ready)[11] = "USTB10Y_ret"
names(apt_ready)[12] = "M1_var"
names(apt_ready)[13] = "Credit_var"
names(apt_ready)[14] = "Spread_var"

View(apt_ready)

par(mfrow=c(3,2))
plot(apt_ready$microsoft_ret, type = "l", ylab = "Microsoft")
plot(apt_ready$sp500_ret, type = "l", ylab = "SP500") 
plot(apt_ready$CPI_var, type = "l", ylab = "CPI")
plot(apt_ready$PPI_var, type = "l")
plot((apt_ready$USTB10Y_ret - apt_ready$USTB3M_ret), type = "l", ylab = "Interest Spread")
plot(apt_ready$M1_var, type = "l", ylab = "M1 Supply") 
plot(apt_ready$Credit_var, type = "l", ylab = "Consumer Credit")
plot(apt_ready$Spread_var, type = "l", ylab = "Spread Bonds")

par(mfrow=c(1,1))
res = cor(apt_ready[2:ncol(apt_ready)])
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#(3)
#Regression to identify the best model
microsoft_premium = apt_ready$microsoft_ret - apt_ready$USTB3M_ret
Interest_premium = apt_ready$USTB10Y_ret - apt_ready$USTB3M_ret

apt_regr = lm(microsoft_premium~apt_ready$sp500_ret +
                 apt_ready$PPI_var + Interest_premium +
                 apt_ready$Spread_var)
summary(apt_regr)
vif(apt_regr) #no multicollinearity
par(mfrow=c(2,2))
plot(apt_regr)
ols_test_normality(apt_regr) #non-normality of residuals by tests
dwtest(apt_regr) #residuals are uncorrelated
ols_test_correlation(apt_regr) #well correlated to the expected residuals
bptest(apt_regr)#no heteroskedasticity problem





