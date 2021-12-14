# Load libraries
library(tidyverse)
library(ggthemes)
library(forecast)

library(tseries)
library(gridExtra)

library(rugarch)
library(rmgarch)

library(FinTS)
# 引入zoo




# ------------------------------------------------------------------------------
# 1、导入感兴趣股票序列数据，需保证股票序列的长度相同
# ------------------------------------------------------------------------------
#导入数据
#数据1
SP500 = read.csv('C:/Users/blinksch/Desktop/dcc3/Index/S&P500^GSPC.csv' ,
              header = T)
SP500 = SP500[,c('Date','Open')]
names(SP500) <- c('Date','SP500')
SP500$Date = as.Date(SP500$Date,"%Y-%m-%d")

#数据2
nasdaq = read.csv('C:/Users/blinksch/Desktop/dcc3/Index/Nasdaq^IXIC.csv' ,
                 header = T)
nasdaq = nasdaq[,c('Date','Open')]
names(nasdaq) <- c('Date','nasdaq')
nasdaq$Date = as.Date(nasdaq$Date,"%Y-%m-%d")

#数据3
DJI = read.csv('C:/Users/blinksch/Desktop/dcc3/Index/DowJones^DJI.csv' ,
                 header = T)
DJI = DJI[,c('Date','Open')]
names(DJI) <- c('Date','DJI')
DJI$Date = as.Date(DJI$Date,"%Y-%m-%d")

#数据4
VIT = read.csv('C:/Users/blinksch/Desktop/dcc3/Index/Volatility^VIX.csv' ,
                 header = T)
VIT = VIT[,c('Date','Open')]
names(VIT) <- c('Date','VIT')
VIT$Date = as.Date(VIT$Date,"%Y-%m-%d")

# 时间区间
startday = as.Date("2009-10-8","%Y-%m-%d")
endday = as.Date("2021-11-3","%Y-%m-%d")
# str(BTC)


# 转换成日期格式
SP500 = SP500[which(SP500$Date <= endday & SP500$Date >= startday),]
nasdaq = nasdaq[which(nasdaq$Date <= endday & nasdaq$Date >= startday),]
DJI = DJI[which(DJI$Date <= endday & DJI$Date >= startday),]
VIT = VIT[which(VIT$Date <= endday & VIT$Date >= startday),]
# names(SP500) <- c('date','sp500')
# 切片提取

# 将所有的数据按照日期合并，使得每个数据序列的日期相同
mergedata <- merge(SP500, nasdaq, by="Date")
mergedata <- merge(mergedata, DJI, by="Date")
mergedata <- merge(mergedata, VIT, by="Date")

# 抽取
time = as.Date(mergedata$Date,"%Y-%m-%d")

library(tseries)
price = ts(mergedata[,c(2,3,4,5)])    # 比特币价格序列
names = names(mergedata)[2:5]

# ------------------------------------------------------------------------------
#   作时序图观察序列的特性，如平稳性等
#   计算一些基础统计量
# ------------------------------------------------------------------------------
# 时序图函数
library(DistributionUtils)
# 描述性统计量
data_outline = function(x){
  m = mean(x)
  d=max(x)
  xd=min(x)
  me = median(x)
  s = sd(x)
  kur=kurtosis(x)
  ske=skewness(x)
  R = max(x)-min(x)
  data.frame( Mean=m,  Median=me, max=d,min=xd,std_dev=s,
              Skewness=ske, Kurtosis=kur, R=R)
}

view <- function(sequence, time, names){
  ## 画出序列1和序列2的时序图,sequence1、sequence2必须是ts对象
  ## names 序列名称，字符串形式，与序列对应
  par(mfrow=c(1,2),oma=c(0.2,0.2,0.2,0.2))
  # library(FinTS)
  plot(zoo(sequence[,1], time), xlab="time(day)", ylab="price(USD)",
       col = 'red',main = paste(names[1], "series"))
  plot(zoo(sequence[,2], time), xlab="time(day)", ylab="price(USD)",
       col = 'blue',main = paste(names[2], "series"))
  par(mfrow=c(1,2),oma=c(0.2,0.2,0.2,0.2))
  plot(zoo(sequence[,3], time), xlab="time(day)", ylab="price(USD)",
       col = 'red',main = paste(names[3], "series"))
  plot(zoo(sequence[,4], time), xlab="time(day)", ylab="price(USD)",
       col = 'blue',main = paste(names[4], "series"))
  # 描述性统计量
  print("描述性统计量：")
  print(data_outline(sequence[,1]))
  print(data_outline(sequence[,2]))
  print(data_outline(sequence[,3]))
  print(data_outline(sequence[,4]))
  ## 相关系数矩阵，提供参考
  print("相关性系数：")
  cor(sequence)
}

view(price, time, names)


# ------------------------------------------------------------------------------
# 2、序列平稳化(差分法)，采用对数收益差分
# ------------------------------------------------------------------------------

# 对数收益率
returns = diff(log(price))  # 比特币价格序列

view(returns, time, names)


# ------------------------------------------------------------------------------
#   平稳性检验,时序图观察只能作为参考,不能成为重要依据。
#   Dickey-Fuller检验, 原假设是存在单位根，不平稳
#   其他调用函数 ：urca.df或者FunitRoot.adfTest()
# ------------------------------------------------------------------------------
for(i in 1:4)
  print(adf.test(returns[,i], alt="stationary"))



# ------------------------------------------------------------------------------
#   白噪声检验，白噪声没有任何研究意义,判断和滞后项间是否存在相关性
# ------------------------------------------------------------------------------
for(i in 1:4)
  print(Box.test(returns[,i],lag=12 ,type = "Ljung-Box"))



# ------------------------------------------------------------------------------
#   3、对平稳序列均值方程建模
# ------------------------------------------------------------------------------
# 法一：做序列的acf和pacf图作为模型arma(p,q)定阶的参考，acf确定q,pacf确定p；
# 由此来建立收益率模型，
view_cf = function(sequence){
  par(mfrow=c(1,2))
  acf(sequence)
  pacf(sequence)
}
for(i in 1:4)
  view_cf(returns[,i])

# 法二：但是forcast包里有更为简单的建立均值方程的方式，即调用auto.arima
library(forecast)
# arima模型根据AIC准则自动拟合收益率模型
armamodel1=auto.arima(returns[,1],ic = 'aic') 
armamodel1   ## 查看序列1模型基础信息
armamodel2=auto.arima(returns[,2],ic = 'aic')
armamodel2   ## 查看序列2模型基础信息
armamodel3=auto.arima(returns[,3],ic = 'aic') 
armamodel3   ## 查看序列3模型基础信息
armamodel4=auto.arima(returns[,4],ic = 'aic')
armamodel4   ## 查看序列4模型基础信息


# # 不断尝试微调，直到残差能通过白噪声检验,追求完美可以去保证模型系数的显著性
# # fixed = c(NA,NA,NA,0,0,NA,NA,NA)可以固定某些参数为0
# armamodel3_medify<-arima(returns[,3], order = c(6,0,2),
#                          include.mean = F)
# summary(armamodel3_medify)
# tsdiag(armamodel3_medify)  # 模型残差情况
# confint(armamodel3_medify)  # 模型系数的显著性
# resid(armamodel3_medify)
# Box.test(residuals(armamodel3_medify),lag=30 , type = "Ljung-Box")


# ------------------------------------------------------------------------------
#   判断建立的均值的模型的好坏
# ------------------------------------------------------------------------------
# confint(armamodel2)
# 提取残差进行白噪声检验(LB)检测残差的异方差性,检测时q值若小于显著水平，则拒绝，
# 即拒绝序列无自相关性，否则接受，这里残差不存在自相关性
Box.test(residuals(armamodel1),lag=30 ,type = "Ljung-Box")        
Box.test(residuals(armamodel2),lag=30 , type = "Ljung-Box")
Box.test(residuals(armamodel3),lag=30 ,type = "Ljung-Box")        
Box.test(residuals(armamodel4),lag=30 , type = "Ljung-Box")

# plot(residuals(armamodel1)^2)   # 可视化残差序列
# pacf(residuals(armamodel1)^2)

# ------------------------------------------------------------------------------
#   4、进一步建模前准备：判断残差的异方差性，即方差是随着时间改变
# -----------------------------------------------------------------------------
# arch检验，判断是否有波动聚集现象或者异方差性，原假设是：不存在这样的现象
# 进一步判断是否需要对方差进行建模

# # 调用1：
# # 残差的平方序列进行LB检验，原假设：序列间无自相关，近假设五期滞后不相关
# Box.test(residuals(armamodel1)^2,lag=12,type = "Ljung-Box")
# Box.test(residuals(armamodel2)^2,lag=12,type = "Ljung-Box")
# Box.test(residuals(armamodel3)^2,lag=12,type = "Ljung-Box")


# # 调用2：
# #也可以使用FinTS.ArchTest()
# # install.packages('MTS')
# # library(MTS)
# archTest(residuals(armamodel1),lag=12)
# archTest(residuals(armamodel2),lag=12)
# archTest(residuals(armamodel3),lag=12)

# # 调用3：
# 使用FinTS.ArchTest()
library(FinTS)
ArchTest(residuals(armamodel1), lag=5)
ArchTest(residuals(armamodel2), lag=5)
ArchTest(residuals(armamodel3), lag=5)
ArchTest(residuals(armamodel4), lag=5)

# ------------------------------------------------------------------------------
#   5、dcc-garch模型,mgarch包
# ------------------------------------------------------------------------------
# 建立dcc模型(多变量相关性研究)
meanSpec=list(armaOrder=c(0,0),include.mean=FALSE,archpow=1)
distSpec=c("mvnorm")
varSpec=list(model="sGARCH",garchOrder=c(1,1))

# 单变量garch
spec1=ugarchspec(mean.model=meanSpec,variance.model=varSpec)
# 多变量garch
mySpec=multispec(replicate(4,spec1))

mspec=dccspec(mySpec,VAR=F,robust = F,lag=1,lag.max=NULL,
              lag.criterion = c("AIC"),external.regressors = NULL,
              robust.control = list(gamma=0.25,delta=0.01,nc=10,ns=500),
              dccOrder = c(1,1),distribution = distSpec,
              start.pars = list(),fixed.pars = list())
# 查看模型
mspec


# dcc模型拟合
dcc_fit=dccfit(data = returns ,mspec,out.sample = 10,solver = "solnp",
               solver.control = list(),
               fit.control = list(eval.se=TRUE,stationary=TRUE,scale=FALSE),
               parallel=TRUE,parallel.control=list(pkg=c("multicore"),cores=2),
               fit=NULL,VAR.fit=NULL)

# 查看拟合结果
class(dcc_fit)
show(dcc_fit)

# 提取动态条件相关系数序列
# correlation12 = ts(rcor(dcc_fit, type = 'R')[1, 2, ])
# correlation13 = ts(rcor(dcc_fit, type = 'R')[1, 3, ])
# correlation14 = ts(rcor(dcc_fit, type = 'R')[1, 4, ])
# correlation23 = ts(rcor(dcc_fit, type = 'R')[2, 3, ])
# correlation24 = ts(rcor(dcc_fit, type = 'R')[2, 4, ])
# correlation34 = ts(rcor(dcc_fit, type = 'R')[3, 4, ])
# 可视化动态动态相关曲线
# palette()  # 查看调色板
for (i in 1:3){
  for (j in (i+1):4){
    par(mfrow=c(1,1),oma=c(0.2,0.2,0.2,0.2))  
    plot(zoo(ts(rcor(dcc_fit, type = 'R')[i,j,]),time), xlab="time", ylab="coef",
         col="red", main=paste("dcc between", names[i],"and", names[j]))
    grid(lwd = 2,col = "gray")
  }
}






