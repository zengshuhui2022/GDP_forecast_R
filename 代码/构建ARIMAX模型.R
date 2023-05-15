############# 数据导入 #################
# 移除所有对象
rm(list = ls())
# 使后续数据直观表示，而不使用科学计数法
options(scipen=200)
# 读取数据
rawdata <- read.table(file.choose(), header = T, sep = ",")
# 显示数据概况
colnames(rawdata)=c('year','First_ind','Second_ind','Third_ind',
                    'af_add','ind_add','con_add','retail_add',
                    'traf_add','ac_add','fin_add','rei_add',
                    'other_add','GDP')
skimr::skim(rawdata)

############# 数据预处理 #################
# 前言：以1992-2018年的数据为训练集，2019-2021年的数据为测试集
# 选择Second_ind，Third_ind作为后续模型构建的所使用的输入变量；GDP为响应变量
GDP=ts(rawdata[1:27,14],start=1992)
Second_ind=ts(rawdata[1:27,3],start=1992)
Third_ind=ts(rawdata[1:27,4],start=1992)
# 作时序图
plot(GDP,type='l',col='blue',xlab='年份',ylab=' ',main='原序列时序图')
lines(Second_ind,type='l',col='green')
lines(Third_ind,type='l',col='red')
legend('topleft',lty=c(1,1,1),col=c("blue",'green','red'),
       legend=c('GDP','Second_ind','Third_ind'))    # 添加图例

# 对原始数据作对数变换
ln_GDP=log(GDP)
ln_Second_ind=log(Second_ind)
ln_Third_ind=log(Third_ind)
# 作时序图
plot(ln_GDP,type='l',col='blue',xlab='年份',ylab=' ',main='对数序列时序图')
lines(ln_Second_ind,type='l',col='green')
lines(ln_Third_ind,type='l',col='red')
legend('topleft',lty=c(1,1,1),col=c("blue",'green','red'),
       legend=c('ln_GDP','ln_Second_ind','ln_Third_ind'))  

# 一阶差分，并检验一阶差分序列的平稳性
F1ln_GDP=diff(ln_GDP,1,1)
F1ln_Second_ind=diff(ln_Second_ind,1,1)
F1ln_Third_ind=diff(ln_Third_ind,1,1)
# 作时序图
plot(F1ln_GDP,type='l',col='blue',xlab='年份',ylab=' ',main='对数序列的一阶差分序列的时序图')
lines(F1ln_Second_ind,type='l',col='green')
lines(F1ln_Third_ind,type='l',col='red')
legend('topright',lty=c(1,1,1),col=c("blue",'green','red'),
       legend=c('F1ln_GDP','F1ln_Second_ind','F1ln_Third_ind'))
# 作平稳性检验
library(fUnitRoots)
print(adfTest(F1ln_GDP,lag=1,type='nc'))
print(adfTest(F1ln_Second_ind,lag=1,type='nc'))
print(adfTest(F1ln_Third_ind,lag=1,type='nc'))
# 判断结果：
# 拒绝原假设：一阶差分序列平稳

# 作白噪声检验
for (i in 1:2) 
  print(Box.test(F1ln_GDP,lag=6*i))
for (i in 1:2) 
  print(Box.test(F1ln_Second_ind,lag=6*i))
for (i in 1:2) 
  print(Box.test(F1ln_Third_ind,lag=6*i))
# 判断结果：拒绝原假设，即对数一阶差分序列非纯随机，可以用于拟合ARIMAX模型

############# 构建ARIMAX模型 #################
# step1:对输入序列F1ln_Second_ind，F1ln_Third_ind分别拟合ARMA模型

## 对于ln_Second_ind
acf(F1ln_Second_ind)
pacf(F1ln_Second_ind)
# 判断结果:ACF拖尾，PACF拖尾
# 模型拟合
ln_Second_ind.fit=arima(F1ln_Second_ind,order=c(4,0,0))
ln_Second_ind.fit
# 残差白噪声检验
for (i in 1:2) 
  print(Box.test(ln_Second_ind.fit$residuals,lag=6*i))
# 判断结果：不拒绝原假设，即残差序列纯随机，模型有效

## 对于ln_Third_ind
acf(F1ln_Third_ind)
pacf(F1ln_Third_ind)
# 判断结果:ACF拖尾，PACF1阶拖尾
# 模型拟合
ln_Third_ind.fit=arima(F1ln_Third_ind,order=c(1,0,0))
ln_Third_ind.fit
# 残差白噪声检验
for (i in 1:2) 
  print(Box.test(ln_Third_ind.fit$residuals,lag=6*i))
# 判断结果：不拒绝原假设，即残差序列纯随机，模型有效

# step2:仿照输入序列建模结果对输出序列F2ln_GDP拟合ARMA模型，并绘制残差互相关图，分析滞后效应
## 对于ln_Second_ind
ln_GDP.fit2=arima(F1ln_GDP,order=c(4,0,0))
ccf(ln_GDP.fit2$residuals,ln_Second_ind.fit$residuals)
## 对于ln_Third_ind
ln_GDP.fit3=arima(F1ln_GDP,order=c(1,0,0))
ccf(ln_GDP.fit3$residuals,ln_Third_ind.fit$residuals)
# 判断结果：无滞后效应，可以同期建模

# step3：拟合ARIMAX模型
# 模型拟合
library(forecast)
data=as.matrix(cbind(F1ln_Second_ind,F1ln_Third_ind))
colnames(data)=c('F1ln_Second_ind','F1ln_Third_ind')
auto.arima(y=F1ln_GDP,xreg=data)
F1ln_GDP.model=Arima(y=F1ln_GDP,xreg=data,order=c(0,0,0))
F1ln_GDP.model
# data=as.matrix(cbind(ln_Second_ind,ln_Third_ind))
# colnames(data)=c('ln_Second_ind','ln_Third_ind')
# auto.arima(y=ln_GDP,xreg=data)
# GDP.fit=arima(x=ln_GDP,xreg=data,order=c(1,0,0))
# GDP.fit

# 残差序列平稳性检验
# 对残差序列进行平稳性检验
library(fUnitRoots)
for (i in 1:2)
 {print(adfTest(F1ln_GDP.fit$residuals,lag=i,type='nc'))}
# 判断结果：拒绝原假设，即认为残差序列平稳
# 对残差序列进行纯随机性检验
for (i in 1:2) 
  print(Box.test(F1ln_GDP.fit$residuals,lag=6*i))
# 判断结果：不拒绝原假设，即认为残差序列纯随机



############# 模型拟合 #################
F1ln_GDP.model$fitted     # 对数差分序列的拟合值
GDP.fit=exp(ln_GDP[1:26]+F1ln_GDP.model$fitted)   # 变换得到原序列的拟合值
# 绘制拟合效果图
GDP.fit=ts(GDP.fit,start=1993)
GDP.true=ts(GDP[2:27],start=1993)
ts.plot(GDP.fit,GDP.true,gpars=list(col=c("blue","red")),
        xlab='年度',ylab='GDP(亿元)')
legend('topleft',lty=c(1,1),col=c("blue","red"),
       legend=c('GDP拟合值','GDP真实值')) 

############# 模型预测及效果评估 #################
library(forecast)
# 测试集数据导入
GDP=ts(rawdata[27:30,14],end=2021)
Second_ind=ts(rawdata[27:30,3],end=2021)
Third_ind=ts(rawdata[27:30,4],end=2021)
# 对原始数据作对数变换
ln_GDP=log(GDP)
ln_Second_ind=log(Second_ind)
ln_Third_ind=log(Third_ind)
# 作一阶差分
F1ln_GDP=diff(ln_GDP,1,1)
F1ln_Second_ind=diff(ln_Second_ind,1,1)
F1ln_Third_ind=diff(ln_Third_ind,1,1)
# 模型预测
data=as.matrix(cbind(F1ln_Second_ind,F1ln_Third_ind))
F1ln_GDP.fore=forecast(F1ln_GDP.model,h=3,xreg=data)
F1ln_GDP.fore         # 对数差分序列的预测
F1ln_GDP.forecast=as.vector(c(0.08374488,0.02231299,0.10870807))
GDP.fore=exp(ln_GDP[1:3]+F1ln_GDP.forecast)   # 变换得到原序列的拟合值
# 计算预测误差
GDP.true.test=GDP[2:4]
AE=GDP.true.test-GDP.fore    # 绝对误差
AE
RE=abs(AE)/GDP.true.test     # 相对误差
RE
mre=mean(abs(RE))            # 平均相对误差
mre















