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

############# 构建ARIMAX模型 #################
# 前言：以1992-2018年的数据为训练集，2019-2021年的数据为测试集
# 选择Second_ind，Third_ind作为后续模型构建的所使用的输入变量；GDP为响应变量
# 测试集数据导入
GDP=ts(rawdata[1:27,14],start=1992)
Second_ind=ts(rawdata[1:27,3],start=1992)
Third_ind=ts(rawdata[1:27,4],start=1992)
# 对数变换
ln_GDP=log(GDP)
ln_Second_ind=log(Second_ind)
ln_Third_ind=log(Third_ind)
# 一阶差分
F1ln_GDP=diff(ln_GDP,1,1)
F1ln_Second_ind=diff(ln_Second_ind,1,1)
F1ln_Third_ind=diff(ln_Third_ind,1,1)
# 构建ARIMAX模型
library(forecast)
data=as.matrix(cbind(F1ln_Second_ind,F1ln_Third_ind))
colnames(data)=c('F1ln_Second_ind','F1ln_Third_ind')
F1ln_GDP.model=Arima(y=F1ln_GDP,xreg=data,order=c(0,0,0))
F1ln_GDP.model

############# 残差序列构建BP神经网络 #################
#### 准备工作 ####
# 归一化函数
pre_uni <- function(x)
{
  a=min(x)
  b=max(x)
  x=(x-a)/(b-a)
  return(x)
}
# 反归一化函数
anti_uni <- function(x,x_pre)
{
  a=min(x_pre)
  b=max(x_pre)
  x=(b-a)*x+a
  return(x)
}

##### 数据预处理 ####
# 前言：选择ARIMAX模型的残差序列F1ln_GDP.model$residuals作为时间序列，建立网络结构模型
# 输入节点取5个，输出节点取1个
# 数据归一化处理：最小-最大规范化  
F1ln_GDP.model$residuals
F1ln_GDP.resid=F1ln_GDP.model$residuals
data=pre_uni(F1ln_GDP.resid)

# 训练集赋值
# 生成训练样本：以滑动窗口的方式生成输入序列和输出序列
# 训练样本个数=26-（5+1）+1=21
# 预先声明矩阵，用于储存输入序列
X_train=matrix(rep(0,times=21*5),nrow=21,ncol=5)
# 滑动窗口，生成输入序列
for (i in 1:21)
{
  for (j in 1:5)
  {
    X_train[i,j]=data[i+j-1]
  }
}
# 输出序列
Y_train=as.vector(data[6:26])

# 测试集赋值
# 输入序列
X_test=X_train[19:21,]
# X_test=matrix(rep(0,times=3*5),nrow=3,ncol=5)
# # 滑动窗口，生成输入序列
# for (i in 1:3)
# {
#   for (j in 1:5)
#   {
#     X_test[i,j]=data[21+i+j-1]
#   }
# }


##### 构建BP神经网络 #####
# 创建网络：单隐藏层的3层神经网络
# 输入节点5个；输出节点1个
# 隐藏层节点数：3~13个
# 激活函数：隐藏层tan-sigmod,输出层purelin
library(AMORE)
# 创建网络结构
net=newff(n.neurons = c(5,9,1),hidden.layer = 'tansig',output.layer = 'purelin',
          learning.rate.global = 1e-3,momentum.global = 0.5,error.criterium = 'LMS')
# 训练模型
result=train(net,X_train,Y_train,
             error.criterium = 'LMS', report=TRUE, show.step=1000, n.shows=5)
result

##### 计算拟合值 #####
fore.fit=sim(result$net,X_train)
fore.fit
# 反归一化，得到拟合值
F1ln_GDP.residuals.train=F1ln_GDP.model$residuals
F1ln_GDP.residuals.fit=anti_uni(fore.fit,F1ln_GDP.residuals.train)
F1ln_GDP.residuals.fit



############# 组合模型拟合效果图 #################
# 组合模型的拟合值
# F1ln_GDP.model$fitted     # ARIMAX模型中对数差分序列的拟合值
# F1ln_GDP.residuals.fit    # BP神经网络模型中关于对数差分序列残差的拟合值
GDP.fit=exp(ln_GDP[5:25]+F1ln_GDP.model$fitted[5:25]+F1ln_GDP.residuals.fit)  
# 绘制拟合效果图
GDP.fit=ts(GDP.fit,start=1997)
GDP.true=ts(GDP[6:26],start=1997)
ts.plot(GDP.fit,GDP.true,gpars=list(col=c("blue","red")),
        xlab='年度',ylab='GDP(亿元)')
legend('topleft',lty=c(1,1),col=c("blue","red"),
       legend=c('GDP拟合值','GDP真实值')) 


############# 组合模型预测效果评估 #################
#### 计算ARIMAX模型的预测值 #####
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
# ARIMAX模型的预测值
data=as.matrix(cbind(F1ln_Second_ind,F1ln_Third_ind))
F1ln_GDP.fore=forecast(F1ln_GDP.model,h=3,xreg=data)
F1ln_GDP.fore         # 对数差分序列的预测
F1ln_GDP.forecast=as.vector(c(0.08374488,0.02231299,0.10870807))

#### 计算BP神经网络对于残差序列的预测值 #####
# 计算预测误差
F1ln_GDP.residuals.fore=sim(result$net,X_test)
F1ln_GDP.residuals.fore
# 反归一化，得到预测值
F1ln_GDP.residuals.true=F1ln_GDP.model$residuals[24:26]
F1ln_GDP.residuals.fore=anti_uni(F1ln_GDP.residuals.fore,F1ln_GDP.residuals.true)
F1ln_GDP.residuals.fore

#### 计算ARIMAX-BP组合模型预测值及预测误差 #####
GDP.fore=exp(ln_GDP[1:3]+F1ln_GDP.forecast+F1ln_GDP.residuals.fore)
GDP.fore
# 计算预测误差
GDP.true.test=GDP[2:4]
# 计算预测误差
AE=GDP.true.test-GDP.fore    # 绝对误差
AE
RE=abs(AE)/GDP.true.test     # 相对误差
RE
mre=mean(abs(RE))            # 平均相对误差
mre

