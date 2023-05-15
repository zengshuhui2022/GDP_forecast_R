############# 准备工作 #################
# 移除所有对象
rm(list = ls())
# 使后续数据直观表示，而不使用科学计数法
options(scipen=200)

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

############# 数据导入 #################
# 读取数据
rawdata <- read.table(file.choose(), header = T, sep = ",")
# 显示数据概况
colnames(rawdata)=c('year','First_ind','Second_ind','Third_ind',
                    'af_add','ind_add','con_add','retail_add',
                    'traf_add','ac_add','fin_add','rei_add',
                    'other_add','GDP')
skimr::skim(rawdata)


############# 数据预处理 #################
# 前言：选择历年GDP作为时间序列建立网络结构模型
# 输入节点取5个，输出节点取1个
# 数据归一化处理：最小-最大规范化  
GDP=rawdata[,14]
data=pre_uni(GDP)

# 数据拆分:以1992-2018年的数据为训练集，2019-2021年的数据为测试集
# data_train=data[1:27]
# data_test=data[28:30]

# 训练集赋值
# 生成训练样本：以滑动窗口的方式生成输入序列和输出序列
# 训练样本个数=27-（5+1）+1=22
# 预先声明矩阵，用于储存输入序列
X_train=matrix(rep(0,times=22*5),nrow=22,ncol=5)
# 滑动窗口，生成输入序列
for (i in 1:22)
{
  for (j in 1:5)
  {
    X_train[i,j]=data[i+j-1]
  }
}
# 输出序列
Y_train=as.vector(data[6:27])


# 测试集赋值
# 输入序列
X_test=matrix(rep(0,times=3*5),nrow=3,ncol=5)
# 滑动窗口，生成输入序列
for (i in 1:3)
{
  for (j in 1:5)
  {
    X_test[i,j]=data[22+i+j-1]
  }
}
# 输出序列
Y_test=as.vector(data[28:30])


############# 构建BP神经网络 #################
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

############# 模型评估 #################
# step1：绘制拟合曲线与真实曲线
# 计算拟合值
fore.fit=sim(result$net,X_train)
fore.fit
# 反归一化，得到拟合值
GDP.true.train=rawdata[6:27,14]
GDP.fit=anti_uni(fore.fit,GDP.true.train)
GDP.fit
fit=ts(GDP.fit,start=1997)
true=ts(GDP.true.train,start=1997)
ts.plot(fit,true,gpars=list(col=c("blue","red")),
        xlab='年度',ylab='GDP(亿元)')
legend('topleft',lty=c(1,1),col=c("blue","red"),
       legend=c('GDP拟合值','GDP真实值')) 

# step2：计算预测误差
# 计算预测值
fore=sim(result$net,X_test)
fore
# 反归一化，得到预测值
GDP.true.test=rawdata[28:30,14]
GDP.fore=anti_uni(fore,GDP.true.test)
GDP.fore
# 计算预测误差
AE=GDP.true.test-GDP.fore    # 绝对误差
AE
RE=abs(AE)/GDP.true.test     # 相对误差
RE
mre=mean(abs(RE))            # 平均相对误差
mre

































