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
# 数据整理
data=as.matrix(rawdata)
x=data[,2:13]      # 自变量
y=data[,14]       # 因变量


#################### 变量相关性分析 #######################
# 方法一
library(ggcorrplot)
corr=cor(x,method='pearson')
ggcorrplot(corr,method = "circle",lab=T)
# 判断结果：
# 发现变量中存在高度相关性，为此使用Lasso进行变量选择

# 方法二
library(corrplot)
corr=cor(x,method='pearson')
# 变量相关性显著性检验结果
testRes=cor.mtest(x,conf.level=0.95)
# 下三角放相关系数
corrplot(corr,method='number',type='lower',tl.pos='lt')
# 上三角放色块叠加显著性星号
corrplot(corr,method='color',type='upper',add=T,
         tl.pos='n',cl.pos='n',diag=F,p.mat=testRes$p,
         sig.level=c(0.001,0.01,0.05),pch.cex=1.5,insig='label_sig')
# 判断结果：
# 发现变量中存在高度相关性，为此使用Lasso进行变量选择

#################### 变量选择 #######################
# 数据标准化
data1=as.data.frame(rawdata[,2:14])
zdata=as.data.frame(scale(data1))
data=as.matrix(zdata)
x=data[,1:12]      # 自变量
y=data[,13]       # 因变量

# LASSO回归作变量选择
library(lars)
larx<-lars(x,y,type = "lasso") # 进行Lasso回归，结果存入larx
larx                           # 查看Lasso回归的结果
plot(larx)                     # 用图表形式查看Lasso回归结果
summary(larx)                  # 输出数据的细节，查看Cp、Df、Step
larx$Cp[which.min(larx$Cp)]    # 直接选取最小的Cp值
larx$beta                      # 得到每一步对应的自变量对应的系数
coef=coef.lars(larx,mode="step",s=14)  # 获取指定自变量系数，s=step+1
coef[coef!=0]                  # 获取系数值不为零的自变量对应的系数值
# 判断结果：
# 选择Second_ind，Third_ind作为后续模型构建的所使用的输入变量