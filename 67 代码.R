#67 练习代码

#一：读取67初始数据

Analysis=read.csv("67AnalysisV03.csv")
fix(Analysis)
names(Analysis)

#二：数据分析
#对数据简单分析,summary提供最小值、下四分位数、中位数、平均值、上四分位数、最大值
#可以看出所有变量的最小值(Min)和最大值(Max)都没有意义有些数据的最小值是0如cpu 磁盘空间(disksp)等。
#其实这些数据可能是因为四舍五入小于1的值导致显示为0.还可以看出r1-r10好像有一些数据缺失
#非数值统计出频数
summary(Analysis)
#总数,因为不全为数值，所以要删掉一些
#Analysis3<-Analysis[,-c(22:26)] 
#Analysis3<-Analysis[-c(22:26),] 
Analysis2=read.csv("67AnalysisV04.csv")
fix(Analysis2)
sum(Analysis$r1)

#对空缺数据进行处理
#判断缺失数据FALSE为不缺
is.na(Analysis)
#统计缺失值个数
Analysis2=read.csv("67AnalysisV04.csv")
sum(is.na(Analysis))

#查看每个样本数据是否完整，其值与is.na()相反
complete.cases(Analysis)

#3种处理缺失值的方法（选其一）
#选择无缺失值的记录，代码如下：
Analysis1=Analysis[complete.cases(Analysis),]

#删除所有有缺失值的记录，这种方法在数据处理中是最常用的。
Analysis2=na.omit(Analysis)

#替换缺失值。通过一定的统计方法计算出相应值来替换缺失值。一般的方法有：平均值法（最常用）、多重插补法、随机模拟法回归预测（较复杂）。
#平均值法如下：
#使用已有值的平均值来代替缺失数据
attach(Analysis)
y[is.na(y)]=mean(y,na.rm=T)
x1[is.na(x1)]=mean(x1,na.rm=T)
x2[is.na(x2)]=mean(x2,na.rm=T)
Analysis=Analysis.frame(y,x1,x2)

#查看两个数据的Person系数 ,P值越小，说明两个关系越强
x <- Analysis1$r2
y <- Analysis1$r9

library(stats)
#单个两个数据的系数Spearman Kendall Pearson
x <- Analysis1$r7
y <- Analysis1$r9
cor(x,y,method="pearson")
cor(x,y,method="spearman")
cor(x,y,method="kendall")

#计算表中数值列Spearman Kendall Pearson
x <- Analysis1[,c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10",
                  "mid","correff","totfp","pcobol","COBOL","ptelon","peasy","pjcl","JCL","t","ageend","avetrans",
                  "disksp","appdef","defect.density")]
y <- Analysis1[,c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10",
                  "mid","correff","totfp","pcobol","COBOL","ptelon","peasy","pjcl","JCL","t","ageend","avetrans",
                  "disksp","appdef","defect.density")]
cor(x,y,method="pearson")
cor(x,y,method="spearman")
cor(x,y,method="kendall")

#如果表内全为数据，则下列可以直接计算出数值数Spearman Kendall Pearson
Analysis1cov<-cov(Analysis1)
Analysis1cor<-cor(Analysis1,method="pearson")

#三：直方图
#绘制直方图，纵坐标为频数，横坐标为变量
library(graphics)
#将页面分成几部分：4
par(mfrow=c(2,2))
hist(Analysis1$mid)
hist(Analysis1$r1)
hist(Analysis1$correff)
hist(Analysis1$appdef)


#由上直方图可知correff并不正态分布，所以对其取对数变为新变量acorreff
#指数函数log(p1 [, p2])，其中p1为幂，p2为底数
#若p2不存在，则底数为e，结果为指数。
acorreff<-log(Analysis1$correff,100)
hist(acorreff)

#不正太分组做法
#先看直方图
hist(Analysis1$cpu)
cpulev<-Analysis1$cpu/24
hist(cpulev)
y1=ifelse(Analysis1$cpu<10,1,ifelse(Analysis1$cpu<99,2,ifelse(Analysis1$cpu<999,3,4)))
hist(y1)

#YES NO的语句可能用下一句
#若x的值包含在1里面，输出yes，否者输出no
#ifelse(x %in% 1, 'yes', 'no') 


#四：散点图
#绘制两个变量的散点图
library(graphics)
#r1 r2散点图
a <- Analysis1$r2
b <- Analysis1$r9
plot(a,b)
#ln(acorreff) ln(totfp)散点图
a <- log(acorreff,100)
b <- log(Analysis1$totfp,100)
#不同区域散点图颜色不同
plot(a,b)
par(mfrow=c(1,1))

library(ggplot2)
data(diamonds)
head(diamonds)
ggplot(Analysis1, aes(a, b, color = "class",shape = "class"))+
  geom_point(size = 2.0, shape = 16)

#五：分类变量
#非数量变量分组，如出现频率
summary(Analysis)
#对于类别变量
# 统计每个类别的计数，了解一下各个类别的分布
table(Analysis$tpms) 
table(Analysis$dbms) 
table(Analysis$apptype) 
table(Analysis$morg) 
table(Analysis$borg) 
# 画出每个类别的占比饼图
pie(table(Analysis$tpms)) 
pie(table(Analysis$dbms))
pie(table(Analysis$apptype))
pie(table(Analysis$morg))
pie(table(Analysis$borg))
# 画出柱状图
barplot(table(Analysis$tpms)) 

#六：简单线性回归
#简单线性回归分析
#ln(acorreff) ln(totfp)回归分析
a <- log(acorreff,100)
b <- log(Analysis1$totfp,100)
#lm.fit=lm(r1~r2,data=Analysis1)
lm.fit=lm(a~b)
lm.fit
summary(lm.fit)
#下面这条语句会输出P值和标准误以及R2和F统计量
summary(lm.fit)
#给定X值来预测Y值，下语句显示出置信区间和预测区间
predict(lm.fit,data.frame(r2=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(r2=(c(5,10,15))), interval="prediction")
#散点图,注意这边散点图的XY和lm的XY相反
plot(b,a)
#最小二乘回归直线，画直线就abline(a,b)
abline(lm.fit,lwd=3,col="red")
#用残差分析剔除异常点
plot(lm.fit,which=1:4)

#七：多元线性回归
#多元线性最小二乘lm(y~x1+x2+x3)
a <- log(acorreff,100)
b <- log(Analysis1$totfp,100)
lm.fit=lm(a~b+mid+JCL,data=Analysis1)
summary(lm.fit)
abline(lm.fit,lwd=3,col="red")
#用残差分析剔除异常点
plot(lm.fit,which=1:4)


#多元线性还可以用如下语句，可以不停变换
#house1 <-  lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
#summary(house1)

#除age变量回归
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)

#三维散点图
library(rgl)
plot3d(Girth,Height,Volume)


