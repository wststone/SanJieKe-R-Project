#1.数据挖掘（数据获取）:
setwd("/Users/siteng/Documents/GitHub/SanJieKe-R-project")
#设置正确的文档读取路径
hpData<- read.csv("房价数据.csv", fileEncoding = "GBK") 
#hpData表示house price data房价数据
summary(hpData)
str(hpData)
#从宏观看数据结构及类型

#2.数据清洗:
hpData$价格.万元. <- as.numeric(hpData$价格.万元.)
hpData$面积.平米. <- as.numeric(hpData$面积.平米.)
class(hpData$价格.万元.)
class(hpData$面积.平米.)
#将“价格（万元）”、“面积（平米）”变量变为数值型
library(dplyr)
newhpData <- hpData%>%
mutate(单价=价格.万元./面积.平米.)%>% 
#使用dplyr包创建新变量“单价”
arrange(挂牌时间)
head(newhpData)
#按照“挂牌时间”升序排序

#异常值处理：
IQR <- quantile(hpData$单价)[4]-quantile(hpData$单价)[2] #定义IQR
extreme.lower.bound <- quantile(hpData$单价)[2]-3*IQR 
extreme.upper.bound <- quantile(hpData$单价)[4]+3*IQR 
#定义极度异常值的边界
mild.lower.bound <- quantile(hpData$单价)[2]-1.5*IQR 
mild.upper.bound <- quantile(hpData$单价)[4]+1.5*IQR 
#定义轻度异常值的边界
extreme.new.hpData <- hpData[hpData$单价<=extreme.upper.bound & hpData$单价 >= extreme.lower.bound,] 
mild.new.hpData <- hpData[hpData$单价<=mild.upper.bound & hpData$单价 >= mild.lower.bound,] 
hpData <- mild.new.hpData 

# 3.数据探索（数据可视化）:
library(ggplot2) 
# 绘制箱型图展示不同楼型房屋单价的分布情况
g1 <- ggplot(newhpData,aes(楼型,单价)) 
g1 + geom_boxplot() +
  # 改变坐标名称
  labs(x="楼型") +
  labs(y="单价（万元）") +
  # 修改题目名称
  labs(title="不同楼型房屋单价的分布情况")

# 通过散点图展示房屋单价与其他变量之间的关系
# 单价与面积
g2 <- ggplot(newhpData,aes(单价,面积.平米.))  
g2+geom_point(alpha=1/3)+ 
  # 用线性回归拟合数据，颜色为steelblue 
  geom_smooth(method="lm",col="steelblue") + 
  # 个性化设置
  theme_minimal(base_family = "Avenir",base_size = 10) + 
  # 横坐标为“单价” 
  labs(x="单价") +  
  # 纵坐标为“面积” 
  labs(y="面积") + 
  # 设置标题 
  labs(title="房屋单价与面积关系") 

# 挂牌时间和单价之间的关系
g3 <- ggplot(newhpData,aes(单价,挂牌时间))  
g3 + geom_point(alpha=1/3)+ 
  # 用线性回归拟合数据，颜色为steelblue 
  geom_smooth(method="lm",col="steelblue") + 
  # 个性化设置
  theme_minimal(base_family = "Avenir",base_size = 10) + 
  # 横坐标为“单价” 
  labs(x="单价") +  
  # 纵坐标为“面积” 
  labs(y="挂牌时间") + 
  # 设置标题 
  labs(title="房屋单价与挂牌时间关系") 

# 朝向等的关系:
g4 <- ggplot(newhpData,aes(朝向,单价))  
g4 + geom_point(alpha=1/3)+ 
  # 用线性回归拟合数据，颜色为steelblue 
  geom_smooth(method="lm",col="steelblue") + 
  # 个性化设置
  theme_minimal(base_family = "Arial",base_size = 10) + 
  # 横坐标为“单价” 
  labs(x="朝向") +  
  # 纵坐标为“面积” 
  labs(y="单价") + 
  # 设置标题 
  labs(title="房屋单价与朝向关系") 