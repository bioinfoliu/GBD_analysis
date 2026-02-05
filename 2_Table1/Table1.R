#### Table1 2021年不同性别，全年龄，及年龄标化率的疾病负担####
library(dplyr)              
library(ggplot2)

IBD_KOR <- read.csv('TB_KOR_GLOBAL.csv',header = T) 
#查看数据的格式
str(IBD_KOR)
table(IBD_KOR$measure_name)  
table(IBD_KOR$metric_name)

####第一步：首先提取2021年全年龄段(All ages)的人数(Number)####
IBD_KOR_number<-subset(IBD_KOR,    ####subset()函数类似excel的筛选功能，用来选数据的子集，也可在Excel完成
                                IBD_KOR$age_name=='All ages' &   ###选择年龄为全年龄段
                                IBD_KOR$year =='2021'&           ###选择年份为2021
                                IBD_KOR$sex_name =='Both'&
                                IBD_KOR$metric_name== 'Number'  ###选择数据格式为数值
                         )
#####由于是人数，所以我们取整数
IBD_KOR_number$val <- round(IBD_KOR_number$val,0)  
IBD_KOR_number$lower <- round(IBD_KOR_number$lower,0)
IBD_KOR_number$upper <- round(IBD_KOR_number$upper,0) 

####把数值与其上下不确定性区间合并为一个单元格
IBD_KOR_number$UI <- paste(IBD_KOR_number$lower,IBD_KOR_number$upper,sep = ',') 
IBD_KOR_number$UI <- paste(IBD_KOR_number$UI,')',sep = '')  ##95%UI
IBD_KOR_number$UI <- paste('(',IBD_KOR_number$UI,sep = '')  ##95%UI
IBD_KOR_number$UI <- paste(IBD_KOR_number$val,IBD_KOR_number$UI,sep = ' ')


unique(IBD_KOR$age_name)

####第二步，我们提取年龄标准化(Age-standardized)的率(Rate)####
IBD_KOR_Rate<-subset(IBD_KOR, IBD_KOR$age_name=='Age-standardized' &   ###选择年龄标准化
                           IBD_KOR$year =='2021'&              ###选择年份为2021
                       IBD_KOR$sex_name =='Both'&
                           IBD_KOR$metric_name== 'Rate'  ###选择数据格式为率值
                       )

#####由于是率，所以我们取2位或3为小数，这里我们取2位小数
IBD_KOR_Rate$val <- round(IBD_KOR_Rate$val,2)  
IBD_KOR_Rate$lower <- round(IBD_KOR_Rate$lower,2)
IBD_KOR_Rate$upper <- round(IBD_KOR_Rate$upper,2) 
####把数值与其上下不确定性区间合并为一个单元格
IBD_KOR_Rate$UI <- paste(IBD_KOR_Rate$lower,IBD_KOR_Rate$upper,sep = ',') 
IBD_KOR_Rate$UI <- paste(IBD_KOR_Rate$UI,')',sep = '')  ##95%UI
IBD_KOR_Rate$UI <- paste('(',IBD_KOR_Rate$UI,sep = '')  ##95%UI
IBD_KOR_Rate$UI <- paste(IBD_KOR_Rate$val,IBD_KOR_Rate$UI,sep = ' ')


####第三步，结果输出####
write.csv(IBD_KOR_number,"TB_KOR_number_2021.csv")   ###输出结果为csv文件
write.csv(IBD_KOR_Rate,"TB_KOR_Rate_2021.csv")       ###输出结果为csv文件
