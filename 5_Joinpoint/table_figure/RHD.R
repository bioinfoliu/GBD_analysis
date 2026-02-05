
library(dplyr)                           
library(ggplot2)
RHD <- read.csv('total.csv',header = T)  
RHD <- RHD[,c(3,5,7,9,11,13,14,15,16,17)]


summary(RHD)


RHD_NUMBER <- subset(RHD,(RHD$year==1990|RHD$year==2019) & 
                     RHD$age_name=='All ages' & 
                     RHD$metric_name== 'Number')

RHD_NUMBER$val <- round(RHD_NUMBER$val,0)  ###
RHD_NUMBER$lower <- round(RHD_NUMBER$lower,0)###
RHD_NUMBER$upper <- round(RHD_NUMBER$upper,0) ###
RHD_NUMBER$Num_1990 <- paste(RHD_NUMBER$lower,RHD_NUMBER$upper,sep = '-') ## 95%UI
RHD_NUMBER$Num_1990 <- paste(RHD_NUMBER$Num_1990,')',sep = '')  ##95%UI
RHD_NUMBER$Num_1990 <- paste('(',RHD_NUMBER$Num_1990,sep = '')  ##95%UI
RHD_NUMBER$Num_1990 <- paste(RHD_NUMBER$val,RHD_NUMBER$Num_1990,sep = ' ')
write.csv(RHD_NUMBER,file ="NUMBER.CSV")



#DALY#Rate
RHD_RATE <- subset(RHD,(RHD$year==1990|RHD$year==2019)&
                      RHD$age_name=='Age-standardized' &
                      RHD$metric_name== 'Rate')


RHD_RATE$val <- round(RHD_RATE$val,3)  
RHD_RATE$lower <- round(RHD_RATE$lower,3)
RHD_RATE$upper <- round(RHD_RATE$upper,3) ###
RHD_RATE$ASMR_1990 <- paste(RHD_RATE$lower,RHD_RATE$upper,sep = '-') ##
RHD_RATE$ASMR_1990 <- paste(RHD_RATE$ASMR_1990,')',sep = '')  ##
RHD_RATE$ASMR_1990 <- paste('(',RHD_RATE$ASMR_1990,sep = '')  ##
RHD_RATE$ASMR_1990 <- paste(RHD_RATE$val,RHD_RATE$ASMR_1990,sep = ' ')
write.csv(RHD_RATE,file ="RATE.CSV")



####joinpoint:AAPC#####


###########发病#######
AAPC <- subset(RHD, RHD$age_name=='Age-standardized' & 
                 RHD$metric_name== 'Rate' &
                 RHD$measure_name=='Incidence'
                 & RHD$sex_name== 'Both'
               )
AAPC$SE <- (AAPC$upper-AAPC$lower)/(1.96*2)   ##计算SE的值
AAPC <- AAPC[,c(2,7,8,11)] #
AAPC <- AAPC[order(AAPC$location_name,AAPC$year),]

write.csv(AAPC,'joinpoint_in.csv')


###读取数据####
AAPC <- read.table('RHD_IN.aapc.txt',header = T)

AAPC <- AAPC[,c(1,6,7,8)]
names(AAPC)[2:4] <- c('val','lower','upper')
######再次处理数据####
AAPC$AAPC <- paste(AAPC$lower,AAPC$upper,sep = ' - ')
AAPC$AAPC <- paste(AAPC$AAPC,')',sep = '')  ##95            
AAPC$AAPC <- paste('(',AAPC$AAPC,sep = '')  
AAPC$AAPC <- paste(AAPC$val,AAPC$AAPC,sep = ' ')
write.csv(AAPC,'AAPC_in.csv')



###########患病#######
AAPC <- subset(RHD, RHD$age_name=='Age-standardized' & 
                 RHD$metric_name== 'Rate' &
                 RHD$measure_name=='Prevalence'
               & RHD$sex_name== 'Both'
)
AAPC$SE <- (AAPC$upper-AAPC$lower)/(1.96*2)   ##计算SE的值
AAPC <- AAPC[,c(2,7,8,11)] #
AAPC <- AAPC[order(AAPC$location_name,AAPC$year),]

write.csv(AAPC,'joinpoint_PR.csv')


###读取数据####
AAPC <- read.table('RHD_PR.aapc.txt',header = T)

AAPC <- AAPC[,c(1,6,7,8)]
names(AAPC)[2:4] <- c('val','lower','upper')
######再次处理数据#### 
AAPC$AAPC <- paste(AAPC$lower,AAPC$upper,sep = ' - ')
AAPC$AAPC <- paste(AAPC$AAPC,')',sep = '')          
AAPC$AAPC <- paste('(',AAPC$AAPC,sep = '')  
AAPC$AAPC <- paste(AAPC$val,AAPC$AAPC,sep = ' ')
write.csv(AAPC,'AAPC_PR.csv')

###########死亡#######
AAPC <- subset(RHD, RHD$age_name=='Age-standardized' & 
                 RHD$metric_name== 'Rate' &
                 RHD$measure_name=='Deaths'& 
                 RHD$sex_name== 'Both')
AAPC$SE <- (AAPC$upper-AAPC$lower)/(1.96*2)   ##计算SE的值
AAPC <- AAPC[,c(2,7,8,11)] #
AAPC <- AAPC[order(AAPC$location_name,AAPC$year),]

write.csv(AAPC,'joinpoint_DE.csv')




###读取数据####
AAPC <- read.table('RHD_DE.aapc.txt',header = T)

AAPC <- AAPC[,c(1,6,7,8)]
names(AAPC)[2:4] <- c('val','lower','upper')
######再次处理数据####
AAPC$AAPC <- paste(AAPC$lower,AAPC$upper,sep = ' - ')
AAPC$AAPC <- paste(AAPC$AAPC,')',sep = '')  ##95         
AAPC$AAPC <- paste('(',AAPC$AAPC,sep = '')  
AAPC$AAPC <- paste(AAPC$val,AAPC$AAPC,sep = ' ')
write.csv(AAPC,'AAPC_DE.csv')


##########daly#######
AAPC <- subset(RHD, RHD$age_name=='Age-standardized' & 
                 RHD$metric_name== 'Rate' &
                 RHD$measure_name=='DALYs (Disability-Adjusted Life Years)'& 
                 RHD$sex_name== 'Both')
AAPC$SE <- (AAPC$upper-AAPC$lower)/(1.96*2)   ##计算SE的值
AAPC <- AAPC[,c(2,7,8,11)] #
AAPC <- AAPC[order(AAPC$location_name,AAPC$year),]

write.csv(AAPC,'joinpoint_DALY.csv')


###读取数据####
AAPC <- read.table('RHD_DALY.aapc.txt',header = T)

AAPC <- AAPC[,c(1,6,7,8)]
names(AAPC)[2:4] <- c('val','lower','upper')
######再次处理数据####
AAPC$AAPC <- paste(AAPC$lower,AAPC$upper,sep = ' - ')
AAPC$AAPC <- paste(AAPC$AAPC,')',sep = '')  ##95         
AAPC$AAPC <- paste('(',AAPC$AAPC,sep = '')  
AAPC$AAPC <- paste(AAPC$val,AAPC$AAPC,sep = ' ')
write.csv(AAPC,'AAPC_DALY.csv')

