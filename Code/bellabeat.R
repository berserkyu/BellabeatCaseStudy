# this is the R script for the case study in capstone project for Google Data Analytics Certificate
# author : Phua Jun Yu
# creation : 26Jun 2023
# last update : 26Jun 2023
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

dailyActivity_raw = read.csv(r'[D:\School\Reading\Google Data Analytic Cert\Case Study\Data\Fitabase Data 4.12.16-5.12.16\dailyActivity_merged.csv]')

fileNames <- c('dailyActivity_merged.csv',
              'heartrate_seconds_merged.csv',
              'minuteCaloriesNarrow_merged.csv',
              'minuteCaloriesWide_merged.csv',
              'minuteIntensitiesNarrow_merged.csv',
              'minuteIntensitiesWide_merged.csv',
              'minuteMETsNarrow_merged.csv',
              'minuteSleep_merged.csv',
              'minuteStepsNarrow_merged.csv',
              'minuteStepsWide_merged.csv',
              'sleepDay_merged.csv',
              'weightLogInfo_merged.csv'
              )
fileBaseDir <- r'[D:\School\Reading\Google Data Analytic Cert\Case Study\Data\Fitabase Data 4.12.16-5.12.16\]'

# checking for empty/missing values using is.na function
for (fileName in fileNames){
  file <- ""
  file <- paste(fileBaseDir ,fileName,sep = "")
  data_raw <- read.csv(file)
  noNa <- which(is.na(data_raw))
  print(paste("The file ",fileName," contains ",length(noNa)," empty values"))
}

col_to_skip <- c('Id',
                 'ActivityDate',
                 'Time',
                 'ActivityHour',
                 'ActivityMinute',
                 'date',
                 'logId',
                 'SleepDay',
                 'IsManualReport')
i <- 0

fileNames <- c('dailyActivity_merged.csv',
               'heartrate_seconds_merged.csv',
               'minuteCaloriesNarrow_merged.csv',
               'minuteCaloriesWide_merged.csv',
               'minuteIntensitiesNarrow_merged.csv',
               'minuteIntensitiesWide_merged.csv',
               'minuteMETsNarrow_merged.csv',
               'minuteSleep_merged.csv',
               'minuteStepsNarrow_merged.csv',
               'minuteStepsWide_merged.csv',
               'sleepDay_merged.csv'
)

# checking for potential outliers
for (fileName in fileNames){
  file <- ""
  file <- paste(fileBaseDir ,fileName,sep = "")
  data_raw <- read.csv(file)
  cols <- colnames(data_raw)
  for(col in cols){
    allNumeric <- TRUE
    if(col %in% col_to_skip) next
    # if it is all numeric(quantitative data)
    t <- is.numeric(data_raw[[col]])
    t1 <-  is.numeric(data_raw[col])
    for(no in data_raw[[col]]){
      if(!is.numeric(no)){
        allNumeric <- FALSE
      }
    }

    if(allNumeric){
      # look for potential outliers
      x <- data_raw[[col]]
      Qs <- quantile(x, prob=c(.25,.5,.75), type=1)
      IQR = Qs['75%'] - Qs['25%']
      upper_lim = Qs['75%'] + IQR
      lower_lim = Qs['25%'] - IQR
      noOutliers <- sum(x<lower_lim | x>upper_lim)
      # print(paste("Upperlim: ",upper_lim,", lowerLim: ",lower_lim))
      if(noOutliers != 0)print(paste("In file ",fileName," column ",col," contains ",noOutliers," potential outliers"))
      else print("no potential outliers")
    }else{
      print("not all numeric")
    }
    # else it is qualitative data and we do nothing to them for now
  }
}

check_numeric <- function(dataDf,colNum){
  for(col in col_numeric){
    allNumeric <- TRUE
    allPos <- TRUE
    for(no in data[[col]]){
      if(!is.numeric(no)){
        allNumeric <- FALSE
      }
      if(sum(data[[col]]<0)>0){
        allPos <- FALSE
      }
    }
    if(!allNumeric){
      print(paste(col, " - Not Numeric"))
    }else{
      print(paste(col, " - Numeric"))
    }
    if(allPos){
      print(paste(col, " - All pos"))
    }else{
      print(paste(col, " - Not all pos"))
    }
  }
}

# check for format consistency and data type
file <- 'dailyActivity_merged.csv'
data <- read.csv(paste(fileBaseDir,file,sep=""))
colnames(data)

# for dailyActivity, check if the columns are all numeric 
col_numeric <- c('TotalSteps','TotalDistance','TrackerDistance','LoggedActivitiesDistance',
            'VeryActiveDistance','ModeratelyActiveDistance','LightActiveDistance',
            'SedentaryActiveDistance','VeryActiveMinutes','FairlyActiveMinutes',
            'LightlyActiveMinutes','SedentaryMinutes','Calories')
#check_numeric(dataDf=data,colNum=col_numeric)


file <- 'heartrate_seconds_merged.csv'
data <- read.csv(paste(fileBaseDir,file,sep=""))
colnames(data)
col_numeric <- c('Value')
#check_numeric(dataDf=data,colNum=col_numeric)


file <- 'minuteCaloriesWide_merged.csv'
data <- read.csv(paste(fileBaseDir,file,sep=""))
colnames(data)
col_numeric <- c('Value')
#check_numeric(dataDf=data,colNum=col_numeric)


showPotentialOutliers <- function(fileName,fileBaseDir){
  
  file <- paste(fileBaseDir ,fileName,sep = "")
  print(paste("File : ",file))
  sink(paste(file,"_potentialOutliers.txt"))
  data_raw <- read.csv(file)
  cols <- colnames(data_raw)
  
  for(col in cols){
    noOutliers <- 0
    allNumeric <- TRUE
    if(col %in% col_to_skip) next
    # if it is all numeric(quantitative data)
    for(no in data_raw[[col]]){
      if(!is.numeric(no)){
        allNumeric <- FALSE
        break
      }
    }
    # there will be no outliers in none-numeric values
    if(!allNumeric) next
    
    if(allNumeric){
      print(paste("potential outliers in ",col," : "))
      # look for potential outliers
      x <- data_raw[[col]]
      Qs <- quantile(x, prob=c(.25,.5,.75), type=1)
      IQR = Qs['75%'] - Qs['25%']
      upper_lim = Qs['75%'] + IQR
      lower_lim = Qs['25%'] - IQR
      cat(paste(col,"\n"))
      cat(paste("Upperlim: ",upper_lim,", lowerLim: ",lower_lim,"\n"))
      noOutliers = sum(x<lower_lim) + sum(x>upper_lim)
      for(val in x){
        if(val<lower_lim || val>upper_lim){
          cat(paste(" - ",val,"\n"))
        }
      }
      
      if(noOutliers != 0)print(paste("column ",col," contains ",noOutliers," potential outliers"))
      else print("no potential outliers")
    }
  }
  sink()
}

# judging potential outliers
fileNames <- c('dailyActivity_merged.csv',
               'heartrate_seconds_merged.csv',
               'minuteCaloriesNarrow_merged.csv',
               'minuteCaloriesWide_merged.csv',
               'minuteIntensitiesNarrow_merged.csv',
               'minuteIntensitiesWide_merged.csv',
               'minuteMETsNarrow_merged.csv',
               'minuteSleep_merged.csv',
               'minuteStepsNarrow_merged.csv',
               'minuteStepsWide_merged.csv',
               'sleepDay_merged.csv'
)
for(fileName in fileNames){
  showPotentialOutliers(fileName,fileBaseDir)
}



# now investigate outliers that seem unreasonable
# sedentaryMinute in dailyActivity_merged.csv 
dailyActivity_raw = read.csv(r'[D:\School\Reading\Google Data Analytic Cert\Case Study\Data\Fitabase Data 4.12.16-5.12.16\dailyActivity_merged.csv]')
glimpse(dailyActivity_raw)
colnames(dailyActivity_raw)
dailyActivity_raw[['SedentaryMinutes']]
mean(dailyActivity_raw[['SedentaryMinutes']])
dailyActivity_raw[['ActivityDate']]
# look for sedentary minutes less than 200, which seems unreasonable
for(sen in dailyActivity_raw[['SedentaryMinutes']]){
  if(sen < 200){
    print(sen)
  }
}
# check logical consistency of minutes data -- check for total minutes recorded
totalMinutes <- dailyActivity_raw[['SedentaryMinutes']] + dailyActivity_raw[['LightlyActiveMinutes']] + dailyActivity_raw[['FairlyActiveMinutes']]+ dailyActivity_raw[['VeryActiveMinutes']]
mean(totalMinutes)
# found that some data record contains unreasonable amount of recorded activity minute
# this indicates that some data record might not reflect the user's activity on that day
# now we combine with sleep data to see if it is just that the person was sleeping all day (weird but not impossible)
# it might or might not because the person has a long sleeping time on that day, we have to check the sum of activity time and time in bed to see

# read sleep data
sleep_raw <- read.csv(r'[D:\School\Reading\Google Data Analytic Cert\Case Study\Data\Fitabase Data 4.12.16-5.12.16\sleepDay_merged.csv]')

# split & rename column for data merging
sleep_sep <- separate(sleep_raw,"SleepDay",into=c("ActivityDate","Time","Clock"),sep= " +")  

# merge activity and sleep data
dailyActivity_sleep_merged <- merge(dailyActivity_raw,sleep_sep,by=c("Id","ActivityDate"))

# check if successfully merged
head(dailyActivity_sleep_merged)

# compute the total time in record 
sumTime <- dailyActivity_sleep_merged[['SedentaryMinutes']] + dailyActivity_sleep_merged[['LightlyActiveMinutes']] + dailyActivity_sleep_merged[['FairlyActiveMinutes']]+ dailyActivity_sleep_merged[['VeryActiveMinutes']] + dailyActivity_sleep_merged[['TotalTimeInBed']]

dailyActivity_sleep_merged <- dailyActivity_sleep_merged %>%
  cbind(sumTime)
# average of the total time in record 
mean(sum_time) 
# average is 1430.891 minutes, which is reasonably 23 hours+ 
# still, we can see that for some records, clearly a huge part of the days was not recorded
# check how many records has time in record less than 1000 minutes
sum(sum_time<900)
# remove records with time less than 1000 minutes
dailyActivity_sleep_merged <- dailyActivity_sleep_merged %>% filter(sumTime>1000)

# now check for unusually low values of calories expenditure
sum(dailyActivity_sleep_merged[["Calories"]]<800)
# the number of unusually low values of calories expenditure is not found(was removed in previous steps)

colnames(dailyActivity_sleep_merged)
mean(dailyActivity_sleep_merged[["TotalTimeInBed"]])




# data cleaning done, starts analysis now
# first analyze activity data
# see the average time of different active levels
mean(dailyActivity_sleep_merged[["SedentaryMinutes"]])
mean(dailyActivity_sleep_merged[["LightlyActiveMinutes"]])
mean(dailyActivity_sleep_merged[["FairlyActiveMinutes"]])
mean(dailyActivity_sleep_merged[["VeryActiveMinutes"]])
# check if there is any relationship between active levels
# from scatter-plot, we see a likely negative linear relationship between sedentary minutes and lightly active minutes
lm_fit <- lm(SedentaryMinutes ~ LightlyActiveMinutes, data=dailyActivity_sleep_merged)
lm_fit

ggplot(data = dailyActivity_sleep_merged) +
  geom_point(mapping = aes(x=SedentaryMinutes,y=LightlyActiveMinutes,color='red')) +
  geom_abline(slope=lm_fit$coefficients[2],
              intercept=lm_fit$coefficients[1],
              color="blue")
# ** possible insight : negative correlation between sedentary minute and lightly active minute

# check if there is any relationship between activity and sleep quality
# compute new metric to assess sleep quality
# a basic assumption : a person has better sleep quality the sooner he/she falls asleep and the quicker he/she can get out from the bed after waking up
# also does not wake up at night (only 1 sleep record)
# the metric is sleep quality index = TimeAsleep/(no of sleep recordxTimeInBed)
head(dailyActivity_sleep_merged,10)
sleepQuality <- dailyActivity_sleep_merged[["TotalMinutesAsleep"]] /dailyActivity_sleep_merged[["TotalTimeInBed"]]
sleepQuality
dailyActivity_sleep_merged <- dailyActivity_sleep_merged %>%
  cbind(sleepQuality)

ggplot(data = dailyActivity_sleep_merged) +
  geom_point(mapping = aes(x=TotalDistance,y=sleepQuality))


ggplot(data = dailyActivity_sleep_merged) +
  geom_point(mapping = aes(x=SedentaryMinutes,y=sleepQuality))

# does not observe apparent relationship between sleep quality and activity level
# but does found some poeple with bad sleeping quality

ggplot(data=dailyActivity_sleep_merged)+
  geom_point(mapping=aes(x=VeryActiveMinutes,y=Calories,color='red'))  

ggplot(data=dailyActivity_sleep_merged)+
  geom_point(mapping=aes(x=FairlyActiveMinutes,y=Calories,color='green')) 

ggplot(data=dailyActivity_sleep_merged)+
  geom_point(mapping=aes(x=SedentaryMinutes,y=Calories,color='blue')) 

ggplot(data=dailyActivity_sleep_merged)+
  geom_point(mapping=aes(x=LightlyActiveMinutes,y=Calories,color='yellow')) 

ggplot(data=dailyActivity_sleep_merged)+
  geom_point(mapping=aes(x=TotalDistance,y=Calories),color='red') 

ggplot(data=dailyActivity_sleep_merged)+
  geom_point(mapping=aes(x=TotalSteps,y=Calories),color='red') +
  geom_smooth(mapping=aes(x=TotalSteps,y=Calories),method="lm" )

# ** potential insight : (apparent linear relationship) greater activity level, greater calories expenditure
colnames(dailyActivity_sleep_merged)

# check for activity & sleep trend in a week
library(lubridate)
# first need to compute records' day in a week
dayOfWeek <- wday(mdy(dailyActivity_sleep_merged$ActivityDate),week_start=1)
dayOfWeek
dailyActivity_sleep_merged <- dailyActivity_sleep_merged %>%
  cbind(dayOfWeek)
dailyActivity_sleep_merged[c("ActivityDate","dayOfWeek")]
# then aggregate records of same day in a week
colnames(dailyActivity_sleep_merged)

lm_fit_weekday <- lm(SedentaryMinutes ~ LightlyActiveMinutes, data=dailyActivity_sleep_merged[dailyActivity_sleep_merged$dayOfWeek <6,])
lm_fit_weekday

lm_fit_weekend <- lm(SedentaryMinutes ~ LightlyActiveMinutes, data=dailyActivity_sleep_merged[dailyActivity_sleep_merged$dayOfWeek >5,])

lm_fit_weekday
lm_fit_weekend

days <- dailyActivity_sleep_merged %>% group_by(dayOfWeek) %>% summarize(
  avgSedentaryMinutes=mean(SedentaryMinutes),
  avgLightlyActiveMinutes=mean(LightlyActiveMinutes),
  avgFairlyActiveMinutes=mean(FairlyActiveMinutes),
  avgVeryActiveMinutes=mean(VeryActiveMinutes),
  avgTotalMinutesAsleep=mean(TotalMinutesAsleep),
  avgSleepQuality=mean(sleepQuality),
  avgCalories=mean(Calories),
  avgTotalDistance=mean(TotalDistance),
  avgTotalSteps=mean(TotalSteps),
  .groups='drop')
days


colnames(days)
# check for trends
# possible insight : sedentary minute on average is higher on weekday than on weekend
# compute average sedentary minute on weekdays

avgSedentaryMinuteWeekday <- mean(days[days$dayOfWeek<6,][["avgSedentaryMinutes"]])
avgSedentaryMinuteWeekEnd <- mean(days[days$dayOfWeek>5,][["avgSedentaryMinutes"]])

avgSedentaryMinuteWeekday -avgSedentaryMinuteWeekEnd 
avgSedentaryMinuteWeekEnd

stringDayOfWeek <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
stringDayOfWeek[days[["dayOfWeek"]]]

days <- days %>% cbind(stringDayOfWeek)
colnames(days)

period <- ifelse(days[["dayOfWeek"]]<6,"weekDay", "weekEnd")
period

colnames(days)
days <- days %>% cbind(period)

ggplot(data=days,aes(x=dayOfWeek,y=avgSedentaryMinutes,fill=period)) +
  geom_bar(stat="identity",size=1.5)+
  geom_segment(aes(x=0.5,xend=5.5,y=avgSedentaryMinuteWeekday,yend=avgSedentaryMinuteWeekday),color='red',size=1)+
  geom_segment(aes(x=5.5,xend=7.5,y=avgSedentaryMinuteWeekEnd,yend=avgSedentaryMinuteWeekEnd),color='blue',size=1)+
  scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

# possible insight : total distance&steps has a weak decreasing trend across the week from monday to sunday except for saturday
ggplot(data=days,aes(x=dayOfWeek,y=avgTotalDistance,fill=period)) +
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
ggplot(data=days,aes(x=dayOfWeek,y=avgTotalSteps,fill=period)) +
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

# possible insight : total minutes asleep on average in higher on weekend than on weekday
ggplot(data=days,aes(x=dayOfWeek,y=avgTotalMinutesAsleep)) +
  geom_bar(stat="identity")

ggplot(data=days,aes(x=dayOfWeek,y=avgSleepQuality)) +
  geom_bar(stat="identity")

avgTotalMinutesAsleepWeekDay <- mean(days[days$dayOfWeek<6,][["avgTotalMinutesAsleep"]])
avgTotalMinutesAsleepWeekEnd <- mean(days[days$dayOfWeek>5,][["avgTotalMinutesAsleep"]])


ggplot(data=days,aes(x=dayOfWeek,y=avgTotalMinutesAsleep,fill=period)) +
  geom_bar(stat="identity",size=1.5)+
  geom_segment(aes(x=0.5,xend=5.5,y=avgTotalMinutesAsleepWeekDay,yend=avgTotalMinutesAsleepWeekDay),color='red',size=1)+
  geom_segment(aes(x=5.5,xend=7.5,y=avgTotalMinutesAsleepWeekEnd,yend=avgTotalMinutesAsleepWeekEnd),color='blue',size=1)+
  scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

colnames(days)

avgLightlyActiveMinutesWeekDay <- mean(days[days$dayOfWeek<6,][["avgLightlyActiveMinutes"]])
avgLightlyActiveMinutesWeekEnd <- mean(days[days$dayOfWeek>5,][["avgLightlyActiveMinutes"]])

avgLightlyActiveMinutesWeekEnd - avgLightlyActiveMinutesWeekDay

gg1 <- ggplot(data=days,aes(x=dayOfWeek,y=avgLightlyActiveMinutes,fill=period)) +
  geom_bar(stat="identity",size=1.5)+
  geom_segment(aes(x=0.5,xend=5.5,y=avgLightlyActiveMinutesWeekDay ,yend=avgLightlyActiveMinutesWeekDay),color='red',size=1)+
  geom_segment(aes(x=5.5,xend=7.5,y=avgLightlyActiveMinutesWeekEnd ,yend=avgLightlyActiveMinutesWeekEnd),color='blue',size=1)+
  scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

gg2 <- ggplot(data=days,aes(x=dayOfWeek,y=avgSedentaryMinutes,fill=period)) +
  geom_bar(stat="identity",size=1.5)+
  geom_segment(aes(x=0.5,xend=5.5,y=avgSedentaryMinuteWeekday,yend=avgSedentaryMinuteWeekday),color='red',size=1)+
  geom_segment(aes(x=5.5,xend=7.5,y=avgSedentaryMinuteWeekEnd,yend=avgSedentaryMinuteWeekEnd),color='blue',size=1)+
  scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

grid.arrange(gg1,gg2,ncol=2)


library(data.table)
# now study about previous days' activities' effect on sleep
shift(dailyActivity_sleep_merged["VeryActiveMinutes"])
prevActivity <- as.numeric(shift(dailyActivity_sleep_merged[["VeryActiveMinutes"]])) + as.numeric(shift(dailyActivity_sleep_merged[["VeryActiveMinutes"]],2))
prevActivity

dailyActivity_sleep_merged <- dailyActivity_sleep_merged %>% cbind(prevActivity)
colnames(dailyActivity_sleep_merged)

ggplot(data=dailyActivity_sleep_merged,aes(x=prevActivity,y=TotalMinutesAsleep)) +
  geom_point()



# done with analyzing activity & sleep data, now move to hourly calories & intensities data to see trend throughout a day
hourCalory <- read.csv(r'[D:\School\Reading\Google Data Analytic Cert\Case Study\Data\Fitabase Data 4.12.16-5.12.16\hourlyCalories_merged.csv]')
hourIntensity<- read.csv(r'[D:\School\Reading\Google Data Analytic Cert\Case Study\Data\Fitabase Data 4.12.16-5.12.16\hourlyIntensities_merged.csv]')
hourStep <- read.csv(r'[D:\School\Reading\Google Data Analytic Cert\Case Study\Data\Fitabase Data 4.12.16-5.12.16\hourlySteps_merged.csv]')

colnames(hourIntensity)
colnames(hourStep)
glimpse(hourIntensity)
glimpse(hourStep)
# merge hourly intensity and steps data 
hourIntensityStep_merged <- merge(hourIntensity,hourStep,by=c("ActivityHour","Id"))
glimpse(hourIntensityStep_merged)


hourIntensityStep_merged_sep <- separate(hourIntensityStep_merged,"ActivityHour",into=c("Date","Time","Clock"),sep=" +")
colnames(hourIntensityStep_merged_sep)
head(hourIntensityStep_merged_sep,10)
# group data by hour to see trend throughout a day
hourIntensityStep_grouped <- hourIntensityStep_merged_sep %>% group_by(Time,Clock) %>% summarize(
  avgTotalIntensity=mean(TotalIntensity),
  avgIntensity=mean(AverageIntensity),
  avgStepTotal=mean(StepTotal),
  .groups='drop'
)
# get subtring of time to indicate hour with numeric value
hourIntensityStep_grouped 
hourIntensityStep_grouped $Time <- gsub("(:([0-9])*)","",hourIntensityStep_grouped $Time)
# add 12 hours to PM values to get a ordinal hour values
hourIntensityStep_grouped 
hourIntensityStep_grouped $Time <- ifelse(hourIntensityStep_grouped $Clock == "PM",as.numeric(hourIntensityStep_grouped $Time)+12,hourIntensityStep_grouped $Time)
hourIntensityStep_grouped 
# arrange grouped data by hour
hourIntensityStep_grouped  <- arrange(hourIntensityStep_grouped ,as.numeric(Time))
hourIntensityStep_grouped 

hourIntensityGraph <- ggplot(data=hourIntensityStep_grouped ,aes(x=as.numeric(Time),y=avgIntensity)) + 
  geom_line(color='red')+
  geom_point() 

hourStepGraph <- ggplot(data=hourIntensityStep_grouped ,aes(x=as.numeric(Time),y=avgStepTotal)) + 
  geom_line(color='red')+
  geom_point() 


library(gridExtra) # To display 2 charts together
grid.arrange(hourIntensityGraph,hourStepGraph,ncol=2)

# potential insight : max steps & intensities at 17:00-19:00
# potential insight : a huge decline in activity (steps count & intensities)  within 1100am-1200pm


# now study about sleep pattern
minuteSleep = read.csv(r'[D:\School\Reading\Google Data Analytic Cert\Case Study\Data\Fitabase Data 4.12.16-5.12.16\minuteSleep_merged.csv]')
colnames(minuteSleep)
glimpse(minuteSleep)
head(minuteSleep,10)
# seprate time and clock 
minuteSleep_sep <- separate(minuteSleep,date,into = c("Date","Time","Clock"),sep=" +")
head(minuteSleep_sep,10)
minuteSleep_sep <- separate(minuteSleep_sep,Time,into = c("Hour","Minute","Second"),sep=":")
head(minuteSleep_sep,10)


# add 12 hours to PM times
minuteSleep_sep$Hour <- ifelse(minuteSleep_sep$Clock=="PM",as.numeric(minuteSleep_sep$Hour)+12,as.numeric(minuteSleep_sep$Hour))

# group by minute
minuteSleep_merged <- minuteSleep_sep %>% group_by(Hour,Minute) %>% summarize(
  avgValue = mean(value),
  .groups='drop'
)

timeStamp <- as.numeric(minuteSleep_merged[["Hour"]])*100 + as.numeric(minuteSleep_merged[["Minute"]])
timeStamp

minuteSleep_merged <- minuteSleep_merged %>% cbind(timeStamp)

p1 <- ggplot(data=minuteSleep_merged,aes(x=timeStamp,y=avgValue)) + 
  geom_line(color='red',size=1) +
  xlim(100,1200) +
  ylim(0,2.0)

p2 <- ggplot(data=minuteSleep_merged,aes(x=timeStamp,y=avgValue)) + 
  geom_line(color='red',size=1) +
  xlim(2100,2400) +
  ylim(0,2.0)

grid.arrange(p2,p1,ncol=2)

 