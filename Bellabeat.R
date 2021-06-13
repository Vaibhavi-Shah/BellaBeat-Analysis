#There are various CSV files. Try exploring them one by one. It is soon found out there are three main datasets.
#1. dailyActivity_merged 2. sleepDay_merged 3. weightLogInfo_merged
#Start with importing necessary libraries and store the CSV files in a variable.


library(tidyverse)
library(dplyr)
library(lubridate)
getwd()
setwd("F:/Vaibhavi/Google Certificate/Project/Fitabase Data 4.12.16-5.12.16")

daily_activity <- read.csv("dailyActivity_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
weight_log <- read.csv("weightLogInfo_merged.csv")

glimpse(daily_activity)
summary(daily_activity)

#By glancing through the CSV file you note that Date and Time are in the same column. 
#We can split them into seperate columns to make it more clean.

sleep_edit <- sleep_day%>%
  separate(SleepDay,c("Date","Time")," ")
  
view(sleep_edit)

#Now we can say that the Daily Activity dataset has most of the important parameters except info about Sleep. 
#We may now look to combine two dataset.

combine_sleep <- full_join(daily_activity,sleep_edit,by = c("Id"="Id","ActivityDate"="Date"))
view(combine_sleep)
summary(combine_sleep)
glimpse(combine_sleep)

#We can see that it has many missing values.
#So let's have only the records for which all data is available.
#Instead of removing the null records, we can simply use leftjoin().

only_sleep <- left_join(sleep_edit,daily_activity,by=c("Id"="Id","Date"="ActivityDate"))
glimpse(only_sleep)
view(only_sleep)

only_sleep$nonSleep_hrs = (only_sleep$SedentaryMinutes+only_sleep$LightlyActiveMinutes+
                             only_sleep$FairlyActiveMinutes+only_sleep$VeryActiveMinutes)/60
only_sleep$total = only_sleep$nonSleep_hrs + (only_sleep$TotalMinutesAsleep/60)
only_sleep


#Going through the dataset we find that there are some hours where the data is greater than 24 which is clearly wrong data.
#So it is better to remove those data.
only_sleep_clean <- only_sleep[!(only_sleep$total>24),]
view(only_sleep_clean)

#Now that our data is clean we may proceed to analyse it.
#For the sake of simplicity we categorize the people into two - Heavy User and Light User based amount on their activity.
#We assume people with more than 1 hour of 'active' and 'very active' as heavy users and others as light users.

only_sleep_clean$heavy_mins = only_sleep_clean$FairlyActiveMinutes + only_sleep_clean$VeryActiveMinutes
only_sleep_clean$lite_mins = only_sleep_clean$SedentaryMinutes + only_sleep_clean$LightlyActiveMinutes + 
                             only_sleep_clean$TotalMinutesAsleep
only_sleep_clean$type = case_when(only_sleep_clean$heavy_mins > 60 ~ "Heavy User", TRUE ~ "Light User",)
view(only_sleep_clean)

#The next stage is visualising the data. For this we'll use ggplot2 library
library(ggplot2)
ggplot(only_sleep_clean,aes(x=type,fill=type))+geom_bar()
ggplot(only_sleep_clean,aes(x=type,y=Calories,fill=type))+geom_boxplot()+labs(title='Calories burned by Type')

only_sleep_clean %>%
  summarise(Id,Calories,type)%>%
  ggplot(aes(Calories,fill=type))+geom_histogram(bins = 50)

only_sleep_clean %>%
  summarise(Id,Calories,type)%>%
  ggplot(aes(Calories))+geom_histogram(aes(fill=..count..),bins=50)

ggplot(only_sleep_clean,aes(x=TotalDistance,y=Calories)) +geom_jitter() +geom_smooth()

only_sleep_clean %>% 
  summarise(Id , TotalSteps, type) %>% 
  ggplot(aes(TotalSteps,fill=type)) + geom_histogram(bins=30)

ggplot(only_sleep_clean,aes(TotalDistance))+geom_histogram(binwidth = 5,color='black',fill='pink',size=1)+
  facet_wrap(~type)

only_sleep_clean %>% 
  summarise(step_cat = factor(case_when(TotalSteps < 4000 ~ 'Less than 4k',
                                        TotalSteps >=4000 & TotalSteps<8000 ~ '4k to 8k',
                                        TotalSteps >=8000 & TotalSteps<11000 ~ '8k to 11k',
                                        TotalSteps >= 11000 ~ 'Greater than 11k'),levels = c('Less than 4k','4k to 8k','8k to 11k','Greater than 11k')),Calories,type) %>% 
  ggplot(aes(x=step_cat,y=Calories)) + geom_boxplot(aes(fill = step_cat)) + facet_wrap(~type) +
  xlab("No. of Steps") +
  labs(title = "No. of Steps Vs Calories Burned")+
  scale_fill_brewer(palette= 'Pastel1')

ggplot(only_sleep_clean,aes(x=type,y=TotalMinutesAsleep/60))+geom_boxplot(color='black',fill='tomato',alpha=.4)

only_sleep_clean %>% 
  group_by(Id) %>% 
  summarise(Id,type,sleep_type=factor(case_when((TotalMinutesAsleep/60) < 6 ~ 'Inadequate Sleep',
                                                (TotalMinutesAsleep/60)>=6 & (TotalMinutesAsleep/60) < 8 ~ 'Adequate Sleep',
                                                (TotalMinutesAsleep/60)>=8 ~ 'Oversleep')),
            
  ) %>% 
  summarise(Id,sleep_type,type, .groups = 'drop') %>% 
  ggplot(aes(sleep_type)) +
  geom_bar(position = "dodge", aes(fill=type))+
  theme(legend.position="bottom",text = element_text(size = 20),plot.title = element_text(hjust = 0.5))

only_sleep_clean %>% 
  group_by(Id) %>% 
  summarise(Id,type,sleep_type=factor(case_when((TotalMinutesAsleep/60) < 6 ~ 'Inadequate Sleep',
                                                (TotalMinutesAsleep/60)>=6 & (TotalMinutesAsleep/60) < 8 ~ 'Adequate Sleep',
                                                (TotalMinutesAsleep/60)>=8 ~ 'Oversleep')),
            
  ) %>% 
  summarise(Id,sleep_type,type, .groups = 'drop') %>% 
  ggplot(aes(type)) +
  geom_bar(position="fill", aes(fill=sleep_type))+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position="right",text = element_text(size = 10),plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = 'Pastel1')

only_sleep_clean$sleep_hrs <- (only_sleep_clean$TotalMinutesAsleep)/60

only_sleep_clean %>% 
  ggplot(aes(x=type,y=sleep_hrs,fill=type)) + geom_violin(scale='area',alpha=.5) + geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = .5,color= 'black') +scale_fill_brewer(palette = 'Pastel2')+
  ylab("Sleep in Hrs") +
  xlab("Type") +
  theme(legend.position = 'none') +
  labs(title = "Type Vs Hrs")

only_sleep_clean$Day = weekdays(as.Date(only_sleep_clean$Date,'%m/%d/%Y'))
only_sleep_clean$Day = factor(only_sleep_clean$Day,levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

only_sleep_clean %>% 
  ggplot(aes(x=Day,y = Calories,fill=type)) + geom_boxplot(outlier.colour = 'red', outlier.shape = 8)+
  theme(legend.position = 'none') +
  stat_summary(fun.y=mean, geom="point", shape=21, size=4)

