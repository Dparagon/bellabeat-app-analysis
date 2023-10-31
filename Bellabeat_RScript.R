                            # COURSERA Case Study 2 (Bellabeat) #

# Loading libraries
library(tidyverse)
library(janitor)
library(skimr)

# DATA PROCESS

#-Importing data
setwd("D:/R Activity/COURSERA Case Study 2")

activity <- read_csv("dailyActivity_merged.csv")
sleep <- read_csv("sleepDay_merged.csv")
weight <- read_csv("weightLogInfo_merged.csv")
#- Overview of each data
skim_without_charts(activity)
skim_without_charts(sleep)
skim_without_charts(weight)

View(activity)
View(sleep)
View(weight)
#- Removing duplicates
activity[!duplicated(activity),]
sleep[!duplicated(sleep),]
weight[!duplicated(weight),]
#- Converting Dates to date format
activity <- activity %>% 
            mutate_at(vars(ActivityDate), as.Date, format = "%m/%d/%y")
sleep <- sleep %>% 
         mutate_at(vars(SleepDay), as.Date, format = "%m/%d/%y")
weight <- weight %>% 
          mutate_at(vars(Date), as.Date, format = "%m/%d/%y")
#- Renaming Dates columns to Date
activity <- activity %>% 
            rename(Date = ActivityDate)
sleep <- sleep %>% 
         rename(Date = SleepDay)

View(activity)
View(sleep)
View(weight)
#- Data merging
bellabeat_data <- activity %>% 
                  left_join(sleep, by = c("Id", "Date")) %>% 
                  left_join(weight, by = c("Id", "Date"))
#- Creating column for Weekday
Weekday <- wday(bellabeat_data$Date, label = T, abbr = F)
bellabeat_data <- bellabeat_data %>% 
                  cbind(Weekday)
#- Overview of merged data
View(bellabeat_data)
skim_without_charts(bellabeat_data)
str(bellabeat_data)

# DATA SAVING
write_csv(bellabeat_data, "bellabeat_data")

# DATA ANALYSIS 

#- Statistical summary of data
summary(bellabeat_data)
#- Average Minutes of Activeness 
ActivenessAvgMins <- bellabeat_data %>% 
                     group_by(Id) %>% 
                     summarize(VeryActiveAvg = mean(VeryActiveMinutes),
                               FairlyActiveAvg = mean(FairlyActiveMinutes),
                               LightlyActiveAvg = mean(LightlyActiveMinutes), 
                               SedentaryActiveAvg =  mean(SedentaryMinutes))
print(ActivenessAvgMins)
#- Distance Activeness 
DistanceActiveness <- bellabeat_data %>% 
                      group_by(Id) %>% 
                      summarize(TotalDistance = sum(TotalDistance),
                                VeryActiveDistance = sum(VeryActiveDistance), 
                                ModeratelyActiveDistance = sum(ModeratelyActiveDistance),
                                LightActiveDistance = sum(LightActiveDistance),
                                SedentaryActiveDistance = sum(SedentaryActiveDistance))
print(DistanceActiveness)
#- Total Time Awake in Bed
TimeAwakeInBed <- bellabeat_data %>% 
                  select(Id, Date, TotalTimeInBed, TotalMinutesAsleep) %>% 
                  mutate(TotalTimeAwakeInBed = TotalTimeInBed - TotalMinutesAsleep)
print(TimeAwakeInBed)
#- Average Steps and Calories Burned by Each Person
AvgSteps_Calories <- bellabeat_data %>% 
                      group_by(Id) %>% 
                      select(Id, TotalSteps, Calories) %>% 
                      summarize(AvgSteps = mean(TotalSteps), Calories = sum(Calories)) %>% 
                      arrange(-AvgSteps)
print(AvgSteps_Calories)

# DATA VISUALIZATION
  
#- Sedentary Time VS Calories Burned
 ggplot(data = bellabeat_data) +
  geom_point(mapping = aes(x = Calories, y = SedentaryMinutes)) +
   labs(title = "CALORIES BURNED vs SEDENTARY TIME", x = "Calories Burned", y = "Sedentary Minutes") +
    theme(axis.title = element_text(face = "bold", colour = "brown"),
          plot.title = element_text(face = "bold", size = 12),
          axis.line = element_line(colour = "black"),
          panel.grid = element_blank())
#- Very Active Time VS Calories Burned
 ggplot(data = bellabeat_data) +
  geom_point(mapping = aes(x = Calories, y = VeryActiveMinutes)) +
   labs(title = "CALORIES BURNED vs VERY ACTIVE TIME", x = "Calories Burned", y = "Very Active Minutes") +
    theme(axis.title = element_text(face = "bold", colour = "brown"),
          plot.title = element_text(face = "bold", size = 12),
          axis.line = element_line(colour = "black"),
          panel.grid = element_blank())
#- Total Steps VS Total Distance
 ggplot(data = bellabeat_data) +
  geom_point(mapping = aes(x = TotalSteps, y = TotalDistance)) +
   labs(title = "TOTAL STEPS vs TOTAL DISTANCE", x = "Total Steps", y ="Total Distance") +
    theme(axis.title = element_text(face = "bold", colour = "brown"),
         plot.title = element_text(face = "bold", size = 12),
         axis.line = element_line(colour = "black"),
         panel.grid = element_blank())
#- Days of the Week VS Total Steps
 ggplot(data = bellabeat_data) +
   geom_col(mapping = aes(x = Weekday, y = TotalSteps)) +
    labs(title = "TOTAL STEPS IN DAYS OF THE WEEK", x = "Days of the Week", y = "Total Steps") +
     theme(axis.title = element_text(face = "bold", colour = "brown"),
         plot.title = element_text(face = "bold", size = 12),
         axis.line = element_line(colour = "black"),
         axis.text = element_text(colour = "black"),
         axis.ticks.x = element_blank(),
         panel.grid = element_blank())
#- Minutes in Bed VS Minutes Asleep
 ggplot(data = bellabeat_data) +
  geom_point(mapping = aes(x = TotalTimeInBed , y = TotalMinutesAsleep)) +
   labs(title = "TOTAL TIME IN BED vs TOTAL TIME ASLEEP", x = "Total Time In Bed", y = "Total Minutes Asleep") +
    theme(axis.title = element_text(face = "bold", colour = "brown"),
         plot.title = element_text(face = "bold", size = 12),
         axis.line = element_line(colour = "black"),
         axis.text = element_text(colour = "black"),
         panel.grid = element_blank(),
         panel.background = element_rect(fill = "violet") )
#- Total Distance VS Very Active Distance
 ggplot(data = bellabeat_data) +
  geom_point(mapping = aes(x = TotalDistance, y = VeryActiveDistance)) +
   theme(axis.title = element_text(face = "bold", colour = "brown"),
         plot.title = element_text(face = "bold", size = 12),
         axis.line = element_line(colour = "black"),
         axis.text = element_text(colour = "black"),
         panel.grid = element_blank()) +
    labs(title = "TOTAL DISTANCE vs VERY ACTIVE DISTANCE", x = "Total Distance", y = "Very Active Distance")
#- Total Distance VS Light Active Distance
 ggplot(data = bellabeat_data) +
   geom_point(mapping = aes(x = TotalDistance, y = LightActiveDistance)) +
   theme(axis.title = element_text(face = "bold", colour = "brown"),
         plot.title = element_text(face = "bold", size = 12),
         axis.line = element_line(colour = "black"),
         axis.text = element_text(colour = "black"),
         panel.grid = element_blank()) +
   labs(title = "TOTAL DISTANCE vs LIGHT ACTIVE DISTANCE", x = "Total Distance", y = "Light Active Distance")
 