library(tidyverse)
library(readxl)
library(lubridate)
library(GGally)

oil_sample <- read_excel("Oil Sample Data (All Rigs).xlsx")
feet_data <- read_excel("100ft Data (All Rigs).xlsx")
top_drive <- read_excel("TopDrive Rig List.xlsx")
TVI <- read_excel("TVI Data (All Rigs).xlsx")

colnames(TVI)[2] <- "Serial_Number"
colnames(TVI)[3] <- "StartTime_TVI"
colnames(feet_data)[2] <- "Serial_Number"

join_data <- left_join(feet_data, top_drive, by = c("RigNumber", "Serial_Number"))

join_data$Serial_Number <- as.factor(join_data$Serial_Number)

join_data$`TopDrive Status` <- as.factor(join_data$`TopDrive Status`)

# Summary Statistics ------------------------------------------------------
summary(join_data)
ggpairs(join_data[,-c(1:4,7,13,14,15)], cardinality_threshold = 16, aes(colour = `TopDrive Status`))
ggpairs(join_data[,c(7:12,18)], aes(color = `TopDrive Status`)) + 
  scale_color_manual(values = c("darkgreen", "red")) +
  scale_fill_manual(values = c("darkgreen", "red"))
summary(join_data[,c(7:12,16)])


#Data seems to be pretty clean. The NAs on Failure date and other similar variables match up with the 
#number of active observations which makes sense


# Times -------------------------------------------------------------------

ggplot(dat = join_data, aes(x = StartTime, y = `Average RPM`, col = Serial_Number)) +
  geom_point()

join_data$StartTime <- ymd_hms(join_data$StartTime)

join_data %>% mutate(day = as.Date(StartTime)) %>% 
  group_by(day) %>% summarize(maxRPM = max(`Average RPM`))
#This is the max average RPM of a drill (dig?) per day for all of the rigs. Might be worth looking into to
#see general trends

join_data %>% mutate(day = as.Date(StartTime)) %>% 
  group_by(Serial_Number,day) %>% 
  summarize(maxRPM = max(`Average RPM`)) %>% 
  ggplot(aes(x = day, y = maxRPM, col = Serial_Number)) +
  geom_point(alpha = .5)
#This takes every serial number and shows their maximum average RPM during a dig during the day
#this will let us see a general trend of the maximum workload that is put on a top_drive.
#One important thing to note is that this does not tell us how deep and how long these maximum average RPMs are lasting

join_data %>% mutate(day = as.Date(StartTime)) %>% 
  group_by(Serial_Number,day,`TopDrive Status`) %>% 
  summarize(maxRPM = max(`Average RPM`)) %>% 
  ggplot(aes(x = day, y = maxRPM, col = `TopDrive Status`)) +
  geom_point(alpha = .5)
#This is similar to the previous graph, but instead of looking at individual drives, it looks at active vs failed.
#We can immediately see that there seem to be a larger spread of dates for failed than active. 
#If we want to do time series or compare top drives in similar conditions, it might be worth asking for more examples
#in the March/April time as that is when the Failed topdrives seem to ramp up their maxRPM and we have a low sample of
#active topdrives in that timeframe.

join_data %>% mutate(day = as.Date(StartTime)) %>% 
  group_by(Serial_Number, `TopDrive Status`, `Sum of Life, yrs`) %>%
  summarise(range_of_dates = (max(day) - min(day))/ddays(1)) %>% 
  arrange(`TopDrive Status`)
#This is a summary of all of the date ranges we are given for each of the topdrives and we do get about 3 months for each. 
#We do get slightly less dates on older failed drive, but I do no think it is a very significant difference.

# 100-Ft Dive -------------------------------------------------------------

join_data %>% filter(Serial_Number == 'A180380') %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`, col = StartDepth)) +
  geom_point()+
  ylim(0,90)+
  scale_color_viridis_c(limits = c(0, 27000))
#This topdrive is a failed topdrive that only lasted .85 years. For reference, most Active topdrives are almost 6 years old.
#If we look at the pattern we see the change in depth over time as the drive is being used in the last three months before it
#failed. The topdrive reaches about 20,000ft about 5 or 6 different instances and hits a peak of about 80 RPM 3-4 of those
#instances

join_data %>% filter(Serial_Number == 'A110531') %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`, col = StartDepth)) +
  geom_point()+
  ylim(0,90)+
  scale_color_viridis_c(limits = c(0, 27000))
#This example is the opposite of the previous example. This topdrive is currently active at almost 6 years.
#This topdrive reaches 20,000ft at 5 seperate occasions and even reaches 25,000ft once or twice; however, the maximum
#RPM of 80 is only hit once and most of the highest RPM days are around the 73 RPM mark. In addition, the StartDepth on these
#days are usually at 10-15,000ft going to 20-25,000ft while in the failed drive we see multiple instances of 0 starting depth.
#this could be an indicator of multiple sites or repeated work.

join_data %>% filter(Serial_Number == 'A115150') %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`, col = StartDepth)) +
  geom_point()+
  ylim(0,90)+
  scale_color_viridis_c(limits = c(0, 27000))
#This final example is an example of a top drive that has lasted almost 6 years that failed. I will take this as a typical
#failure example.
#This example has the highest peak RPM and has no examples of 0 average RPM, but this drive is consistently around 73 RPM
#at the deepest parts. In addition, there are a few high average RPMs at even low depths, unlike the other two examples.

join_data %>% filter(Serial_Number == 'A108265') %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`, col = StartDepth)) +
  geom_point()+
  ylim(0,90)+
  scale_color_viridis_c(limits = c(0, 27000))

# ANOVA Testing -----------------------------------------------------------
# Splitting Data into Active / Inactive -----------------------------------
active_rigs <- join_data %>% 
  filter(`TopDrive Status` == "Active") %>% 
  select(7:12)

failed_rigs <- join_data %>% 
  filter(`TopDrive Status` == "Failed") %>% 
  select(7:12)

# WOB came out significantly different
t.test(active_rigs[5],failed_rigs[5])

# RPM came out significantly different
t.test(active_rigs[6],failed_rigs[6])

# ROP came out significantly different 
t.test(active_rigs[4],failed_rigs[4])

# Max Torque
t.test(active_rigs[1],failed_rigs[1])

# DeltaP
t.test(active_rigs[3], failed_rigs[3])

# Avg Torque
t.test(active_rigs[2], failed_rigs[2])


glmModel <- glm(join_data$`Average Torque, k ft-lbs` +
                  join_data$`Average DeltaP, psi` +
                  join_data$`Average ROP, ft/hr` +
                  join_data$`Average WOB, klbs` + 
                  join_data$`Average RPM` ~
                 `TopDrive Status`, data = join_data)
summary(glmModel)

# Splitting by years
join_data$LifeGroup <- cut(join_data$`Sum of Life, yrs`, c(0,3,5,6))
join_data$LifeGroup <- as.factor(join_data$LifeGroup)

levels(join_data$LifeGroup)

summary(glm(join_data$`Average RPM` ~
              LifeGroup, data = join_data))

summary(glm(join_data$`Average WOB, klbs` ~
              LifeGroup, data = join_data))

summary(glm(join_data$`Average ROP, ft/hr` ~
              LifeGroup, data = join_data))

summary(glm(join_data$`Average Torque, k ft-lbs` ~ 
              LifeGroup, data = join_data))

summary(glm(join_data$`Average DeltaP, psi` ~
              LifeGroup, data = join_data))

ggpairs(join_data[,c(7:12,19)], aes(color = `LifeGroup`))

join_data$LifeGroup <- relevel(join_data$LifeGroup, ref = 2)

summary(glm(join_data$`Average RPM` ~
              LifeGroup, data = join_data))

summary(glm(join_data$`Average WOB, klbs` ~
              LifeGroup, data = join_data))

summary(glm(join_data$`Average ROP, ft/hr` ~
              LifeGroup, data = join_data))

summary(glm(join_data$`Average Torque, k ft-lbs` ~ 
              LifeGroup, data = join_data))

summary(glm(join_data$`Average DeltaP, psi` ~
              LifeGroup, data = join_data))

# Removing the outlier?
remove_out_data <- join_data %>% filter(Serial_Number != "A115150")

summary(glm(join_data$`Average RPM` ~
              LifeGroup, data = join_data))

summary(glm(join_data$`Average WOB, klbs` ~
              LifeGroup, data = join_data))

summary(glm(join_data$`Average ROP, ft/hr` ~
              LifeGroup, data = join_data))

summary(glm(join_data$`Average Torque, k ft-lbs` ~ 
              LifeGroup, data = join_data))

summary(glm(join_data$`Average DeltaP, psi` ~
              LifeGroup, data = join_data))

join_data$LifeGroup <- relevel(join_data$LifeGroup, ref = 1)

ggpairs(remove_out_data[,-c(1:4,7,13,14,15,18)], cardinality_threshold = 16, aes(colour = LifeGroup))
ggpairs(join_data[,c(7:12,19)], aes(color = `LifeGroup`))


join_data %>% 
  ggplot(aes(x = `Average WOB, klbs`, 
             y = `Average ROP, ft/hr`, 
             color = `TopDrive Status`)) +
  geom_point(alpha = 0.25) +
  scale_color_manual(values = c("darkgreen", "red"))
# Control Charts ----------------------------------------------------------


# Calculate control limits 

mean_max_torque <- mean(feet_data$`Max Torque, k ft-lbs`)
sd_max_torque <- sd(feet_data$`Max Torque, k ft-lbs`)

mean_avg_torque <- mean(feet_data$`Average Torque, k ft-lbs`)
sd_avg_torque <- sd(feet_data$`Average Torque, k ft-lbs`)

mean_rpm <- mean(feet_data$`Average RPM`)
sd_rpm <- sd(feet_data$`Average RPM`)

mean_wob <- mean(feet_data$`Average WOB, klbs`)
sd_wob <- sd(feet_data$`Average WOB, klbs`)

mean_deltap <- mean(feet_data$`Average DeltaP, psi`)
sd_deltap <- sd(feet_data$`Average DeltaP, psi`)

mean_rop <- mean(feet_data$`Average ROP, ft/hr`)
sd_rop <- sd(feet_data$`Average ROP, ft/hr`)



ucl_max_torque <- mean_max_torque + 3 * sd_max_torque
lcl_max_torque <- mean_max_torque - 3 * sd_max_torque

ucl_avg_torque <- mean_avg_torque + 3 * sd_avg_torque
lcl_avg_torque <- mean_avg_torque - 3 * sd_avg_torque

ucl_rpm <- mean_rpm + 3 * sd_rpm
lcl_rpm <- mean_rpm - 3 * sd_rpm

ucl_wob <- mean_wob + 3 * sd_wob
lcl_wob <- mean_wob - 3 * sd_wob

ucl_deltap <- mean_deltap + 3 * sd_deltap
lcl_deltap <- mean_deltap - 3 * sd_deltap

ucl_rop <- mean_rop + 3 * sd_rop
lcl_rop <- mean_rop - 3 * sd_rop

mucl_max_torque <- mean_max_torque + 2 * sd_max_torque
mlcl_max_torque <- mean_max_torque - 2 * sd_max_torque

mucl_avg_torque <- mean_avg_torque + 2 * sd_avg_torque
mlcl_avg_torque <- mean_avg_torque - 2 * sd_avg_torque

mucl_rpm <- mean_rpm + 2 * sd_rpm
mlcl_rpm <- mean_rpm - 2 * sd_rpm

mucl_wob <- mean_wob + 2 * sd_wob
mlcl_wob <- mean_wob - 2 * sd_wob

mucl_deltap <- mean_deltap + 2 * sd_deltap
mlcl_deltap <- mean_deltap - 2 * sd_deltap

mucl_rop <- mean_rop + 2 * sd_rop
mlcl_rop <- mean_rop - 2 * sd_rop

# Identify out of control observations
out_of_control <- function(x, ucl, lcl) {
  x > ucl | x < lcl
}


# Torque (max) per rig ----------------------------------------------------------
feet_data %>% filter(Serial_Number == "A000214") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

#out of bounds over time
feet_data %>% filter(Serial_Number == "A1240") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

feet_data %>% filter(Serial_Number == "A187401") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

feet_data %>% filter(Serial_Number == "A51019") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

feet_data %>% filter(Serial_Number == "A108265") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

feet_data %>% filter(Serial_Number == "A113771") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

feet_data %>% filter(Serial_Number == "A115150") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

#Out of bounds over time
feet_data %>% filter(Serial_Number == "A44880") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque Rig A44880", 
       x = "Time", y = "Max Torque", color = "Out of Control")

#Clustered out of bounds
feet_data %>% filter(Serial_Number == "A180380") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

#ACTIVES

#A lot out of bounds over time
feet_data %>% filter(Serial_Number == "A209138") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")


feet_data %>% filter(Serial_Number == "A208648") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

feet_data %>% filter(Serial_Number == "A500814") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

feet_data %>% filter(Serial_Number == "A702445") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

feet_data %>% filter(Serial_Number == "A55577") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

feet_data %>% filter(Serial_Number == "A72154") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

feet_data %>% filter(Serial_Number == "A110531") %>% 
  ggplot(aes(x = StartTime, y = `Max Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Max Torque, k ft-lbs`, ucl_max_torque, lcl_max_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_max_torque, lcl_max_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_max_torque, mlcl_max_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Max Torque", x = "Time", y = "Max Torque")

# Torque (avg) per rig -------------------------------------------------------------

#End of life out of bounds
feet_data %>% filter(Serial_Number == "A000214") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(mucl_avg_torque, mlcl_avg_torque), linetype = "dashed", color = "orange")+
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#Out of bounds cluster in early Jan
feet_data %>% filter(Serial_Number == "A1240") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(mucl_avg_torque, mlcl_avg_torque), linetype = "dashed", color = "orange")+
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#large spread, lead up to out of bounds observations over time
feet_data %>% filter(Serial_Number == "A187401") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(mucl_avg_torque, mlcl_avg_torque), linetype = "dashed", color = "orange")+
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#Lead up to out of bounds, but stops for testing right before
feet_data %>% filter(Serial_Number == "A51019") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#stable, lived for 4 years
feet_data %>% filter(Serial_Number == "A108265") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#stable, lived for 4.5 years
feet_data %>% filter(Serial_Number == "A113771") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#longest living failed, stable
feet_data %>% filter(Serial_Number == "A115150") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#larger spread, ups and downs, no out of bounds
feet_data %>% filter(Serial_Number == "A44880") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#Clustered out of bounds
feet_data %>% filter(Serial_Number == "A180380") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(mucl_avg_torque, mlcl_avg_torque), linetype = "dashed", color = "orange")+
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#ACTIVES

#A lot out of bounds over time
feet_data %>% filter(Serial_Number == "A209138") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(mucl_avg_torque, mlcl_avg_torque), linetype = "dashed", color = "orange") +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#stable
feet_data %>% filter(Serial_Number == "A208648") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#stable
feet_data %>% filter(Serial_Number == "A500814") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#stable
feet_data %>% filter(Serial_Number == "A702445") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#stable
feet_data %>% filter(Serial_Number == "A55577") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_avg_torque, mlcl_avg_torque), linetype = "dashed", color = "orange") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#stable
feet_data %>% filter(Serial_Number == "A72154") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

#stable
feet_data %>% filter(Serial_Number == "A110531") %>% 
  ggplot(aes(x = StartTime, y = `Average Torque, k ft-lbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average Torque, k ft-lbs`,ucl_avg_torque, lcl_avg_torque)), size = 2) +
  geom_hline(yintercept = c(ucl_avg_torque, lcl_avg_torque), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg Torque", x = "Time", y = "Avg Torque")

# rpm per rig -------------------------------------------------------------


#
feet_data %>% filter(Serial_Number == "A000214") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rpm, mlcl_rpm), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#Past 100
feet_data %>% filter(Serial_Number == "A1240") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#High rate of change
feet_data %>% filter(Serial_Number == "A187401") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#Stops after reaching ~100
feet_data %>% filter(Serial_Number == "A51019") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#High rate of change, reaches > 100 before giving out
feet_data %>% filter(Serial_Number == "A108265") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#
feet_data %>% filter(Serial_Number == "A113771") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#
feet_data %>% filter(Serial_Number == "A115150") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#larger spread, ups and downs, no out of bounds
feet_data %>% filter(Serial_Number == "A44880") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rpm, mlcl_rpm), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#Clustered out of bounds
feet_data %>% filter(Serial_Number == "A180380") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#ACTIVES

#A lot out of bounds over time
feet_data %>% filter(Serial_Number == "A209138") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#stable
feet_data %>% filter(Serial_Number == "A208648") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#stable
feet_data %>% filter(Serial_Number == "A500814") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#stable
feet_data %>% filter(Serial_Number == "A702445") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#stable
feet_data %>% filter(Serial_Number == "A55577") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#stable
feet_data %>% filter(Serial_Number == "A72154") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")

#stable
feet_data %>% filter(Serial_Number == "A110531") %>% 
  ggplot(aes(x = StartTime, y = `Average RPM`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average RPM`,ucl_rpm, lcl_rpm)), size = 2) +
  geom_hline(yintercept = c(ucl_rpm, lcl_rpm), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg RPM", x = "Time", y = "Avg RPM")


# wob -------------------------------------------------------------------

#End of life out of bounds
feet_data %>% filter(Serial_Number == "A000214") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_wob, mlcl_wob), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#Out of bounds cluster in early Jan
feet_data %>% filter(Serial_Number == "A1240") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_wob, mlcl_wob), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#large spread, lead up to out of bounds observations over time
feet_data %>% filter(Serial_Number == "A187401") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#Lead up to out of bounds, but stops for testing right before
feet_data %>% filter(Serial_Number == "A51019") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#stable, lived for 4 years
feet_data %>% filter(Serial_Number == "A108265") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_wob, mlcl_wob), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#stable, lived for 4.5 years
feet_data %>% filter(Serial_Number == "A113771") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#longest living failed, stable
feet_data %>% filter(Serial_Number == "A115150") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#larger spread, ups and downs, no out of bounds
feet_data %>% filter(Serial_Number == "A44880") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_wob, mlcl_wob), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#Clustered out of bounds
feet_data %>% filter(Serial_Number == "A180380") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#ACTIVES

#A lot out of bounds over time
feet_data %>% filter(Serial_Number == "A209138") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_wob, mlcl_wob), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#stable
feet_data %>% filter(Serial_Number == "A208648") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_wob, mlcl_wob), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#stable
feet_data %>% filter(Serial_Number == "A500814") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_wob, mlcl_wob), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#stable
feet_data %>% filter(Serial_Number == "A702445") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#stable
feet_data %>% filter(Serial_Number == "A55577") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#stable
feet_data %>% filter(Serial_Number == "A72154") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")

#stable
feet_data %>% filter(Serial_Number == "A110531") %>% 
  ggplot(aes(x = StartTime, y = `Average WOB, klbs`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average WOB, klbs`,ucl_wob, lcl_wob)), size = 2) +
  geom_hline(yintercept = c(ucl_wob, lcl_wob), linetype = "dashed", color = "red") +
  labs(title = "Individual (I) Chart for Avg WOB", x = "Time", y = "WOB")


# deltap ------------------------------------------------------------------

#
vline <- as_datetime("2021-11-11")
feet_data %>% filter(Serial_Number == "A000214") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  geom_vline(xintercept = vline, color = "black") +
  labs(title = "Individual (I) Chart for Avg DeltaP Rig A000214", 
       x = "Time", y = "Avg DeltaP", color = "Out of Control") 

#
feet_data %>% filter(Serial_Number == "A1240") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#
feet_data %>% filter(Serial_Number == "A187401") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#
feet_data %>% filter(Serial_Number == "A51019") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#lived for 4 years
feet_data %>% filter(Serial_Number == "A108265") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#lived for 4.5 years
feet_data %>% filter(Serial_Number == "A113771") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#longest living failed
feet_data %>% filter(Serial_Number == "A115150") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#
feet_data %>% filter(Serial_Number == "A44880") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#
feet_data %>% filter(Serial_Number == "A180380") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#ACTIVES

#
feet_data %>% filter(Serial_Number == "A209138") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#
feet_data %>% filter(Serial_Number == "A208648") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#
feet_data %>% filter(Serial_Number == "A500814") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#
feet_data %>% filter(Serial_Number == "A702445") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#
feet_data %>% filter(Serial_Number == "A55577") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#
feet_data %>% filter(Serial_Number == "A72154") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")

#
feet_data %>% filter(Serial_Number == "A110531") %>% 
  ggplot(aes(x = StartTime, y = `Average DeltaP, psi`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average DeltaP, psi`, ucl_deltap, lcl_deltap)), size = 2) +
  geom_hline(yintercept = c(ucl_deltap, lcl_deltap), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_deltap, mlcl_deltap), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg DeltaP", x = "Time", y = "Avg DeltaP")













# rop ---------------------------------------------------------------------
#
feet_data %>% filter(Serial_Number == "A000214") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A1240") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A187401") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A51019") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A108265") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A113771") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A115150") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A44880") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A180380") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#ACTIVES

#
feet_data %>% filter(Serial_Number == "A209138") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A208648") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A500814") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A702445") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A55577") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A72154") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")

#
feet_data %>% filter(Serial_Number == "A110531") %>% 
  ggplot(aes(x = StartTime, y = `Average ROP, ft/hr`)) +
  geom_line(color = "blue") +
  geom_point(aes(color = out_of_control(`Average ROP, ft/hr`,ucl_rop, lcl_rop)), size = 2) +
  geom_hline(yintercept = c(ucl_rop, lcl_rop), linetype = "dashed", color = "red") +
  geom_hline(yintercept = c(mucl_rop, mlcl_rop), linetype = "dashed", color = "orange")+
  labs(title = "Individual (I) Chart for Avg ROP", x = "Time", y = "Avg ROP")





# Histograms --------------------------------------------------
# Torque (max)
ggplot(feet_data, aes(x = `Max Torque, k ft-lbs`)) +
  geom_histogram()

# Torque (avg)
ggplot(feet_data, aes(x = `Average Torque, k ft-lbs`)) +
  geom_histogram()

# rpm 
ggplot(feet_data, aes(x = `Average RPM`)) +
  geom_histogram()

# wob
ggplot(feet_data, aes(x = `Average WOB, klbs`)) +
  geom_histogram()

# deltap
ggplot(feet_data, aes(x = `Average DeltaP, psi`)) +
  geom_histogram()

# rob
ggplot(feet_data, aes(x = `Average ROP, ft/hr`)) +
  geom_histogram()













# Out of control count ----------------------------------------------------

# Define a function to calculate values exceeding thresholds for each column
calculate_thresholds <- function(x) {
  mean_value <- mean(x)
  std_dev <- sd(x)
  value_2_std_dev_above_mean <- mean_value + 2 * std_dev
  value_3_std_dev_above_mean <- mean_value + 3 * std_dev
  return(list(mean_value = mean_value,
              std_dev = std_dev,
              value_2_std_dev_above_mean = value_2_std_dev_above_mean,
              value_3_std_dev_above_mean = value_3_std_dev_above_mean))
}

# Calculate thresholds for each column within each group
group_summary <- feet_data[,c(-1,-3,-4)] %>%
  group_by(Serial_Number) %>%
  summarize(across(starts_with("Max Torque"), calculate_thresholds))



summary_feet_data <- join_data[-c(1,6,13,14,15,17,19)]




# Max Torque Summary ------------------------------------------------------

#overshooting
max_torque_overshot <- summary_feet_data[which(summary_feet_data$`Max Torque, k ft-lbs` > ucl_max_torque),]

ggplot(max_torque_overshot, aes(y = `Max Torque, k ft-lbs`, x = `Latest Failure Date`, color = Serial_Number))+
  geom_point()


ggplot(max_torque_overshot, aes(y = `Max Torque, k ft-lbs`, x = StartTime, color = Serial_Number))+
  geom_point()
  
summary_max_torque_overshot <- max_torque_overshot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(max_torque_above_3standard = n())
 
max_torque_medium <- summary_feet_data[which(summary_feet_data$`Max Torque, k ft-lbs` > mucl_max_torque & summary_feet_data$`Max Torque, k ft-lbs` < ucl_max_torque),]

summary_max_torque_medium <- max_torque_medium %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(max_torque_above_2standard = n())

#undershooting
max_torque_undershot <- summary_feet_data[which(summary_feet_data$`Max Torque, k ft-lbs` < lcl_max_torque),]

summary_max_torque_undershot <- max_torque_undershot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(max_torque_below_3standard = n())

max_torque_under <- summary_feet_data[which(summary_feet_data$`Max Torque, k ft-lbs` < mlcl_max_torque & summary_feet_data$`Max Torque, k ft-lbs` > lcl_max_torque),]

summary_max_torque_under <- max_torque_under %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(max_torque_below_2standard = n())

# Average Torque Summary --------------------------------------------------

avg_torque_overshot <- summary_feet_data[which(summary_feet_data$`Average Torque, k ft-lbs` > ucl_avg_torque),]

summary_avg_torque_overshot <- avg_torque_overshot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(avg_torque_above_3standard = n())

avg_torque_medium <- summary_feet_data[which(summary_feet_data$`Average Torque, k ft-lbs` > mucl_avg_torque & summary_feet_data$`Average Torque, k ft-lbs` < ucl_avg_torque),]

summary_avg_torque_medium <- avg_torque_medium %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(avg_torque_above_2standard = n())

#undershooting
avg_torque_undershot <- summary_feet_data[which(summary_feet_data$`Average Torque, k ft-lbs` < lcl_avg_torque),]

summary_avg_torque_undershot <- avg_torque_undershot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(avg_torque_below_3standard = n())

avg_torque_under <- summary_feet_data[which(summary_feet_data$`Average Torque, k ft-lbs` < mlcl_avg_torque & summary_feet_data$`Average Torque, k ft-lbs` > lcl_avg_torque),]

summary_avg_torque_under <- max_torque_under %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(avg_torque_below_2standard = n())


# Rpm Summary -------------------------------------------------------------

rpm_overshot <- summary_feet_data[which(summary_feet_data$`Average RPM` > ucl_rpm),]

#No observations above 3 standard deviations
summary_rpm_overshot <- rpm_overshot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(rpm_above_3standard = n())

rpm_medium <- summary_feet_data[which(summary_feet_data$`Average RPM` > mucl_rpm & summary_feet_data$`Average RPM` < ucl_rpm),]

summary_rpm_medium <- rpm_medium %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(rpm_above_2standard = n())

#undershooting
rpm_undershot <- summary_feet_data[which(summary_feet_data$`Average RPM` < lcl_rpm),]

summary_rpm_undershot <- rpm_undershot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(rpm_below_3standard = n())

rpm_under <- summary_feet_data[which(summary_feet_data$`Average RPM` < mlcl_rpm & summary_feet_data$`Average RPM` > lcl_rpm),]

summary_rpm_under <- rpm_under %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(rpm_below_2standard = n())

# wob Summary ---------------------------------------------------------------------
wob_overshot <- summary_feet_data[which(summary_feet_data$`Average WOB, klbs` > ucl_wob),]

summary_wob_overshot <- wob_overshot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(wob_above_3standard = n())

wob_medium <- summary_feet_data[which(summary_feet_data$`Average WOB, klbs` > mucl_wob & summary_feet_data$`Average WOB, klbs` < ucl_wob),]

summary_wob_medium <- wob_medium %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(wob_above_2standard = n())

#undershooting
wob_undershot <- summary_feet_data[which(summary_feet_data$`Average WOB, klbs` < lcl_wob),]

summary_wob_undershot <- wob_undershot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(wob_below_3standard = n())

wob_under <- summary_feet_data[which(summary_feet_data$`Average WOB, klbs` < mlcl_wob & summary_feet_data$`Average WOB, klbs` > lcl_wob),]

summary_wob_under <- wob_under %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(wob_below_2standard = n())


# Deltap Summary ----------------------------------------------------------

deltap_overshot <- summary_feet_data[which(summary_feet_data$`Average DeltaP, psi` > ucl_deltap),]

summary_deltap_overshot <- deltap_overshot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(deltap_above_3standard = n())

deltap_medium <- summary_feet_data[which(summary_feet_data$`Average DeltaP, psi` > mucl_deltap & summary_feet_data$`Average DeltaP, psi` < ucl_deltap),]

summary_deltap_medium <- deltap_medium %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(deltap_above_2standard = n())

#undershooting
deltap_undershot <- summary_feet_data[which(summary_feet_data$`Average DeltaP, psi` < lcl_deltap),]

summary_deltap_undershot <- deltap_undershot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(deltap_below_3standard = n())

deltap_under <- summary_feet_data[which(summary_feet_data$`Average DeltaP, psi` < mlcl_deltap & summary_feet_data$`Average DeltaP, psi` > lcl_deltap),]

summary_deltap_under <- deltap_under %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(deltap_below_2standard = n())
# rop Summary -------------------------------------------------------------

rop_overshot <- summary_feet_data[which(summary_feet_data$`Average ROP, ft/hr` > ucl_rop),]

summary_rop_overshot <- rop_overshot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(rop_above_3standard = n())

rop_medium <- summary_feet_data[which(summary_feet_data$`Average ROP, ft/hr` > mucl_rop & summary_feet_data$`Average ROP, ft/hr` < ucl_rop),]

summary_rop_medium <- rop_medium %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(rop_above_2standard = n())

#undershooting
rop_undershot <- summary_feet_data[which(summary_feet_data$`Average ROP, ft/hr` < lcl_rop),]

summary_rop_undershot <- rop_undershot %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(rop_below_3standard = n())

rop_under <- summary_feet_data[which(summary_feet_data$`Average ROP, ft/hr` < mlcl_rop & summary_feet_data$`Average ROP, ft/hr` > lcl_rop),]

summary_rop_under <- rop_under %>%
  group_by(Serial_Number, `Sum of Life, yrs`, `TopDrive Status`) %>% 
  summarise(rop_below_2standard = n())




# Overall Summary ---------------------------------------------------------

Combined_Summary <- summary_max_torque_overshot %>% 
  full_join(summary_max_torque_medium, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_max_torque_undershot, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_max_torque_under, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_avg_torque_overshot, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>%
  full_join(summary_avg_torque_medium, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_avg_torque_undershot, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_avg_torque_under, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_rpm_overshot, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>%
  full_join(summary_rpm_medium, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_rpm_undershot, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_rpm_under, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_wob_overshot, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>%
  full_join(summary_wob_medium, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_wob_undershot, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_wob_under, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_deltap_overshot, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>%
  full_join(summary_deltap_medium, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_deltap_undershot, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_deltap_under, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_rop_overshot, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>%
  full_join(summary_rop_medium, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_rop_undershot, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status")) %>% 
  full_join(summary_rop_under, by = c("Serial_Number", "Sum of Life, yrs", "TopDrive Status"))
  
Combined_Summary$Serial_Number <- as.factor(Combined_Summary$Serial_Number)

write_excel_csv(Combined_Summary, "Combined_Summary.xlsx")
  
