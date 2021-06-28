install.packages("tidyverse")
library(tidyverse)
library(dplyr)

demo_table1 <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

linear_regression <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=demo_table1)#generate multiple linear regression model

summary(linear_regression)

coil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
total_summary <- coil_table %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI)) #create summary table with multiple columns
lot_summary <- coil_table %>% group_by(Manufacturing_Lot)%>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI)) #create summary table with multiple columns

t.test(coil_table$PSI,mu = 1500)

t.test(subset(coil_table, Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
t.test(subset(coil_table, Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
t.test(subset(coil_table, Manufacturing_Lot=="Lot3")$PSI,mu = 1500)