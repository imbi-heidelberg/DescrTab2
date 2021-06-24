#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data

data=read.csv(system.file("examples", "testredcap.csv", package = "DescrTab2"))
#Setting Labels
label(data$age)="Age"
label(data$sex)="Gender"

#Setting Factors(will create new variable for factors)
data$sex.factor = factor(data$sex,levels=c("0", "1"))
levels(data$sex.factor)=c("Male", "Female")
