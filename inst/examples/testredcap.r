#Clear existing data and graphics
rm(list=ls())
#graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data

data=read.csv(system.file("examples", "testredcap.csv", package = "DescrTab2"))
#Setting Labels
label(data$patid)="Patient ID"
label(data$redcap_repeat_instrument)="Repeat instrument"
label(data$age)="Age"
label(data$sex)="Gender"
label(data$time)="Time"

#Setting Factors(will create new variable for factors)
data$sex.factor = factor(data$sex,levels=c("0", "1"))
levels(data$sex.factor)=c("Male", "Female")
data$redcap_repeat_instrument.factor = factor(data$redcap_repeat_instrument,levels=c("0"))
levels(data$redcap_repeat_instrument.factor)=c("Interview")
