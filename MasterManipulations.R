#filtering master spreadsheet for geomean calculations 
#Created: 10/21/20

#load necessary packages 
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(magrittr)
library(tbrf)
library(dbplyr)
library(stats)
library(car)

setwd("~/Dropbox/WRP_Ecoli/Data/Laramie River/Raw Data")

#load master and reformate date and time
master<-read.csv("~/Dropbox/WRP_Ecoli/Data/Master_datasheet.csv")
master<- master %>% mutate(date = as.POSIXct(date, format = "%m/%d/%y", tz = "UTC"))

#### Temporal Averages df####

# New df w/ only Enumeration parameters & Create new column with avg of replicates
ENU<- master %>% 
  filter(parameter == "Ec1603" | parameter == "EcIDEXX") %>% 
  group_by(parameter,date, site) %>% mutate(Rep_avg = mean(value))

#New df without substrate (all water) and one value per unique  Enumeration/site combo  
ENU_avg<- ENU %>% select(date, site, parameter, replicate, Rep_avg) %>% 
  filter(replicate == 1) %>% 
  arrange(site, date)

#create csv for manual calcualtion of geomeans b/c had issues troubleshooting line 36 ....

write.csv(ENU_avg, '~/Dropbox/WRP_Ecoli/Data/Laramie River/Raw Data/EnumerationAvgsDUP.csv')

##Geo<-tbr_gmean(ENU_avg, x= Rep_avg, tcolumn =  date, unit = "weeks", n=6)
  
###Stats####  
## testing variance between geomeans of two methods

means<- read.csv("GeoMeansEnumerations.csv")

#ANOVA for geomeans of enumeration methods   
geomod<- aov(Value ~ Parameter, data= means)
summary(geomod)

#test the residuals 
geores<- residuals(geomod)
hist(geores)
shapiro.test(geores) # this P < .5 residual error may not follow normal distribution 

#testing normality of residuals
leveneTest(means$Value ~ means$Parameter) 
bartlett.test(means$Value ~ means$Parameter)

TukeyHSD(geomod, "Parameter")

#concern about normaligy of residual error 
##going to try log transformation of data and non-parametric comparison 
wilcox.test(means$Value ~ means$Parameter)

#log transform the data to see if we can increase residual error (did not work)
geomod_log<- aov(log(Value)~ Parameter, data= means)
summary(geomod_log)
geores_log<- residuals(geomod_log)

hist(geores_log)
shapiro.test(geores_log)

###  ANOVA between the Enumeration avg of two Enumeration methods 
avgENUmod<- aov(Rep_avg ~ parameter, data= ENU_avg)
summary(avgENUmod)

#test the residuals 
ENUres<- residuals(avgENUmod)
hist(ENUres) #some outliers in this histogram 

#testing residuals
shapiro.test(ENUres) # this P < .5 residual error may not follow normal distribution 
bartlett.test(ENU_avg$Rep_avg ~ ENU_avg$parameter) # also <.5 

TukeyHSD(avgENUmod, "parameter")

#non-parametric test of variance, is significant
wilcox.test(ENU_avg$Rep_avg ~ ENU_avg$parameter)

##log transforming Enumeration avg data to see if that will help residuals
ENU_avgSelect<- ENU_avg %>% filter(Rep_avg != 0)
ENUmod_log<- aov(log(Rep_avg) ~ parameter, data= ENU_avgSelect)
summary(ENUmod_log)

#looking at residuals- to confirm ANOVA assumptions 
ENUres_log<- residuals(ENUmod_log)
hist(ENUres_log)
shapiro.test(ENUres_log) 
bartlett.test(ENU_avgSelect$Rep_avg ~ ENU_avgSelect$parameter)
#small p value for both shapiro and Bartlett may not have normally distributed residuals

####Multiplier####
#looking at the multiplying factor between two methods avg
ENU_avgWD<- ENU_avgWD %>% mutate(Multiplier = (EcIDEXX/Ec1603))
ggplot(ENU_avgWD, aes(Multiplier))+geom_histogram()




