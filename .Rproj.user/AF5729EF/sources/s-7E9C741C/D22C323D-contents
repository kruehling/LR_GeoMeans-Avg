#graphics for enumeration geomeans and temporal trends over summer 2020 Laramie River 
#Created: 10/21/20

#load necessary packages 
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(magrittr)
library(ggplot2)


setwd("~/Dropbox/WRP_Ecoli/Data/Laramie River/Raw Data")

###GeoMeans plot ####
#read in manually calulated geomeans 
means<- read.csv("GeoMeansEnumerations.csv")

#get different columns for each parameter
meansWide<- means %>% spread(key = "Parameter",value= "Value")

#remove WC2 and LR1 for visualization purposes since they only had one value 
meansWideSelect<- meansWide %>% filter(Site != "WC2", Site != "LR1")

#plot dashed/solid lines for different parameters
ggplot(meansWideSelect, aes(x= GM, y= Ec1603, linetype= "Ec1603"))+geom_line(aes( color = Site)) +
  geom_line(aes(y= EcIDEXX, color= Site, linetype= "EcIDEXX")) +
  labs(title = "Geometric means for IDEXX and 1603 \nE. coli Enumeration Laramie River", 
       y="CFU/100ml", x="Rolling Geometric Means") +
  theme_minimal(base_size = 19)

####Methods comp####

#filtering and spreading data for plot, removed blanks for visualization purposes 
ENU_avgWD<- ENU_avg %>% spread(key = "parameter",value= "Rep_avg")%>% 
  filter(site != "LR-BlankDI", site != "LR-FB")

#### scatter plot of enumeration with reps averaged
ggplot(ENU_avgWD, aes(x=EcIDEXX , y= Ec1603))+geom_point(aes(color= site))+
  scale_x_log10()+ scale_y_log10()+
  theme_minimal(base_size = 19)+ 
  labs(title = "E. coli Enumeration methods \nLaramie River Summer 2020", 
       y= "EPA 1603 (CFU/100ml)", x= "IDEXX (MPN/100ml)")

##### scatter plot not on log scale 
ggplot(ENU_avgWD, aes(x=EcIDEXX , y= Ec1603))+geom_point(aes(color= site))+
  theme_minimal(base_size = 19)+ 
  labs(title = "E. coli Enumeration methods \nLaramie River Summer 2020", 
       y= "EPA 1603 (CFU/100ml)", x= "IDEXX (MPN/100ml)")


#####Temporal avgs####

#1603
ggplot(ENU_avgWD, aes(x= date, y= Ec1603, linetype= "Ec1603"))+
  geom_line(aes( color = site)) +
  labs(title = "1603 Enumeration Averages", 
       y="CFU/100ml") +
  theme_minimal(base_size = 19)

#IDEXX
ggplot(ENU_avgWD, aes(x= date, y= EcIDEXX, linetype= "EcIDEXX"))+
  geom_line(aes( color = site)) +
  labs(title = "IDEXX Enumeration Averages", 
       y="MPN/100ml") +
  theme_minimal(base_size = 19)+
  scale_linetype_manual(values = "dashed")
