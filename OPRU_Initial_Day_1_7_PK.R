#-----17-OHPC Data Exploration----------#

rm(list = ls(all.names= TRUE)) #clear environment

## Load libraries used in this script
library(ggplot2) # For plotting
library(tidyverse) #data cleaning and manipulation
library(cowplot) # For plotting multiple graphs

# Set working directory
setwd("E:/R files/17OHPC/17OHPC")
getwd()
################
# Read dataset
# --------------
raw<- read.csv("OPRU_PK_Day1_7_Raw.csv")

#filter the data
raw_no_missing<- filter(raw, DV!=".") #remove the DVs that have missing value

#DV is considered a string, so change to numeric--- ELSE ERRORS
raw_no_missing$DV= as.numeric(raw_no_missing$DV)

#Data Exploration By ID-Linear Scale
plot_IND <- 
  ggplot()+
  geom_point(data=raw_no_missing, aes(x = TIME, y=DV))+
  geom_line(data = raw_no_missing, aes(x = TIME, y=DV))+
  facet_wrap(~ID)+    #separating the plots by subject+
  scale_y_continuous(limits=c(0,60))+
  ylab("17-OHPC Concentration (ng/mL)")+
  xlab("Time (days)")+
  theme_cowplot()+
  ggtitle("Data Exploration", subtitle="Linear Scale")
plot_IND
#ID 57 and 52 look best

#Data Exploration By ID-Log Scale
plot_IND_log <- 
  ggplot()+
  geom_point(data=raw_no_missing, aes(x = TIME, y=DV))+
  geom_line(data = raw_no_missing, aes(x = TIME, y=DV))+
  facet_wrap(~ID)+    #separating the plots by subject
  scale_y_log10()+
  scale_x_continuous(limits=c(0,8))+
  ylab("17-OHPC Concentration (ng/mL)")+
  xlab("Time (days)")+
  theme_cowplot()+
  ggtitle("Data Exploration", subtitle="Log10 Scale")
plot_IND_log
