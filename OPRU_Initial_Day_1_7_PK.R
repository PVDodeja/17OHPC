rm(list = ls(all.names= TRUE)) #

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

# Create Individual plots
plot_IND <- 
  ggplot(data=raw_no_missing, aes(x = TIME, y = DV)) +
  theme_cowplot()+
  geom_point(data = raw_no_missing, aes(x = TIME, y=DV))+
  #geom_line(data = raw_no_missing, aes(x = TIME, y=DV))+
  facet_wrap(~ID)+    #separating the plots by subject
  ylab("17-OHPC Concentration (ng/mL)")+
  xlab("Time in days")+
  ggtitle("Data Exploration", subtitle="OPRU study")
plot_IND

#ID 57 and 52 look best