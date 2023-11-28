#Demographics exploration--- OPRU and MFMU



#clear all previous activity in the workspace
rm(list=ls(all.names=TRUE)) 

install.packages("trelliscopejs")
install.packages("GGally")
#load packages
library(pmplots)
library(dplyr)
library(ggplot2) 
library(ggpubr)
library(cowplot)
library(gridExtra)
library(GGally)

#load source data
setwd("E:/PHD/17OHPC/Prerna/Datasets/Appended_OPRU_MFMU")
getwd()
Single_GA<-  read.csv("Demographics_OPRU.csv")
Multi_GA<-  read.csv("Demographics_MFMU.csv")

contCovarList1<- c('AGE','BMI','WT','HEIGHT')
ContVCont1= Single_GA %>% pairs_plot(y=contCovarList1)+ rot_x(50)
ContVCont1
ggsave("SingleGA_ContCovar_PairPlot.png", height=6, width=7)

Cat<- c('RACE')
Race_Plot1= Single_GA %>% wrap_cont_cat(x=Cat, y=contCovarList1, use_labels=TRUE)+ rot_x(35)
Race_Plot1
ggsave("Continuous_Categorical.png", Race_Plot, height=5, width=6)

contCovarList2<- c('AGE','BMI','WEIGHT','HEIGHT')
ContVCont2= Multi_GA %>% pairs_plot(y=contCovarList2)+ rot_x(50)
ContVCont2
Race_Plot2= Multi_GA %>% wrap_cont_cat(x=Cat, y=contCovarList2, use_labels=TRUE)+ rot_x(35)
Race_Plot2
ggsave("MultiGA_ContCovar_PairPlot.png", ContVCont2, height=6, width=7)
  
