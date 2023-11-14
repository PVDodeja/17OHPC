# install.packages("devtools")
devtools::install_github("metrumresearchgroup/pmplots")
install.packages("yspec")


#load packages
library(pmplots)
library(dplyr)
library(purrr)


#load source data
setwd("E:/PHD/17OHPC/Prerna/Datasets/Appended_OPRU_MFMU")
getwd()
df<-  read.csv("test_EDA.csv") 
df<- df %>%  rename ("ID"= "ID_Flagged") #rename ID_Flagged to ID for pmplots

#filter data by study
df_SingleGA<- filter(df, INDEX< 20000)
df_SingleGA<-filter(df_SingleGA, DV!=0)
df_MultiGA<-filter(df, INDEX>20000)


dv_time(df_SingleGA) #+ facet_wrap(~ID, scales="free_x")
dv_time(df_SingleGA, yname="17OHPC Plasma Concentrations (ng/mL)", log=TRUE) + facet_wrap(~ID)
dv_time(df_MultiGA)
