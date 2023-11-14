rm(list = ls(all.names= TRUE)) #clear environment

## Load libraries used in this script
library(ggplot2) # For plotting
library(tidyverse) #data cleaning and manipulation
library(cowplot) # For plotting multiple graphs

# Set working directory
setwd("E:/PHD/17OHPC/Prerna/Datasets/OPRU")
getwd()
################
# Read dataset
# --------------
raw<- read.csv("Shringi_OPRU_Summary_MODIFIED_R.csv")

plot1<- ggplot(data = raw, aes(x = T.half, y = "")) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Spread of Half-Lives in OPRU Study", x= "Half-Life (Days)", y="") +
  scale_x_continuous(limits = c(9, 28), breaks=c(10,12,14,16,18,20,22,24,26,28))+
  theme_cowplot()
  # theme(axis.text.y = element_blank(),   # Remove Y-axis labels
  #       axis.ticks.y = element_blank())  # Remove Y-axis ticks
plot1

ggsave("Half_Life_BoxPlot.png",plot1,height=4,width=5,dpi=300)
