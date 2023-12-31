#Data Exploration MFMU
#clear all previous activity in the workspace
rm(list=ls(all.names=TRUE)) 

install.packages("trelliscopejs")
install.packages("GGally")
install.packages("ggpmisc")
#load packages
library(pmplots)
library(dplyr)
library(ggplot2) 
library(ggpubr)
library(cowplot)
library(gridExtra)
library(GGally)
library(ggpmisc)
#load source data
setwd("E:/PHD/17OHPC/Prerna/Datasets/MFMU")
getwd()
d1<-  read.csv("DataExplore_MFMU.csv")


d1_test<- d1 %>% mutate_at(1:20, as.numeric)



# Function for scatter plots
plot_scatter<-function(data_for_plot, biom, conc)
{
  p3<- ggplot(data_for_plot, aes({{biom}}, {{conc}}))+
    geom_point()
  p3+theme_cowplot()
}
# Function for quantile plots
q10 <- c(0.25, 0.5,0.75)
plot_quantile<-function(data_for_plot, biom, conc)
  {
    p1<- ggplot(data_for_plot, aes({{biom}}, {{conc}}))+
  geom_point()
     p1+ geom_quantile(quantiles=q10, colour = "blue", linewidth =1, linetype="dashed" ,alpha = 0.5)+theme_cowplot()
}

# Function for boxplots
plot_box<-function(data_for_plot, cont)
{
  p1<- ggplot(data_for_plot, aes({{cont}}))+
    geom_boxplot()+theme_cowplot()
    p1
}


# Function for histograms
plot_hist<-function(data_for_plot, cont)
{
  p2<- ggplot(data_for_plot, aes({{cont}}))+
    geom_histogram(binwidth=0.5)+theme_cowplot()
  p2
}


#looking at PARITY as a factor on OHP concentrations?
d1_test$PARITY <- as.factor(d1_test$PARITY)
plot_OHP1<- ggplot(d1_test, aes(x=PARITY, y=OHP1))
plot_OHP1<- plot_OHP1+ geom_boxplot()+theme_cowplot()


plot_OHP2<- ggplot(d1_test, aes(x=PARITY, y=OHP2))
plot_OHP2<- plot_OHP2+ geom_boxplot()+theme_cowplot()

# parity_as_factor<- ggarrange(plot_OHP1, plot_OHP2 + rremove("x.text"), 
#                   labels = c("A", "B"),
#                   ncol = 2, nrow = 1)
# parity_as_factor



#looking at RACE as a factor on OHP concentrations?
d1_test$RACE <- as.factor(d1_test$RACE)
plot_race1<- ggplot(d1_test, aes(x=RACE, y=OHP1))
plot_race1<- plot_race1+ geom_boxplot()+theme_cowplot()


plot_race2<- ggplot(d1_test, aes(x=RACE, y=OHP2))
plot_race2<- plot_race2+ geom_boxplot()+theme_cowplot()

categorical<- ggarrange(plot_OHP1, plot_OHP2, plot_race1, plot_race2 + rremove("x.text"), 
                             labels = c("A", "B", "C", "D"),
                             ncol = 2, nrow = 2)
categorical<- annotate_figure(categorical,top = text_grob("PARITY & RACE: Impact on HPC Conc", color = "red", face = "bold", size = 14))
ggsave("RACE_PARITY_OHP1_OHP2.pdf", categorical, width=8, height=6)
#Call scatter
plot_scatter(d1_test, OHP1, OHP2)
plot_scatter(d1_test, CAP1, OHP1)
plot_scatter(d1_test, CAP2, OHP2)
#Call function
plot_hist(d1_test, RACE)
plot_box(d1_test, GAVISIT)


#Callfunction
plot_box(d1_test, BMI)


plot_quantile(d1_test, GATEST1, OHP1)
plot_quantile(d1_test, GATEST2, OHP2)



#CRP and OHP
cr_ohp1<- plot_quantile(d1_test, CRP1, OHP1)
cr_ohp2 <-plot_quantile(d1_test, CRP2, OHP1)
cr_ohp3<-plot_quantile(d1_test, CRP1, OHP2)
cr_ohp4 <-plot_quantile(d1_test, CRP2, OHP2)
plot1<- ggarrange(cr_ohp1, cr_ohp2, cr_ohp3, cr_ohp4 + rremove("x.text"), 
              labels = c("A", "B", "C", "D"),
              ncol = 2, nrow = 2)
plot1
ggsave("CRP_OHP.pdf", plot1, width=8, height=6)

#CRH and OHP
ch_ohp1<- plot_quantile(d1_test, CRH1, OHP1)
ch_ohp2 <-plot_quantile(d1_test, CRH2, OHP1)
ch_ohp3<-plot_quantile(d1_test, CRH1, OHP2)
ch_ohp4 <-plot_quantile(d1_test, CRH2, OHP2)
plot2<- ggarrange(ch_ohp1, ch_ohp2, ch_ohp3, ch_ohp4 + rremove("x.text"), 
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2)
plot2
ggsave("CRH_OHP.pdf", plot2, width=8, height=6)
#Progesterone
prog1<- plot_quantile(d1_test, PROG1, OHP1)
prog2<- plot_quantile(d1_test, PROG2, OHP1)
prog3<- plot_quantile(d1_test, PROG1, OHP2)
prog4<- plot_quantile(d1_test, PROG2, OHP2)
plot3<- ggarrange(prog1, prog2, prog3, prog4 + rremove("x.text"), 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
plot3
ggsave("PROG_OHP.pdf", plot3, width=8, height=6)
# contCovarList1<- c('AGE','BMI','WT','HEIGHT')
# ContVCont1= Single_GA %>% pairs_plot(y=contCovarList1)+ rot_x(50)
# ContVCont1
# ggsave("SingleGA_ContCovar_PairPlot.png", height=6, width=7)
# 
# Cat<- c('RACE')
# Race_Plot1= Single_GA %>% wrap_cont_cat(x=Cat, y=contCovarList1, use_labels=TRUE)+ rot_x(35)
# Race_Plot1
# ggsave("Continuous_Categorical.png", Race_Plot, height=5, width=6)
# 
# contCovarList2<- c('AGE','BMI','WEIGHT','HEIGHT')
# ContVCont2= Multi_GA %>% pairs_plot(y=contCovarList2)+ rot_x(50)
# ContVCont2
# Race_Plot2= Multi_GA %>% wrap_cont_cat(x=Cat, y=contCovarList2, use_labels=TRUE)+ rot_x(35)
# Race_Plot2
# ggsave("MultiGA_ContCovar_PairPlot.png", ContVCont2, height=6, width=7)