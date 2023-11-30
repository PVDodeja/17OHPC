#Data Exploration OPRC
#https://finchstudio.io/blog/exposure-response-plots/
#https://opensource.nibr.com/xgx/Multiple_Ascending_Dose_PKPD_continuous.html


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
library(binom) 
#load source data
setwd("E:/PHD/17OHPC/Prerna/Datasets/OPRC_250_500")
getwd()
d1<-  read.csv("OPRU_PD_Data_Subset_Clean.csv") %>% select(-c(2)) # dropping the STUDY ID Column
d1_test<- d1 %>% mutate_at(1:23, as.numeric) #convert all to numeric

d1_quant<- d1_test %>% select(c(1,2, 4, 16:23)) %>% group_by(HPC_QUART)

#how is the data distributed
breaks <- quantile(d1_test$OHPC_26_30WK, na.rm=TRUE)
quantile(d1_test$LOG_OHPC_26_30WK, na.rm=TRUE)

#Partitioning the concentrations into quartiles and summarize
# Use cut function to place each subjects exposure into the correct bin
d1_quant <- d1_quant %>%
  mutate(quantile = cut(OHPC_26_30WK, breaks, include.lowest = TRUE))


#summarize and calculate median concentration for each quantile
summary_data<- function(data_for_summarize, response, response_n, response_perc)
  {
  
  sum1<- data_for_summarize %>%
  group_by(quantile) %>%
  summarise(median_data = median(OHPC_26_30WK),  
              n = n(),
             response_n = sum({{response_n}}),
             response_perc = sum({{response_perc}}/n()*100),
           ci = binom.confint(sum({{response}}), n(),
                               conf.level = 0.95, methods = "exact"))
print(sum1)
}

summary_data(d1_quant, PTB32, PTB32_n, PTB32_perc)

#Conc-Response Quantile Plot
plot_quant<- function(data_for_plot, conc, response)
{
  ggplot({{data_for_plot}}, aes(x = {{conc}}, y = {{response}})) +
  geom_col(aes(fill = "purple4"), alpha = 0.6) +
  geom_text(aes(label = paste0("(",PTB37_n,"/", n, ")")), vjust = -0.5, color = "grey30", size = 3,
            position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(PTB37_perc,1),"%")), vjust = -1.9, color = "grey30", size = 3,
            position = position_dodge(width = 0.9)) +
  labs(x = "OHPC Ctrough Quantile (ng/mL)",
       #y = "",
       title = "OHPC Ctrough Quantile Plot",)+
       #subtitle = paste(n_quantile, "Bins") ) +
  scale_y_continuous(limits = c(0, 100), breaks=c(seq(0,100,25))) +
  scale_fill_manual(values = c( "purple4", "grey20")) + theme_cowplot()+
  guides(fill = "none")
}

PTB1<- plot_quant(summary, quantile, PTB37_perc)
ggsave("OHPC_Quantile_PTB37Wk.png", PTB1, height=4, width=5)


# Exposure Response Logistic Regression Plot
logit_plot <- ggplot(summary, aes(x = median_OHPC_26_30WK, y = PTB37_perc/100)) +
  geom_point() +
  geom_errorbar(aes(ymax = ci$upper, ymin = ci$lower)) +
  geom_jitter(data = d1_quant,
              aes(x = OHPC_26_30WK,y=PTB37, color = HPC_QUART),
              height = 0.05,
              alpha = 0.5) +
  geom_smooth(data = d1_quant,
              aes(x = OHPC_26_30WK,y =PTB37),
              color = "blue",
              method = "glm",
              method.args = list(family = "binomial")) + theme_cowplot()+
  labs(x= "17OHPC Ctrough (ng/mL)", y = "Probability of PTB at 37 Weeks") 
  #scale_x_continuous(limits= c(-1, 125))
logit_plot
ggsave("Logit_MedianOHPC_PTB37.png", logit_plot, height=4, width=6)

# # Summary plot of exposures
# exposure_plot <- ggplot(summary, aes(x = median_OHPC_26_30WK), fill = DOSE, color = DOSE) +
#   #facet_wrap(~DOSE)+
#   geom_point()+
#  # scale_x_binned() +
#   labs(x= "OHPC Ctrough (ng/mL)", y ="Gestational Age" ) +
#   guides(color = "none", fill = "none")
# exposure_plot



# Function for scatter plots
plot_scatter<-function(data_for_plot, biom, conc)
{
  p3<- ggplot(data_for_plot, aes({{biom}}, {{conc}}))+
    geom_point()
  p3+theme_cowplot()
}
# Function for quantile plots
Q<- c(0.25,0.5,0.75,1.00)
plot_quantile<-function(data_for_plot, biom, conc)
{
  p1<- ggplot(data_for_plot, aes({{biom}}, {{conc}}))+
    geom_point()+
    scale_y_continuous(limits=c(0,20), n.breaks=15)
  p1+ geom_quantile(quantiles=Q, colour = "blue", linewidth =1, linetype="dashed" ,alpha = 0.5)+theme_cowplot()
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
    scale_x_binned()+
    geom_bar()
  p2
}


#looking at PARITY as a factor on OHP concentrations?
d1_test$PARITY <- as.factor(d1_test$PARITY)
plot_OHP1<- ggplot(d1_test, aes(x=PARITY, y=OHP1))
plot_OHP1<- plot_OHP1+ geom_boxplot()+theme_cowplot()


plot_OHP2<- ggplot(d1_test, aes(x=PARITY, y=OHP2))
plot_OHP2<- plot_OHP2+ geom_boxplot()+theme_cowplot()



#Pair Plot for Continuous Demographic Variables
contCovarList<- c('MAT_AGE','BMI')
ContVCont1= d1_test %>% pairs_plot(y=contCovarList)+ rot_x(50)
ContVCont1

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

plot_scatter(d1_test, CAP1, OHP1)
plot_scatter(d1_test, CAP2, OHP2)

plot_scatter(d1_test, CRP1, CAP1)
plot_scatter(d1_test, CRP2, CAP2)
plot_scatter(d1_test, CRH1, CAP1)

#Call function
plot_hist(d1_test, GESTAGE_PTB)
plot_hist(d1_test, OHPC_26_30WK)


#Callfunction
plot_box(d1_test, BMI)


plot_scatter(d1_test, OHPC_26_30WK, GESTAGE_PTB)
plot_quantile(d1_test, GATEST2, CAP2)
plot_quantile(d1_test, CRH2, CAP2)
plot_quantile(d1_test, PROG1, CAP1)
plot_quantile(d1_test, PROG2, CAP2)

CRP<- plot_quantile(d1_test, CRP1, CAP1)
CRP
ggsave("CRP_CAP.png", CRP, width=6, height=5)

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