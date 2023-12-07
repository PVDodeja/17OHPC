library(haven)

rm(list=ls(all.names=TRUE)) 
#load source data
setwd("E:/PHD/17OHPC/Prerna/Datasets/MFMU")
getwd()

library(data.table)
library(foreign)


d1<- read_xpt("E:/PHD/17OHPC/Prerna/Datasets/MFMU/STTARS_TWINS.XPT")


#Sorry to say that R can't read compressed SAS files yet. 
#I had to use another software to convert the compressed SAS file into an uncompressed SAS file,
#then was able to read into R.