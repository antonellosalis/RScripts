##############################################################
# 
# Simple dataset import (csv version)
# R Training
# 2019
#                      
# Antonello Salis
# antonello.salis@fao.org
# antonellosalis@gmail.com
# last version 20190122
#
###############################################################

rm(list = ls())
dev.off()
path<-setwd("FULL PATH HERE")
path


# import all the csv files
for(i in list.files(pattern = ".csv")) { 
  x <- read.csv((i));
      assign(i, x);
      for (i in ls(pattern = ".csv")) 
      { x <- sub(".csv",'',i) ; assign(x,get(i)); rm(list=ls(pattern = ".csv"));rm(x); rm(i)}
  }

# Import only trees data

names(tree)
