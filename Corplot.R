##############################################################
# 
# Corplot script to diplay confusion matrices
# R Training, Ethiopia
# 2017
#                      
# Antonello Salis
# antonello.salis@fao.org
# antonellosalis@gmail.com
# last version 20170218
#
###############################################################


setwd("/home/antonello/R_projects/AA/")
require(corrplot)
M<- read.csv("Data/Ethiopia_Accuracy_Matrix_4.csv", header = F)
M

Magg<-read.csv("Data/Ethiopia_Accuracy_Matrix_Aggregate_1.csv", header = F)

M<-as.matrix(M)
rownames(M) <- c("agriculture","grassland","scrubland","shrubland","open_woodland","dense_woodland","forest","bareland","builtup","afroapline","plantation","saltpan","wetland","bamboo","riverine","water")
colnames(M) <- c("agriculture","grassland","scrubland","shrubland","open_woodland","dense_woodland","forest","bareland","builtup","afroapline","plantation","saltpan","wetland","bamboo","riverine","water")
corrplot(M, method = "circle",cl.lim = c(0, 1))

corrplot(M, method = "number")
corrplot(M, method = "number",cl.lim = c(0, 1))
corrplot(M, method = "color",cl.lim = c(0, 1))



Magg<-as.matrix(Magg)
rownames(Magg) <- c("agriculture","grassland","shrubland","forest","built up","bareland","saltpan","water body"
)
colnames(Magg) <-  c("agriculture","grassland","shrubland","forest","built up","bareland","saltpan","water body"
)
corrplot(Magg, method = "color",cl.lim = c(0, 1))
corrplot(Magg, method = "number",cl.lim = c(0, 1))

M1<- read.csv("Data/Ethiopia_Accuracy_Matrix_5.csv", header = F)
M1<-as.matrix(M1)
rownames(M1) <- c("agriculture","grassland","scrubland","shrubland","open_woodland","dense_woodland","forest","bareland","builtup","afroapline","plantation","saltpan","wetland","bamboo","riverine","water")
colnames(M1) <- c("agriculture","grassland","scrubland","shrubland","open_woodland","dense_woodland","forest","bareland","builtup","afroapline","plantation","saltpan","wetland","bamboo","riverine","water")
corrplot(M1, method = "number",cl.lim = c(0, 1))
corrplot(M1, method = "color",cl.lim = c(0, 1))


