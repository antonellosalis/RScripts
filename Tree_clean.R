##############################################################
# 
# Dataclean script to identify errors and outliars in NFI datasets
# R Training
# 2019
#                      
# Antonello Salis
# antonello.salis@fao.org
# antonellosalis@gmail.com
# last version 20190122
#
###############################################################

# library(ggExtra)
library(ggplot2)
library(gridExtra)
library(validate)
dev.off()
rm(list = ls())

############################################################
################## DATA IMPORT##############################
############################################################
#PLEASE: Adapt the variable names according to the needs main dataset is called "tree"

path<-setwd("FULL PATH HERE")
path


# import all the csv files
for(i in list.files(pattern = ".csv")) { 
  x <- read.csv((i));
  assign(i, x);
  for (i in ls(pattern = ".csv")) 
  { x <- sub(".csv",'',i) ; assign(x,get(i)); rm(list=ls(pattern = ".csv"));rm(x); rm(i)}
}

tree$tree_height
############################################################
################## DATA Exploration ########################
############################################################
#1.Check the marginals
p = ggplot(tree, aes(x=diameter,y=total_height)) + 
    geom_point( alpha=I(1/4)) +
    geom_smooth(method="lm")
p
#### STOP HERE AND ANALYZE THE TREE DBH/HEIGHT GRAPHIC ####

#1.1. default: type="density", it only works if ggExtra is installed
# ggMarginal(p, type="density", colour = "black")

# 1.2 We first eliminate stumps and in this phase we will only consider "non stumps" values
tree_clean<-tree[which(tree$stump=="false"),]

# The next step is to give to each tree a unique value (ID)
tree_clean$ID <- c(paste('T', tree_clean$tree_no, tree_clean$cluster_plotno, sep  = '_'))

p = ggplot(tree_clean, aes(x=diameter,y=total_height)) + 
    geom_point( alpha=I(1/4)) +
    geom_text(aes(label=ID))+
    geom_smooth(method="lm")
p


# Two histograms for DBH and height
qplot(tree_clean$diameter,
      geom="histogram", 
      binwidth = 5
)

qplot(tree_clean$total_height,
      geom="histogram", 
      binwidth = 1
)


# tree_clean<-tree_clean[!(tree_clean$total_height>45),]

############################################################
################## DATA Validation #########################
############################################################
# How many suspect values there are in our datasets?
# We check also non dbh and non height values starting by:
# Ratio height/dbh
ggplot(tree_clean, aes(x = total_height/diameter, y=seq_along(total_height/diameter))) + geom_point( alpha=I(1/4)) +
geom_text(aes(label=ID))
  
# diameter_30/diameter
# ggplot(tree_clean, aes(x = diameter_30/diameter, y=seq_along(diameter_30/diameter))) +geom_point( alpha=I(1/4))

Valid_tree<-validator(total_height > 1.3
             , total_height < 40
             # , diameter < 10
             , diameter > 0
             , diameter < 250
             , diameter/total_height > 1
             # , tree_bole_height/diameter<1
             # , diameter_30/diameter>=1
             # , branch1_diameter>=20
             # , branch2_diameter>=20
             # , branch3_diameter>=20
             # , branch4_diameter>=20
             # , branch1_length>=2
             # , branch2_length>=2
             # , branch3_length>=2
             # , branch4_length>=2
             )

Result_valid_tree<-confront(tree_clean,Valid_tree)
Summary_valid_tree<-summary(Result_valid_tree)
barplot(Result_valid_tree)
View(Summary_valid_tree)


# # 1.1 identify suspect trees
# #  We create 4 datsets with the most suspect values
#
Suspect_values_ratio<- tree_clean[which(tree_clean$total_height/tree_clean$diameter>1),]
Suspect_values_DBH<-merge(Suspect_values, tree_clean[which(tree_clean$diameter>250),], all.x = T)
Suspect_values_DBH_NA<-tree[which(is.na(tree$diameter)),]
Suspect_values_Heights<-merge(Suspect_values, tree_clean[which(tree_clean$total_height>40),], all.x = T)


# Export each Suspect dataset on csv.
Suspects_list<-ls(pattern = "Suspect_")
# save each new data frame as an individual .csv file based on its name

mainDir <- c('/home/antonello/R_projects/NFI_Uganda/')
subDir <- "Suspect_data"

dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))
getwd()

lapply(1:length(Suspects_list), function(i) write.csv(get(Suspects_list[[i]]), 
                                                      file = paste0(Suspects_list[i], ".csv"),
                                                      row.names = FALSE))

setwd(mainDir)

#Plotting the Suspect values
p = ggplot(Suspect_values_ratio, aes(x=diameter,y=total_height)) + 
  geom_point( alpha=I(1/4)) +
  geom_text(aes(label=ID))+
  geom_smooth(method="glm")+
  ggtitle("Plot of suspect ratios")

p




#####################################################################################
############### Check and clean the tree_clean dataset before continuing ############
#####################################################################################

#Same graphs as above to check if the data is now cleaned
p = ggplot(tree_clean, aes(x=diameter,y=total_height)) + 
  geom_point( alpha=I(1/4)) +
  geom_text(aes(label=ID))+
  geom_smooth(method="lm")
p


# Two histograms for DBH and height
qplot(tree_clean$diameter,
      geom="histogram", 
      binwidth = 5
)

qplot(tree_clean$total_height,
      geom="histogram", 
      binwidth = 1
)


#Re-check of the outliars using the sequential number of all the trees


p <- ggplot(tree_clean, aes(x=diameter, y=total_height, label=ID))
p <- p + geom_text(cex=5)
p


#Only trees with total_height>40 by Tree number
p <- ggplot(Suspect_values_Heights, aes(x=diameter, y=total_height, label=ID))
p <- p + geom_text(cex=5)
p

#Only trees with total_height>40 by family

p <- ggplot(Suspect_values_Heights, aes(x=diameter, y=total_height, label=Suspect_values$species_scientific_name))
p <- p + geom_text(cex=5)
p


#Logaritmic scale
op = par(mfrow = c(1, 2))
plot(total_height~diameter, data = tree_clean, pch = 1, cex = 1, col ="lightseagreen", xlab = "diameter")
plot(total_height~diameter, data = tree_clean, pch = 1, cex = 1, col ="lightseagreen", xlab = "diameter (log scale)", log = "x")
par(op)

# Density (each color is a specie)
plot(total_height~diameter, data = tree_clean, 
     cex = tree$WD*2.5, col = tree_clean$species_scientific_name)


# Plot average of DBH and height
plot(total_height~diameter, data = tree_clean, type = "n")
points(total_height~diameter, data = tree_clean, pch = 16, col = "darkblue")
abline(h = mean(tree_clean$total_height), v = mean(tree_clean$diameter),
       col = "gray80", lwd = 2)
grid()

p <- qplot(diameter, total_height, data = tree_clean) + geom_hex()
p 

