#################################################################
# 
# Sankey diagrams and Matrix plots to analyse LULC Changes
# More descriptions here: https://github.com/antonellosalis/RScripts/wiki/Land-Use-Land-Cover-Changes-graphics
# More scripts here: https://github.com/antonellosalis/RScripts
# You can use LULCtest.csv to understand the structure needed
# R
# 2019
#                      
# Antonello Salis
# antonello.salis@fao.org
# antonellosalis@gmail.com
# last version 20190227
#
#################################################################

require(reshape)
require(corrplot)
require(igraph)
require(networkD3)
require(ggplot2)
require(ggalluvial)
require(ggrepel)
require(networkD3)
require(dplyr)
require(tidyr)
require(tibble)

rm(list=ls())

# Import CSVs

LULC2000 <- (read.csv("Data/LULC/1990_2000.csv", header = T, check.names=FALSE))
LULC2005 <- (read.csv("Data/LULC/2000_2005.csv", header = T, check.names=FALSE))
LULC2010 <- (read.csv("Data/LULC/2005_2010.csv", header = T, check.names=FALSE))
LULC2015 <- (read.csv("Data/LULC/2010_2015.csv", header = T, check.names=FALSE))

#List of the reference Years

Y1<-"Y1990"
Y2<-"Y2000"  
Y3<-"Y2005"
Y4<-"Y2010"
Y5<-"Y2015"

# funtion that I'll use later to Wrap the labels.
wrap.it <- function(x, len)
{
  sapply(x, function(y) paste(strwrap(y, len),
                              collapse = "\n"),
         USE.NAMES = FALSE)
}


# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

#I am removing all the Stable Land Use by removing the autocorrelations in the matrix
# To do that  have to transform a bit the table
LULC2000<-column_to_rownames(LULC2000,"Classes")
LULC2005<-column_to_rownames(LULC2005,"Classes")
LULC2010<-column_to_rownames(LULC2010,"Classes")
LULC2015<-column_to_rownames(LULC2015,"Classes")

diag(LULC2000)<-NA

diag(LULC2005)<-NA

diag(LULC2010)<-NA

diag(LULC2015)<-NA

LULC2000<-rownames_to_column(LULC2000,"Classes")
LULC2005<-rownames_to_column(LULC2005,"Classes")
LULC2010<-rownames_to_column(LULC2010,"Classes")
LULC2015<-rownames_to_column(LULC2015,"Classes")

# By melting the table I obtain a classic and easy to use Dataframe
LULC2000melt=melt(LULC2000,"Classes")
names(LULC2000melt)<-c(Y1,Y2,"Area")

LULC2005melt=melt(LULC2005,"Classes")
names(LULC2005melt)<-c(Y2,Y3,"Area")

LULC2010melt=melt(LULC2010,"Classes")
names(LULC2010melt)<-c(Y3,Y4,"Area")

LULC2015melt=melt(LULC2015,"Classes")
names(LULC2015melt)<-c(Y4,Y5,"Area")

# Here I am removing all the NAs
LULC2000melt[LULC2000melt==0] <- NA
LULC2000melt<-LULC2000melt[complete.cases(LULC2000melt),]

LULC2005melt[LULC2005melt==0] <- NA
LULC2005melt<-LULC2005melt[complete.cases(LULC2005melt),]

LULC2010melt[LULC2010melt==0] <- NA
LULC2010melt<-LULC2010melt[complete.cases(LULC2010melt),]

LULC2015melt[LULC2015melt==0] <- NA
LULC2015melt<-LULC2015melt[complete.cases(LULC2015melt),]

# In order to improve the visualzation I wrap the label text

LULC2000melt$Y1990<-wrap.labels(LULC2000melt$Y1990,12)
LULC2000melt$Y2000<-wrap.labels(LULC2000melt$Y2000,12)

LULC2005melt$Y2000<-wrap.labels(LULC2005melt$Y2000,12)
LULC2005melt$Y2005<-wrap.labels(LULC2005melt$Y2005,12)

LULC2010melt$Y2005<-wrap.labels(LULC2010melt$Y2005,12)
LULC2010melt$Y2010<-wrap.labels(LULC2010melt$Y2010,12)

LULC2015melt$Y2010<-wrap.labels(LULC2015melt$Y2010,12)
LULC2015melt$Y2015<-wrap.labels(LULC2015melt$Y2015,12)

# I may have the need to call only the forest in my graphics, so I prepare a lis
Forest<-c("Plantations\nconiferous","Tropical\nhigh forest\nwell-stocked","Tropical\nhigh forest\nlow-stocked","Plantations\nbroadleaved","Woodland")

# I prefer to show the LULC Change not as Area but as % of change
# I already know how much is the change, what I need is to better understand the dynamics
# I add a new column that shows in percent where in how each LULC classe has been changed

LULC2000melt<-LULC2000melt %>% group_by(Y1990) %>% mutate(Percrel = (Area/sum(Area) * 100))

# In order to have a better result I suggest to display no more that 2/3 classes at the time

ggplot(LULC2000melt[which(LULC2000melt$Y1990=="Tropical\nhigh forest\nlow-stocked" | LULC2000melt$Y1990=="Tropical\nhigh forest\nwell-stocked"),],
       aes(y = Percrel, 
           axis1 = Y1990, 
           # This command will agregate in the classe "Other" all the changed classes < 2%
           axis2 = ifelse(Percrel<2,"Other",Y2000))) +
  geom_alluvium(aes(fill = Y2000, color = Y2000), 
                width = 1/12,  knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  # If labels still overlaps you can consider to use geom_label_repel option instead than geom_text
  geom_text(stat = "stratum", label.strata = TRUE, size=3) +
  # geom_label_repel(label.strata=T,stat = "stratum", size = 4, direction = "y", nudge_x =.05)+
  scale_x_continuous(breaks = 1:2, labels = c("LULC 1990", "LULC 2000"))+
  scale_y_discrete(labels = element_blank())+
  ggtitle("Land Use Land Cover Change between 1990 and 2000 (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, face = "bold"),   axis.text.y = element_blank())

LULC2005melt<-LULC2005melt %>% group_by(Y2000) %>% mutate(Percrel = (Area/sum(Area) * 100))

ggplot(LULC2005melt[which(LULC2005melt$Y2000=="Tropical\nhigh forest\nlow-stocked" | LULC2005melt$Y2000=="Tropical\nhigh forest\nwell-stocked"),],
       aes(y = Percrel, 
           axis1 = Y2000, 
           axis2 = ifelse(Percrel<2,"Other",Y2005))) + 
  geom_alluvium(aes(fill = Y2005, color = Y2005), 
                width = 1/12,  knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  # the following instruction iis necessary to avoid overlap in labeling
  geom_text(stat = "stratum", label.strata = TRUE, size=3) +
  # geom_label(label.strata=T,stat = "stratum", size = 4, direction = "y", nudge_x =.05)+
  scale_x_continuous(breaks = 1:2, labels = c("LULC 2000", "LULC 2005"))+
  scale_y_discrete(labels = element_blank())+
  ggtitle("Land Use Land Cover Change between 2000 and 2005 (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, face = "bold"),   axis.text.y = element_blank())

LULC2010melt<-LULC2010melt %>% group_by(Y2005) %>% mutate(Percrel = (Area/sum(Area) * 100))

ggplot(LULC2010melt[which(LULC2010melt$Y2005=="Tropical\nhigh forest\nlow-stocked" | LULC2010melt$Y2005=="Tropical\nhigh forest\nwell-stocked"),],
       aes(y = Percrel, 
           axis1 = Y2005, 
           axis2 = ifelse(Percrel<2,"Other",Y2010))) + 
  geom_alluvium(aes(fill = Y2010, color = Y2010), 
                width = 1/12,  knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  # the following instruction iis necessary to avoid overlap in labeling
  geom_text(stat = "stratum", label.strata = TRUE, size=3) +
  # geom_label(label.strata=T,stat = "stratum", size = 4, direction = "y", nudge_x =.05)+
  scale_x_continuous(breaks = 1:2, labels = c("LULC 2005", "LULC 2010"))+
  scale_y_discrete(labels = element_blank())+
  ggtitle("Land Use Land Cover Change between 2005 and 2010 (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, face = "bold"),   axis.text.y = element_blank())

LULC2015melt<-LULC2015melt %>% group_by(Y2010) %>% mutate(Percrel = (Area/sum(Area) * 100))

ggplot(LULC2015melt[which(LULC2015melt$Y2010=="Tropical\nhigh forest\nlow-stocked" | LULC2015melt$Y2010=="Tropical\nhigh forest\nwell-stocked"),],
       aes(y = Percrel, 
           axis1 = Y2010, 
           axis2 = ifelse(Percrel<2,"Other",Y2015))) + 
  geom_alluvium(aes(fill = Y2015, color = Y2015), 
                width = 1/12,  knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  # the following instruction iis necessary to avoid overlap in labeling
  geom_text(stat = "stratum", label.strata = TRUE, size=3) +
  # geom_label(label.strata=T,stat = "stratum", size = 4, direction = "y", nudge_x =.05)+
  scale_x_continuous(breaks = 1:2, labels = c("LULC 2010", "LULC 2015"))+
  scale_y_discrete(labels = element_blank())+
  ggtitle("Land Use Land Cover Change between 2010 and 2015 (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, face = "bold"),   axis.text.y = element_blank())

#CORRPLOTS
Forest2000<-LULC2000melt[LULC2000melt$Y1990 %in% Forest,]
Forest2005<-LULC2005melt[LULC2005melt$Y2000 %in% Forest,]
Forest2010<-LULC2010melt[LULC2010melt$Y2005 %in% Forest,]
Forest2015<-LULC2015melt[LULC2015melt$Y2010 %in% Forest,]

p <- ggplot(data =  LULC2000melt, aes(x = Y1990, y = Y2000)) +
  geom_tile(aes(fill = Percrel), colour = "white") +
  geom_text(aes(label = sprintf("%1.2f",Percrel)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "red")

p

#FOREST

p <- ggplot(data =  Forest2000, aes(x = Y1990, y = Y2000)) +
  geom_tile(aes(fill = Percrel), colour = "white") +
  geom_text(aes(label = sprintf("%1.2f",Percrel)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "red")

p


p <- ggplot(data =  Forest2005, aes(x = Y2000, y = Y2005)) +
  geom_tile(aes(fill = Percrel), colour = "white") +
  geom_text(aes(label = sprintf("%1.2f",Percrel)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "red")

p


p <- ggplot(data =  Forest2010, aes(x = Y2005, y = Y2010)) +
  geom_tile(aes(fill = Percrel), colour = "white") +
  geom_text(aes(label = sprintf("%1.2f",Percrel)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "red")

p

p <- ggplot(data =  Forest2015, aes(x = Y2010, y = Y2015)) +
  geom_tile(aes(fill = Percrel), colour = "white") +
  geom_text(aes(label = sprintf("%1.2f",Percrel)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "red")

p

# 
# 
# 
# LULC2000melt$Area[is.na(LULC2000melt$Area)] <- 0
# ggplot(LULC2000melt,
#        aes(x = (Year),
#            stratum = Y1990,
#            alluvium = Area,
#            y = Area,
#            fill=Y1990,
#            label=Y1990,
#            width = 1/12,
#            alpha = 0.5,
#            knot.pos = 0.4
#            )) +
#   # geom_alluvium(aes(fill = Area, color = LULC),
#   #               width = 1/12, alpha = alpha, knot.pos = 0.4) +
#   scale_x_discrete(expand = c(.1, .1)) +
#   geom_flow() +
#   geom_stratum(width = 1/6, color = "grey") +
#   # the following instruction iis necessary to avoid overlap in labeling
#   # geom_text(stat = "stratum", label.strata = TRUE, size=3) +
#   # geom_label_repel(label.strata=T,stat = "stratum", size = 4, direction = "y", nudge_x =.05)
#   # scale_x_continuous(breaks = 1:2, labels = c("LULC 2014", "LULC 2016"))+
#   # scale_y_discrete(labels = element_blank())+
#   # scale_fill_manual(values  = c(A_col, B_col, C_col, D_col, E_col, F_col, G_col, H_col , I_col, J_col, K_col, L_col)) +
#   # scale_color_manual(values = c(A_col, B_col, C_col, D_col, E_col, F_col, G_col, H_col , I_col, J_col, K_col, L_col)) +
#   ggtitle("Land Use Land Cover Change between 2014 and 2016 (ha)") +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(size = 8, face = "bold"),   axis.text.y = element_blank())
# 
# 

unique(LULC2000melt)
ggplot(LULC2010melt[which(LULC2010melt$Y2005=="Woodland" | LULC2010melt$Y2005=="Bushland" | LULC2010melt$Y2005=="Grassland" ),],
       aes(y = Percrel, 
           axis1 = Y2005, 
           axis2 = ifelse(Percrel<2,"Other",Y2010))) + 
  geom_alluvium(aes(fill = Y2010, color = Y2010), 
                width = 1/12,  knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  # the following instruction iis necessary to avoid overlap in labeling
  geom_text(stat = "stratum", label.strata = TRUE, size=3) +
  # geom_label(label.strata=T,stat = "stratum", size = 4, direction = "y", nudge_x =.05)+
  scale_x_continuous(breaks = 1:2, labels = c("LULC 2005", "LULC 2010"))+
  scale_y_discrete(labels = element_blank())+
  ggtitle("Land Use Land Cover Change between 2005 and 2010 (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, face = "bold"),   axis.text.y = element_blank())
