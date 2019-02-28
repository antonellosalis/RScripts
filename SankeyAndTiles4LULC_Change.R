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

LULC2000 <- (read.csv(url("https://raw.githubusercontent.com/antonellosalis/RScripts/master/LULCtest.csv"), header = T, check.names=FALSE))

#List of the reference Years

Y1<-"Y1990"
Y2<-"Y2000"  

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
diag(LULC2000)<-NA
LULC2000<-rownames_to_column(LULC2000,"Classes")

# By melting the table I obtain a classic and easy to use Dataframe
LULC2000melt=melt(LULC2000,"Classes")
names(LULC2000melt)<-c(Y1,Y2,"Area")

# Here I am removing all the NAs
LULC2000melt[LULC2000melt==0] <- NA
LULC2000melt<-LULC2000melt[complete.cases(LULC2000melt),]

# In order to improve the visualzation I wrap the label text
LULC2000melt$Y1990<-wrap.labels(LULC2000melt$Y1990,12)
LULC2000melt$Y2000<-wrap.labels(LULC2000melt$Y2000,12)

# I may have the need to call only the Forest strata in my graphics, so I prepare a list (uncomment and edit it if needed)
#Forest<-c("Plantations","Tropical forest","Woodland")

# I prefer to show the LULC Change not as Area but as % of change
# I already know how much is the change, what I need is to better understand the dynamics
# I add a new column that shows in percent where in how each LULC classe has been changed
LULC2000melt<-LULC2000melt %>% group_by(Y1990) %>% mutate(Percrel = (Area/sum(Area) * 100))

# In order to have a better result I suggest to display no more that 2/3 classes at the time
ggplot(LULC2000melt[which(LULC2000melt$Y1990=="LULC1" | LULC2000melt$Y1990=="LULC2"),],
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



#Tileplots
Forest2000<-LULC2000melt[LULC2000melt$Y1990 %in% Forest,]

p <- ggplot(data =  LULC2000melt, aes(x = Y1990, y = Y2000)) +
  geom_tile(aes(fill = Percrel), colour = "white") +
  geom_text(aes(label = sprintf("%1.2f",Percrel)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "red")

p

#FOREST (works only if you set-up the Forest List)
#p <- ggplot(data =  Forest2000, aes(x = Y1990, y = Y2000)) +
#  geom_tile(aes(fill = Percrel), colour = "white") +
#  geom_text(aes(label = sprintf("%1.2f",Percrel)), vjust = 1) +
#  scale_fill_gradient(low = "white", high = "red")

#p


