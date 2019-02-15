##############################################################
# 
# Sankey diagram tests (csv version) for LULC maps
# R
# 2019
#                      
# Antonello Salis
# antonello.salis@fao.org
# antonellosalis@gmail.com
# last version 20190215
#
###############################################################

#import required packages
require(ggplot2)
require(ggalluvial)
require(ggrepel)
require(networkD3)
require(dplyr)
require(tidyr)

LULCData<-read.csv("Data/SankeyDATA.csv", header = T)

#These colours are necessary only if you want to change the default ones (then uncomment or comment accrdingly)
# A_col <- "#a6cee3"
# B_col <- "#1f78b4"
# C_col <- "#b2df8a"
# D_col <- "#33a02c"
# E_col <- "#fb9a99"
# F_col <- "#e31a1c"
# G_col <- "#fdbf6f"
# H_col <- "#ff7f00"
# I_col <- "#cab2d6"
# J_col <- "#6a3d9a"
# K_col <- "#ffff99"
# L_col <- "#b15928"
# 
# alpha <- 0.5

# 1st Graphic with B/W labels
#############################
ggplot(LULCData,
       aes(y = (Area), axis1 = LULC_2014, axis2 = LULC_2016)) +
  geom_alluvium(aes(fill = LULC_2016, color = LULC_2016), 
                width = 1/12, alpha = alpha, knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
# the following instruction iis necessary to avoid overlap in labeling
    geom_label_repel(stat = "stratum", label.strata = TRUE, size=3) +
  # geom_label_repel(label.strata=T,stat = "stratum", size = 4, direction = "y", nudge_x =.05)
  scale_x_continuous(breaks = 1:2, labels = c("LULC 2014", "LULC 2016"))+
  scale_y_discrete(labels = element_blank())+
  # scale_fill_manual(values  = c(A_col, B_col, C_col, D_col, E_col, F_col, G_col, H_col , I_col, J_col, K_col, L_col)) +
  # scale_color_manual(values = c(A_col, B_col, C_col, D_col, E_col, F_col, G_col, H_col , I_col, J_col, K_col, L_col)) +
  ggtitle("Land Use Land Cover Change between 2014 and 2016 (ha)") +
  theme_minimal() +
  theme(
  axis.text.x = element_text(size = 8, face = "bold"),   axis.text.y = element_blank())

# 2nd Graphic with coloured labels
##################################
ggplot(LULCData,
       aes(y = (Area), axis1 = LULC_2014, axis2 = LULC_2016, color=LULC_2016)) +
  geom_alluvium(aes(fill = LULC_2016, color = LULC_2016), 
                width = 1/12, alpha = alpha, knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  geom_label_repel(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:2, labels = c("LULC 2014", "LULC 2016"))     +
  # scale_fill_manual(values  = c(A_col, B_col, C_col, D_col, E_col, F_col, G_col, H_col , I_col, J_col, K_col, L_col)) +
  # scale_color_manual(values = c(A_col, B_col, C_col, D_col, E_col, F_col, G_col, H_col , I_col, J_col, K_col, L_col)) +
  ggtitle("Land Use Land Cover Change between 2014 and 2016 (ha)") +
  theme_minimal() +
  theme(
  axis.text.x = element_text(size = 8, face = "bold"),   axis.text.y = element_blank())


# 3rd Using another package than networkD3 package
##################################################



LULC_2014<-unique(LULCData$LULC_2014)
LULC_2016<-unique(LULCData$LULC_2016)
nodes<-data.frame(node = c (0:(length(LULC_2014)+length(LULC_2016)-1)), name = c(as.character(LULC_2014),as.character(LULC_2016)))
# Colors<-c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99","#b15928")


LULCChange <- merge(LULCData, nodes, by.x = "LULC_2014", by.y = "name")
LULCChange <- merge(LULCChange, nodes, by.x = "LULC_2016", by.y = "name")
links <- LULCChange[ , c("node.x", "node.y", "Area")]
colnames(links) <- c("source", "target", "value")

# draw sankey network

networkD3::sankeyNetwork(Links = links, Nodes = nodes, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'name',
                         units = 'Area',
                         fontFamily = 'sans',
                         fontSize = 12
                         )



