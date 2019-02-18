##############################################################
# 
# Sankey diagram tests (csv version) for LULC maps
# R
# 2019
#                      
# Antonello Salis
# antonello.salis@fao.org
# antonellosalis@gmail.com
# last version 20190216
#
###############################################################

#import required packages
require(ggplot2)
require(ggalluvial)
require(ggrepel)
require(networkD3)
require(dplyr)
require(tidyr)
dev.off()

LULCData<-read.csv("Data/SankeyDATA.csv", header = T)

# 1st Graphic with B/W labels, link colours depending on the Area sixe
#############################
ggplot(LULCData,
       aes(y =Area, axis1 = LULC_2014, axis2 = LULC_2016))+
  geom_alluvium(aes(fill=Area), width = 1/12, alpha = 0.5, knot.pos = 0.4)+
  geom_stratum(width = 1/6, color = "grey") +
  # the following instruction iis necessary to avoid overlap in labeling
  geom_label_repel(stat = "stratum", label.strata = TRUE, size=3) +
  # geom_label_repel(label.strata=T,stat = "stratum", size = 4, direction = "y", nudge_x =.05)
  scale_x_discrete(breaks = 1:2, labels = c("LULC 2014", "LULC 2016"))+
  # scale_y_discrete(labels = element_blank())+
  # scale_fill_manual(values  = c(A_col, B_col, C_col, D_col, E_col, F_col, G_col, H_col , I_col, J_col, K_col, L_col)) +
  # scale_color_manual(values = c(A_col, B_col, C_col, D_col, E_col, F_col, G_col, H_col , I_col, J_col, K_col, L_col)) +
  ggtitle("Land Use Land Cover Change between 2014 and 2016 (ha)") +
  theme_minimal() +
  theme(
  axis.text.x = element_text(size = 8, face = "bold"),   axis.text.y = element_blank())

# 2nd Graphic with coloured labels, colour depending on the LULC 2016
##################################
ggplot(LULCData,
       aes(y = (Area), axis1 = LULC_2014, axis2 = LULC_2016, color=LULC_2016)) +
  geom_alluvium(aes(fill = LULC_2016, color = LULC_2016), 
                width = 1/12, alpha = 0.5, knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  geom_label_repel(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(breaks = 1:2, labels = c("LULC 2014", "LULC 2016"))     +
  ggtitle("Land Use Land Cover Change between 2014 and 2016 (ha)") +
  theme_minimal() +
  theme(
  axis.text.x = element_text(size = 8, face = "bold"),   axis.text.y = element_blank())


# 3rd Graphic using the networkD3 package
##################################################
#This method implies the creation of two separate datasets: "nodes" and "links"
LULC2014<-unique(LULCData$LULC_2014)
LULC2016<-unique(LULCData$LULC_2016)
nodes<-data.frame(node = c (0:(length(LULC2014)+length(LULC2016)-1)), name = c(as.character(LULC2014),as.character(LULC2016)))
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



