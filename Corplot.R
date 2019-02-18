##############################################################
# 
# Corplot script to diplay confusion matrices
#                       
# Antonello Salis
# antonello.salis@fao.org
# antonellosalis@gmail.com
# last version 20190217
#
###############################################################

require(corrplot)
M<- read.csv("/home/antonello/R_projects/NFI_Uganda/Data/LULC/LULC2017_Uganda.csv", header = F)
Classes <-as.vector(read.csv("/home/antonello/R_projects/NFI_Uganda/Data/LULC/Classes.csv", header = F))
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

Classes<-wrap.labels(Classes,20)

M<-as.matrix(M)
M<-cor(M)
colnames(M) <- Classes$V1
rownames(M) <- Classes$V1
corrplot(M, method = "color",cl.lim = c(min(M), max(M)), tl.col = "black", tl.srt = 45)
