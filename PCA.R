#==========================================
# Title:  PCA
# Author: Matthieu Rouland
# Affiliation: Institut Cochin, INSERM U1016, CNRS UMR 8104, Université de Paris, Paris, France.
# Acknowledgments: Dr. Marc Diedisheim for advice and coding help.
# License: CC-BY-SA-4.0 
# Date: 20.08.2020
# Doi: https://doi.org/10.1038/s41590-021-00870-z
#==========================================

#--Settings--#
library(ggplot2)
library(readxl)
library(writexl)
library(ggpubr)
library(purrr)
library(dplyr)
library(ggrepel)
library(corrplot)
library(Hmisc)
library(psych)
library(RcmdrMisc)
library(PerformanceAnalytics)
library(GGally)
library(FactoMineR)
library(factoextra)
setwd("")
set.seed (1234)
data <- read_excel("data.xlsx")
#--------------#

group <- data[,4]
visu <- data[,1]
data_numeric_surface <- select(data,7:59)

res.pca_surface <- PCA(data_numeric_surface, ncp = 9,scale.unit = TRUE,graph = FALSE)
print(res.pca_surface)


pdf(file = paste0("PCA-", Sys.Date(), ".pdf"), width = 11, height = 11)
for (i in 1:9){ 
  for (j in 1:9){
    if (j != i){
      print((fviz_pca_ind(res.pca_surface,
                       geom.ind = "point", # Show only dots
                       col.ind = data$DeathService, # colored by COVID-19 groups and disease outcomes
                       palette = c("#FC4E07","#00AFBB", "black", "black"),
                       pointsize = 2,
                       addEllipses = FALSE, ellipse.level = 0.95, #Concentration ellipses 
                       legend.title = "Groups",
                       axes = c(i, j),
                       mean.point = TRUE,
                       mean.point.size = 6))+ 
              scale_shape_manual(values=c(16, 16, 17, 15)))

      
     print(fviz_pca_ind(res.pca_surface,
                         geom.ind = "text", # show text only
                        col.ind = data$DeathService, # colored by COVID-19 groups and disease outcomes
                        palette = c("#FC4E07","#00AFBB", "black", "black"),
                        addEllipses = FALSE, ellipse.level = 0.95, # Ellipses de concentration
                         legend.title = "Groups",
                         axes = c(i, j)))

      print(fviz_pca_var(res.pca_surface, 
                       axes = c(i, j),
                       geom = c("text"),
                       col.ind = data$DeathService, # colored by COVID-19 groups and disease outcomes
                       label = "all", invisible = "none", labelsize = 2,
                       col.var = "black", alpha.var = 1, col.quanti.sup = "blue",
                       col.circle = "grey70",
                       select.var = list(name =NULL, cos2 = NULL, contrib = NULL),
                       repel = TRUE))

      print(fviz_pca_biplot(res.pca_surface, label="var",
                            axes = c(i, j),
                            select.var = list(contrib = 40),
                            palette = c("orange","purple"),
                            ellipse.alpha = 0.01,
                            col.ind = data$Death, # colored by COVID-19 disease outcomes
                            col.var = "black", alpha.var = 1, col.quanti.sup = "blue",
                            labelsize = 3,
                            alpha.arrow = 0.10,
                            pointsize = 0,
                            addEllipses = TRUE, ellipse.level = 0.95, arrowsize = 0.0001,
                            repel = TRUE, 
                            alpha.ind = 0,
                            mean.point.size = 5,
                            legend.title =  "Groups"
      )) 
      
      print(fviz_contrib(res.pca_surface, choice ="var", axe = c(i,j), 
                   top=60)) #Show variable contribution in COVID-19 cohort

      print(i)
      print(j)     
           }
       }
    }
dev.off()


