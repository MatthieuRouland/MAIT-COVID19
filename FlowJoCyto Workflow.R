#==========================================
# Title:  FlowJoCyto-Workflow 
# Author: Matthieu Rouland
# Affiliation: Institut Cochin, INSERM U1016, CNRS UMR 8104, Université de Paris, Paris, France.
# License: CC-BY-SA-4.0 
# Date: 20.08.2020
#==========================================

#------ Setup -----#
# install.packages("tidyverse")
# install.packages("writexl")
# install.packages("pheatmap")
#
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("cytoML")
# BiocManager::install("flowWorkspace")
# BiocManager::install("cytofWorkflow")


#------ Library -----#
library(flowWorkspace)
library(ggcyto)
library(CytoML)
library(writexl)
library(CATALYST)
library(cytofWorkflow)
library(dplyr)

#------ Reproducibility -----#
set.seed(1234)

#------ Directory -----#
setwd("")


# |||||||||||   MULTIPLE .WSP  ||||||||||||||||
# ----------------------------------------------------- WSP n.1
wsfilemulti <- list.files(getwd(), pattern="2 avril_edited.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti1 <- flowjo_to_gatingset(wsmulti,name = 1) 


# ----------------------------------------------------- WSP n.2
wsfilemulti <- list.files(getwd(), pattern="1-Surface Bichat pour intra-2020.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti2 <- flowjo_to_gatingset(wsmulti,name = 1)

#-----------------------------------------------------Wsp n.3
wsfilemulti <- list.files(getwd(), pattern="2-Surface Bichat pour intra-2020.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti3 <- flowjo_to_gatingset(wsmulti,name = 1)

#-----------------------------------------------------Wsp n.4
wsfilemulti <- list.files(getwd(), pattern="6 mi Analyse.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti4 <- flowjo_to_gatingset(wsmulti,name = 2) 

#-----------------------------------------------------Wsp nÂ°5
wsfilemulti <- list.files(getwd(), pattern="07-May-2020 Vanalyse_edited.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti5 <- flowjo_to_gatingset(wsmulti,name = 1, extend_val = -Inf)


#-----------------------------------------------------Wsp nÂ°6
wsfilemulti <- list.files(getwd(), pattern="7 Mai-2020.wsp Analyse.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti6 <- flowjo_to_gatingset(wsmulti,name = 1) 

#-----------------------------------------------------Wsp nÂ°7
wsfilemulti <- list.files(getwd(), pattern="9 avril.wsp Analyse.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti[2])
wsmulti
gsmulti7 <- flowjo_to_gatingset(wsmulti,name = 1) 


#-----------------------------------------------------Wsp nÂ°8
wsfilemulti <- list.files(getwd(), pattern="21-22 avril.wsp Analyse_edited4.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti8 <- flowjo_to_gatingset(wsmulti,name = 1) 


#-----------------------------------------------------Wsp nÂ°9
wsfilemulti <- list.files(getwd(), pattern="24 au 29 avril.wsp Analyse_edited2.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti9 <- flowjo_to_gatingset(wsmulti,name = 1) 

#-----------------------------------------------------Wsp nÂ°10
wsfilemulti <- list.files(getwd(), pattern="27-Apr-2020.wsp Analyse.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti10 <- flowjo_to_gatingset(wsmulti,name = 1) 

#-----------------------------------------------------Wsp nÂ°11
wsfilemulti <- list.files(getwd(), pattern="30 avril 4 mai .wsp Analyse.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti11 <- flowjo_to_gatingset(wsmulti,name = 1) 

#-----------------------------------------------------Wsp nÂ°12
wsfilemulti <- list.files(getwd(), pattern="All pop 12 Mai-2020.wsp Analyse_edited.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti12 <- flowjo_to_gatingset(wsmulti,name = 1)


#-----------------------------------------------------Wsp nÂ°13
wsfilemulti <- list.files(getwd(), pattern="1er avril-2020.wsp Analyse.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti13 <- flowjo_to_gatingset(wsmulti,name = 1)

#-----------------------------------------------------Wsp nÂ°14
wsfilemulti <- list.files(getwd(), pattern="27-mars-2020.wsp Analyse.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti14 <- flowjo_to_gatingset(wsmulti,name = 1)

#-----------------------------------------------------Wsp nÂ°15
wsfilemulti <- list.files(getwd(), pattern="28-Apr-2020.wsp Analyse.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti15 <- flowjo_to_gatingset(wsmulti,name = 1)


#-----------------------------------------------------Wsp nÂ°16
wsfilemulti <- list.files(getwd(), pattern="30 mars-2020.wsp Analyse.wsp", full = TRUE)
wsmulti <- open_flowjo_xml(wsfilemulti)
wsmulti
gsmulti16 <- flowjo_to_gatingset(wsmulti,name = 1)



#------ On prend les events de la gate que l'on souhaite ------
getgsdata1 <-gs_pop_get_data(gsmulti1, y = "MAIT", inverse.transform = FALSE)
getgsdata2 <-gs_pop_get_data(gsmulti2, y = "MAIT", inverse.transform = FALSE)
getgsdata3 <-gs_pop_get_data(gsmulti3, y = "MAIT", inverse.transform = FALSE)
getgsdata4 <-gs_pop_get_data(gsmulti4, y = "MAIT", inverse.transform = FALSE)
getgsdata5 <-gs_pop_get_data(gsmulti5, y = "MAIT", inverse.transform = FALSE)
getgsdata6 <-gs_pop_get_data(gsmulti6, y = "MAIT", inverse.transform = FALSE)
getgsdata7 <-gs_pop_get_data(gsmulti7, y = "MAIT", inverse.transform = FALSE)
getgsdata8 <-gs_pop_get_data(gsmulti8, y = "MAIT", inverse.transform = FALSE)
getgsdata9 <-gs_pop_get_data(gsmulti9, y = "MAIT", inverse.transform = FALSE)
getgsdata10 <-gs_pop_get_data(gsmulti10, y = "/Lymphocytes/Single Cells/Single Cells/CD3, B- subset/CD3, TCRyd neg/T cells/DN, CD8 subset/MAIT", inverse.transform = FALSE)
getgsdata11 <-gs_pop_get_data(gsmulti11, y = "MAIT", inverse.transform = FALSE)
getgsdata12 <-gs_pop_get_data(gsmulti12, y = "MAIT", inverse.transform = FALSE)
getgsdata13 <-gs_pop_get_data(gsmulti13, y = "MAIT", inverse.transform = FALSE)
getgsdata14 <-gs_pop_get_data(gsmulti14, y = "MAIT", inverse.transform = FALSE)
getgsdata15 <-gs_pop_get_data(gsmulti15, y = "MAIT", inverse.transform = FALSE)
getgsdata16 <-gs_pop_get_data(gsmulti16, y = "MAIT", inverse.transform = FALSE)

rm("gs_merge")
gs_merge <- rbind2(getgsdata1,getgsdata4)
gs_merge <- rbind2(gs_merge,getgsdata6)
gs_merge <- rbind2(gs_merge,getgsdata7)
gs_merge <- rbind2(gs_merge,getgsdata8)
gs_merge <- rbind2(gs_merge,getgsdata9)
gs_merge <- rbind2(gs_merge,getgsdata10)
gs_merge <- rbind2(gs_merge,getgsdata11)
gs_merge <- rbind2(gs_merge,getgsdata12)
gs_merge <- rbind2(gs_merge,getgsdata14)
gs_merge <- rbind2(gs_merge,getgsdata16) 
x <- length(gs_merge)

for (i in 1:x){
  keyword(gs_merge[[i]])[["$CYT"]] <- "FACS"
}

#------ Production and edition of the two main excel files needed for the workflow: Table.xlsx & Panel.xlsx
#------ Preparation des deux fichier excel pour le workflow : Table.xlsx & Panel.xlsx
df_file_name <- keyword(gs_merge, "$FIL")
colnames(df_file_name) <- c("file_name")

sample_id <- c("sample_id")
condition <- c("condition")
patient_id <- c("patient_id")
tmp_table <- data.frame(df_file_name, sample_id, condition, patient_id)
tmp_table
write_xlsx(tmp_table, "tmp_table.xlsx", col_names = TRUE)

df_fcs_colname <- colnames(gs_merge)
colnames(df_fcs_colname) <- colnames("fcs_colname")
df_antigen <- c("antigen")
df_marker_class <- c("marker_class")
tmp_panel <- data.frame(df_fcs_colname, df_antigen, df_marker_class)
tmp_panel
write_xlsx(tmp_panel, "tmp_panel.xlsx", col_names = TRUE)


##
###
#### -- WORKFLOW -- ####
###
##

#Table
md <- read_excel("table.xlsx")
md$condition <- factor(md$condition, levels = c("CT","IDU", "ICU"))
md$sample_id <- factor(md$sample_id,           
                       levels = md$sample_id[order(md$condition)])    
md
gs_merge

#Panel
panel2 <- read_excel("panel.xlsx")
all(panel2$fcs_colname %in% colnames(gs_merge))
panel2 <- as.data.frame(panel2)
panel2

sce <- prepData(gs_merge, panel2, md, features = panel2$fcs_colname)      

#  .;|;.  Filtering .;|;. #
# SCEs constructed with prepData can be filtered using the filterSCE function, 
# which allows for filtering of both cells and markers according to conditional statements in dplyr-style. 
# When filtering on cluster_ids, argument k specifies which clustering to use (the default NULL uses colData column "cluster_id").
sce <- filterSCE(sce,
                 sample_id != 75,
                 sample_id != 76,
                 sample_id != 79)

color_conditions <- c("#00AC05", "#0080ff", "#E10000", "#000000", "#000000")
names(color_conditions) <- levels(md$condition)

# **** CLUSTERING **** #
#Cell population identification with FlowSOM and ConsensusClusterPlus // Standard => xdim = 10, ydim = 10, maxK = 20, 
set.seed(1234)
sce <- cluster(sce, features = type_markers(sce),    
               xdim = 10, ydim = 10, maxK = 20, 
               seed = 1234) 


#===============================  Graphical outputs and dimension reduction techniques  ===========================================================#

p <- plotExprs(sce, color_by = "condition")
p
rm("p")

#event count
#nombre evenement
plotCounts(sce, color_by = "condition")
plotCounts(sce, color_by = "sample_id")
plotCounts(sce, 
           prop = TRUE,
           group_by = "condition", 
           color_by = "patient_id")

#MDS PLOT
pbMDS(sce, color_by = "condition",  pal = color_conditions)

pbMDS(sce, by = "sample_id", label_by = NULL, pal = color_conditions)

pbMDS(sce, by = "both", k = "meta20", 
      shape_by = "condition", size_by = TRUE)

plotNRS(sce, features = "type", color_by = "condition")






#Antigen/antibodies for boucle
cdx <- grep("", rownames(sce), value = TRUE)

#delta area represents the amount of extra cluster stability gained when clustering into k groups
delta_area(sce)


#cluster complet
plotClusterHeatmap(sce,                               
                   hm2 = NULL, 
                   k = "meta20", 
                   m = NULL,           
                   cluster_anno = FALSE, 
                   draw_freqs = TRUE)  

plotExprHeatmap(sce, features = "type", by = "cluster_id", 
                k = "meta20", 
                scale = "never", 
                col_clust = FALSE, 
                row_anno = FALSE, 
                bars = TRUE)

## 'plotFreqHeatmap' for cluster frequency heatmaps
plotFreqHeatmap(sce,                               
                k = "meta20", 
                m = NULL)  


## 'plotMultiHeatmap' to combine multiple heatmaps
plotMultiHeatmap(sce,
                 k = "meta20")

#Clusters with antibody intensity
#Cluster avec un rapide coup d'oeil sur l'intensite des anticorps
p <- plotClusterExprs(sce, 
                 k = "meta20", 
                 features = "type")  

(p + labs(x = NULL) + theme(
                          axis.text.x = element_text(angle = 90, size = 8), 
                          axis.text.y = element_text(size = 6) ))

rm("p")

                          # ---- tSNE / UMAP / PCA ---- #
                  # run t-SNE/UMAP on at most X cells per sample                               
sce <- runDR(sce, dr = "TSNE", cells = 500, features = "type")
sce <- runDR(sce, dr = "UMAP", cells = 500, features = "type")
sce <- runDR(sce, dr = "PCA", cells = 500, features = "type")

p <- plotDR(sce, "TSNE", color_by = "condition")
p + scale_color_manual(values =color_conditions)
p <- plotDR(sce, "UMAP", color_by = "condition")
p + scale_color_manual(values =color_conditions)
p <- plotDR(sce, "PCA", color_by = "condition")
p + scale_color_manual(values =color_conditions)


for(i in cdx){
  print(plotDR(sce, "TSNE", color_by = i, facet_by= "condition"))
}

for(i in cdx){
  print(plotDR(sce, "UMAP", color_by = i, facet_by= "condition"))
}
  

## Facet per condition                                                        
plotDR(sce, "TSNE", color_by = "meta20") + 
          facet_wrap("condition") +          
          guides(color = guide_legend(ncol = 2, override.aes = list(size = 3)))     

plotDR(sce, "UMAP", color_by = "meta20") + 
          facet_wrap("condition") +          
          guides(color = guide_legend(ncol = 2, override.aes = list(size = 3))) 

plotDR(sce, "PCA", color_by = "meta20") + 
          facet_wrap("condition") +          
          guides(color = guide_legend(ncol = 2, override.aes = list(size = 3))) 



## Abundance par ech
plotAbundances(sce, k = "meta20", 
               by = "cluster_id")

plotAbundances(sce, k = "meta20", 
               by = "sample_id")


plotAbundances(sce, k = "meta20", 
               by = "cluster_id", 
               group_by = "condition")


# plot selected markers side-by-side;
# omit left-hand side heatmap
plotMultiHeatmap(sce, 
                 k = "meta20", scale = "never",
                 hm1 = FALSE, hm2 = c("CD69", "CD56", "CD25"),
                 row_anno = FALSE, col_clust = FALSE,
                 hm2_pal = c("grey95", "black"))


#Heatmap exploratoire :: for aggregated expression heatmaps
plotExprHeatmap(sce,
                row_anno = FALSE,   # don't annotate samples
                row_clust = FALSE,  # keep samples in original order
                col_clust = FALSE,  # keep markers in original order
                bin_anno = FALSE,   # don't annotate bins
                bars = FALSE,       # don't include sample sizes
                scale = "last",     # aggregate, then scale
                hm_pal = hcl.colors(10, "YlGnBu", rev = TRUE))

pdf(file= "plotExprHeatmap-3.pdf", width = 40, height = 40)
plotExprHeatmap(sce, row_anno = "condition",   # annotate samples
                row_clust = FALSE, 
                col_clust = FALSE, # cluster samples/markers
                row_dend = FALSE, 
                col_dend = FALSE,   # include dendrograms
                bin_anno = FALSE,          # annotate bins with value
                bars = FALSE, 
                perc = FALSE, # include barplot of sample sizes
                hm_pal = c("black", "orange"))
dev.off()

plotExprHeatmap(sce, features = "type",
                by = "cluster_id", k = "meta20",
                scale = "first", q = 0.01, perc = TRUE, col_dend = FALSE)

plotFreqHeatmap(sce, k = "meta20", 
                normalize = FALSE, bars = FALSE,
                row_anno = FALSE, col_anno = FALSE,
                row_clust = FALSE, col_clust = FALSE,
                hm_pal = c("grey95", "black"))

Sys.time()
sessionInfo()
