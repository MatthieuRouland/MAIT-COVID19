#==========================================
# Title: Correlation and Corrplot
# Author: Matthieu Rouland
# Affiliation: Institut Cochin, INSERM U1016, CNRS UMR 8104, Université de Paris, Paris, France.
# Acknowledgment: Taiyun for the Corrplot package.
# License: CC-BY-SA-4.0 
# Date: 15.07.2020 - complete version with excel writting/Padj/best reading code and FigShare files production.
# Doi: https://doi.org/10.1038/s41590-021-00870-z
#==========================================
##

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
setwd("")

##

cohort_select <- read_excel("data.xlsx")
# cohort_select <- filter(cohort_select2, cohort_select2$Death == "Alive")
# cohort_select <- filter(cohort_select2, cohort_select2$Service == "ICU")
cohort_select <- select(cohort_select, x:y)

cohort_select_cor <- rcorr.adjust(cohort_select, type = "spearman", use = "pairwise.complete.obs")
cohort_select_cor_P <- cohort_select_cor$R$P
cohort_select_cor_Padj <- cohort_select_cor$P
cohort_select_cor_n <- cohort_select_cor$R$n
cohort_select_cor_r <- cohort_select_cor$R$r

# -- in case of No NA, NaN, or infinite in our matrix => replace with null values
cohort_select_cor_r[is.na(cohort_select_cor_r)] <- 0
cohort_select_cor_r[is.nan(cohort_select_cor_r)] <- 0
cohort_select_cor_r[is.infinite(cohort_select_cor_r)] <- 0

# -- in case of >1 or <-1 => correct to 1 or -1
cohort_select_cor_r[cohort_select_cor_r > 1 ] <- 1
cohort_select_cor_r[cohort_select_cor_r < sign(-1) ] <- -1

# -- in case of No NA, NaN, or infinite in our matrix => replace with null p-values
cohort_select_cor_P[is.na(cohort_select_cor_P)] <- 0.99
cohort_select_cor_P[is.nan(cohort_select_cor_P)] <- 0.99
cohort_select_cor_Padj[is.na(cohort_select_cor_P)] <- 0.99
cohort_select_cor_Padj[is.nan(cohort_select_cor_P)] <- 0.99


write_xlsx(as.data.frame(cohort_select_cor_P), "p values1.xlsx", col_names = TRUE)

write_xlsx(as.data.frame(cohort_select_cor_Padj), "p values2.xlsx", col_names = TRUE)

write_xlsx(as.data.frame(cohort_select_cor_n), "n values.xlsx", col_names = TRUE)

write_xlsx(as.data.frame(cohort_select_cor_r), "r values.xlsx", col_names = TRUE)

cohort_select_cor_P <- read_excel("p values1.xlsx") #check if p values are ok and removed '<' if needed 

cohort_select_cor_Padj <- read_excel("p values2.xlsx") #check if p values are ok and removed '<' if needed 

## -- color palette (reversed compare to corrplot original library palette)
col2 <- colorRampPalette(c( "#053061"    , "#2166AC" ,  "#4393C3"    , "#92C5DE",   "#D1E5F0", 
                            "#FFFFFF",  "#FDDBC7","#F4A582" , "#D6604D" ,"#B2182B",  "#67001F"))

pdf(file = paste0("data",Sys.Date(),".pdf"),width= 30,height= 30)
corrplot(cohort_select_corv2_r, type = "full", na.label = "NA", p.mat = cohort_select_cor_P,  insig = "label_sig", method = "square",
         sig.level = c(.001, .01, .05), pch.cex = 1, tl.cex = 1, pch.col = "black", 
         order = "hclust", addrect = 6, tl.col = "black", outline = "black", col = col2(100))
dev.off()

pdf(file = paste0("data",Sys.Date(),".pdf"),width= 30,height= 30)
corrplot(cohort_select_corv2_r, type = "full", na.label = "NA", p.mat = cohort_select_cor_Padj,  insig = "label_sig", method = "square",
         sig.level = c(.001, .01, .05), pch.cex = 1, tl.cex = 1, pch.col = "black", 
         order = "hclust", addrect = 6, tl.col = "black", outline = "black", col = col2(100))
dev.off()
