#==========================================
# Title: Correlation and Corrplot
# Author: Matthieu Rouland
# Affiliation: Institut Cochin, INSERM U1016, CNRS UMR 8104, Université de Paris, Paris, France.
# Acknowledgment: Taiyun for the Corrplot package.
# License: CC-BY-SA-4.0 
# Date: 15.07.2020
#==========================================

##

library(ggplot2)
library(readxl)
library(writexl)
library(ggpubr)
library(purrr)
library(dplyr)
library(shiny)
library(ggrepel)
library(corrplot)
library(Hmisc)
library(psych)
library(RcmdrMisc)
library(PerformanceAnalytics)
library(GGally)
setwd("")

##

cohort_select2 <- read_excel("data.xlsx")
# cohort_select2 <- filter(cohort_select2, cohort_select2$Death == "Alive")
# cohort_select2 <- filter(cohort_select2, cohort_select2$Service == "ICU")
cohort_select2 <- select(cohort_select2,6:95)

cohort_select_corv2 <- rcorr.adjust(cohort_select2, type = "spearman", use = "pairwise.complete.obs")
cohort_select_corv2_P <- cohort_select_corv2$R$P
cohort_select_corv2_n <- cohort_select_corv2$R$n
cohort_select_corv2_r <- cohort_select_corv2$R$r 

#in case of No NA, NaN, or infinite in our matrix => replace with null values
cohort_select_corv2_r[is.na(cohort_select_corv2_r)] <- 0
cohort_select_corv2_r[is.nan(cohort_select_corv2_r)] <- 0
cohort_select_corv2_r[is.infinite(cohort_select_corv2_r)] <- 0

#in case of >1 or <-1 => correct to 1 or -1
cohort_select_corv2_r[cohort_select_corv2_r > 1 ] <- 1
cohort_select_corv2_r[cohort_select_corv2_r < sign(-1) ] <- -1

#in case of No NA, NaN, or infinite in our matrix => replace with null p-values
cohort_select_corv2_P[is.na(cohort_select_corv2_P)] <- 0.99
cohort_select_corv2_P[is.nan(cohort_select_corv2_P)] <- 0.99

col2 <- colorRampPalette(c( "#053061"    , "#2166AC" ,  "#4393C3"    , "#92C5DE",   "#D1E5F0", 
                           "#FFFFFF",  "#FDDBC7","#F4A582" , "#D6604D" ,"#B2182B",  "#67001F"))

pdf(file = paste0("data",Sys.Date(),".pdf"),width= 30,height= 30)
corrplot(cohort_select_corv2_r, type = "full", na.label = "NA", p.mat = cohort_select_corv2_P,  insig = "label_sig", method = "square",
         sig.level = c(.001, .01, .05), pch.cex = 1, tl.cex = 1, pch.col = "black", 
         order = "hclust", addrect = 6, tl.col = "black", outline = "black", col = col2(100))
dev.off()
