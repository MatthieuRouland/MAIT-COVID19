#==========================================
# Title: Clinical table generation
# Author: Matthieu Rouland
# Affiliation: Institut Cochin, INSERM U1016, CNRS UMR 8104, Université de Paris, Paris, France.
# License: CC-BY-SA-4.0 
# Date: 20.08.2020
# Doi: https://doi.org/10.1038/s41590-021-00870-z
#==========================================


#--Setup--#
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
library(atable)
library(xtable)
setwd("")
cohort_untouched <- read_excel("data.xlsx")

#--atable options--#
atable_options(format_to = "Latex") 
atable_options('replace_NA_by' = 'Missing') # Replace NA by "Missing" groups

#--Statistical functions and labeling--#
new_statistics_numeric <- function(x, ...){
  statistics_out <- list(Median = median(x, na.rm = TRUE), 
                         MAD = mad(x, na.rm = TRUE),
                         Mean = mean(x, na.rm = TRUE),
                         SD = sd(x, na.rm = TRUE),
                         Range = range(x, na.rm = TRUE))

  class(statistics_out) <- c("statistics_numeric", class(statistics_out))
  # We will need this new class later to specify the format
  return(statistics_out)
}

new_format_statistics_numeric <- function(x, ...){
  Median_MAD <- paste(round(c(x$Median, x$MAD), digits = 1), collapse = " \\textpm ")
  Mean_SD <- paste(round(c(x$Mean, x$SD), digits = 1), collapse = " \\textpm ")
  Range_Range <- paste(round(c(x$Range), digits = 1), collapse = "-")

  out <- data.frame(tag = factor(c("Median; MAD", "Mean; SD", "Range"), 
                                 levels = c("Median; MAD", "Mean; SD", "Range")
                                    ,labels = c("Median (MAD)", "Mean (SD)", "Range")),
                    # use levels to retain the order of the rows 
                    value = c(Median_MAD, Mean_SD, Range_Range),
                    stringsAsFactors = FALSE)
  return(out)
}

#-Col names verification-#
for (i in 1:length(cohort_untouched)){ 
    print((colnames(cohort_untouched)[i]))
    print(is_syntactically_valid_name((colnames(cohort_untouched)[i])))
}


#####
## Customyze labeling, name, ect of the cohort from our french excel file ##
cohort <- within(cohort_untouched, {
  Sex <- factor(Sexe, levels = c("F","M"), labels = c("female", "male"))
  Hospital <- factor(Hospital, levels = c("Bichat", "Cochin", "Beaujon-Lariboisière", "Cochin", "EFS"), 
                         labels = c("Bichat", "Cochin", "Beaujon-Lariboisière", "Cochin", "EFS"))
  
  Service <- factor(Service, levels = c("Temoin sain","SMIT","REA"), labels = c("uninfected control", "IDU", "ICU" ))
  
  Deaths <- factor(Deaths, levels = c("oui"),labels = c("oui"))
  
  Tocilizumab <- factor(Tocilizumab, levels = c("oui"), labels = c("oui"))
  
  Anakinra <- factor(Anakinra, levels = c("oui"), 
                     labels = c("oui" ))
  
  Kaletra <- factor(Kaletra, levels = c("oui"), 
                    labels = c("oui"))
  
  INFbetaDiscovery <- factor(INFbetaDiscovery, levels = c("oui"), 
                             labels = c("oui"))
  
  Dexamethasone <- factor(Dexamethasone, levels = c("oui"), 
                          labels = c("oui"))
  Plaquenil <- factor(Plaquenil, levels = c("oui"), 
                                   labels = c("oui"))
  

  Infection <- factor(Infection, levels = c("oui"),
                           labels = c("oui"))
  TDM <- factor(TDM, levels = c("mild", "moderate", "severe", "Very severe", "Critical"), 
                             labels = c("Mild", "Moderate", "Severe", "Grievous", "Critical"))
  
  Diabetes <- factor(Diabetes, levels = c("DT2"),
                           labels = c("T2D"))
  
  ATCD <- factor(ATCD, levels = c("oui"),
                     labels = c("oui"))
  
  
  PNNgL <- as.numeric(PNNgL)
  HbA1c <- as.numeric(HbA1c)
  PaO2FiO2 <- as.numeric(PaO2FiO2)
  Num <- as.numeric(Num)
  Initiales <- as.numeric(Initiales)
})

#- Compute table -#
thetable <- atable(as.data.frame(cohort), 
        statistics.numeric = new_statistics_numeric,
        format_statistics.statistics_numeric = new_format_statistics_numeric,
       group_col = "Service", 
       target_cols = c("Sex", (colnames(select(cohort, 4,5,7:37)))
                       ))
#- Latex output -#
Hmisc::latex(thetable,
             file = "",
             title = "",
             rowname = NULL)


#- Other possible output if needed -#
# atable(form, arthritis, format_to = "Latex")   # format to LaTeX
# atable(form, arthritis, format_to = "Console") # format to console
# atable(form, arthritis, format_to = "HTML")    # format to HTML
# atable(form, arthritis, format_to = "Raw")     # no formatting
# atable(form, arthritis, format_to = "Word")    # format to MS Word
