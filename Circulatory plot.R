#==========================================
# Title:  Circulatory plot 
# Author: Matthieu Rouland
# Affiliation: Institut Cochin, INSERM U1016, CNRS UMR 8104, Université de Paris, Paris, France.
# License: CC-BY-SA-4.0 
# Date: 20.08.2020
#==========================================

###

library(ggplot2)
library(readxl)
library(writexl)
library(tidyverse)
setwd("")

###


data <- read_excel("data.xlsx")

#Produces Mann-Withney p-values
final <- "Pvalues Dead vs Alive"
for (i in 6:length(data)){

  x <- (data  %>% select(i) %>% filter(data$Death == "Dead"))
  y <- (data  %>% select(i) %>% filter(data$Death == "Alive"))
  
  print(lenght(x))
  print(lenght(y))
  pvalues <- wilcox.test(as.matrix(x),as.matrix(y))
  print(paste0(colnames(x),"___",pvalues$p.value))
  final <- append(final, paste0(colnames(x),"___",pvalues$p.value))
  }
write_xlsx(as.data.frame(final), "PvalueAliveDead.xlsx", col_names = TRUE)




#Produces Circulatory plot
data <- read_excel("PvalueAliveDead.xlsx")

empty_bar=0
to_add = as.data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar)
data=rbind(data, to_add)
data$id=seq(1, nrow(data))

label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

base_data=data %>% 
  group_by(group) %>% 
summarize(start=min(id), end=max(id) - empty_bar) %>%
rowwise() %>%
mutate(title=mean(c(start, end)))

grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]




# Make the plot #
p = ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +

#pvalues of 0.05 as a red dotted circle
geom_segment(data=grid_data, aes(x = 0, y = 2.995732, xend = 41, yend = 3), colour = "red", alpha=0.50, size=0.7 , inherit.aes = FALSE, linetype=2 ) +
  
  annotate("text", x = rep(41.5,4), y = c(0, 2, 4, 6), label = c("0", "2", "4", "6") , 
           color="grey", size=3 , angle=0, fontface="bold", hjust=2) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  scale_fill_manual(values = c("#1FBAF7", "#F9877F", "#AEB01F" , "#1FC78D","#EA7DF4"))+
  
   ylim(-6,6) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),
                       "cm"),
    text=element_text(family="serif")
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+0.5, label=variable, hjust=hjust), color="black", fontface="bold",alpha=0.6, 
            size=2.8, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = 0, xend = end, yend = 0), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -1, label=group), hjust=c(0.5,1,0.5,0,0.2), colour = c("#1FBAF7", "#F9877F", "#AEB01F" , "#1FC78D","#EA7DF4"), 
            alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)



pdf(file= "circularplot.pdf", width = 10, height = 10)
p
dev.off()

png(file= "circularplot.png", width = 1600, height = 1600)
p
dev.off()
