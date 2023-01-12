## Load library
library(pacman)
pacakges <- c("dplyr","ggplot2","data.table","stringr","readr","janitor","corrplot","pheatmap","ggdendro","plotly","pryr","MASS","ggpmisc","ggrepel","RColorBrewer")
pacman::p_load(pacakges,character.only = TRUE)


data <- read.csv("../../Data/Fig3_data//108RNA_sample.csv")
data <- data %>% mutate(Species=as.factor(Species))
colnames(data)
color <- paletteer::paletteer_d("ggthemes::stata_s2color")[c(1,2,3,4,6,5,8,9,14,10,7,11,13)]


# Extract Index info
indexName <- "MQI"

data$Species <- sapply(as.character(data$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)
# Re-factor Species on descending order of index 
subDF <- data %>% group_by(Label,Species) %>% summarise(avg=mean(get(indexName))) %>% arrange(desc(avg))
data$Species <- factor(data$Species, levels = unique(subDF$Species))

# Customized x axis text
custom <- ifelse(subDF$Label == "Human & Mouse", "red", "black")

suppressWarnings({
  # Draw figure with categorized by species (*if you aim to categorized by Type, x axis should became Type*)
  fig <- ggplot(data=data,aes(x=Species,y=get(indexName))) + 
    geom_jitter(stat="identity",aes(color=Label),size=9, alpha=0.3, stroke = 0) +
    geom_boxplot(outlier.shape = NA, color = "black" , fill =NA, lwd = 1.4) + 
    ylab(indexName) + xlab("Species") + 
    scale_color_manual(values=color) + 
    theme_classic() + 
    ylab("Mapping quality index") + 
    geom_hline(yintercept = mean(data$MQI, na.rm=TRUE),color='#484848', lty='dashed', lwd=0.7) + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 40,face = "bold"),
          axis.text.y = element_text(size = 35),
          axis.text.x = element_text(size = 35, angle = 90, hjust = 1, face = "italic"),
          legend.title = element_blank(),
          legend.text = element_text(size=35))+
    theme(legend.position="none")  
})

fig

ggsave("Figure3C.pdf",fig,width = 60, height = 13,limitsize = FALSE)







