library(ggplot2)
library(data.table)
library(ggcorrplot)
library(pheatmap)
library(corrplot)
library(stringr)


data <- fread("../../Data/Fig2_data/Assembly_statistics.txt")

color.df <- fread("../../Data/Fig2_data/Fig2_color.txt")
color <- color.df$color
names(color) <- color.df$label

data$organism[which(data$organism=="Livestock")] <- "Livestock animals"


levels <- data$organism[-which(grepl(data$organism,pattern="Human & Mouse|Livestock animals"))]

data$organism <- factor(data$organism,levels=c("Human & Mouse","Livestock animals",unique(levels)))


data$`Adjusted N50 in scaffold` <- data$`Adjusted N50 in scaffold`/1
#### Total length ####

Fig2C <- ggplot(data=data,aes(x=organism,y=data$`Adjusted N50 in scaffold`,color=organism))+
  geom_jitter(aes(color=organism), 
              position=position_jitter(width = .3), 
              size=6,stroke = 0) + 
  geom_boxplot(color = "black",fill = NA, 
               show.legend = F,  
               outlier.shape = NA,
               lwd = 1) + 
  ylab("Adjusted N50 in scaffold")+
  labs(title="")+ ##Adjusted N50 in scaffold according to organism
  theme_bw()+
  theme_classic()+
  theme(axis.title = element_text(size=25, face = "bold"),
        axis.text.y = element_text(size= 25, face = "bold"), 
        axis.text.x = element_text(size= 9.5, face = "bold"), 
        axis.title.x = element_blank(),
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color))+
  theme(legend.position="none")

Fig2C


ggsave("Fig2C.pdf",height =  6, width = 13, limitsize = F, plot = Fig2C)
