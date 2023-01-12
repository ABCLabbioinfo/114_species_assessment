library(ggplot2)
library(data.table)
library(ggcorrplot)
library(pheatmap)
library(corrplot)
library(dplyr)
library(pafr)


data <- fread("../../Data/SupplementaryFigure_data/Assembly_statistics.txt")

data %>% dim

color.df <- fread("../../Data/SupplementaryFigure_data/color.txt")
color <- color.df$color
names(color) <- color.df$label


data$organism[which(data$organism=="Livestock")] <- "Livestock animals"


levels <- data$organism[-which(grepl(data$organism,pattern="Human & Mouse|Livestock animals"))]

data$organism <- factor(data$organism,levels=c("Human & Mouse","Livestock animals",unique(levels)))


data$`Genome size` <- data$`Genome size`/1



data$`Length of masekd repeat elements` <- data$`Length of masekd repeat elements`/1
### Number of spanned according to total length ###

options(scipen=1000000)


cor(data$`Genome size`,data$`Length of masekd repeat elements`)

Fig2E <- ggplot(data=data,aes(x=data$`Number of spanned gaps`,data$`Number of contigs`,color=organism))+
  geom_point(size=5)+
  ylab("# of contigs")+
  labs(title="")+ ## Length of masked repeat elements according to genome size
  xlab("# of spanned gaps")+
  #scale_x_continuous(labels=Gb_lab)+
  #scale_y_continuous(labels=Gb_lab) +
  theme_bw()+
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5))+
  scale_color_manual(values = c(color))+ 
  geom_smooth(method="lm",se=T,alpha=0.3,color="red") +
  theme(axis.title = element_text(size=25, face = "bold"),
        axis.text.y = element_text(size= 25, face = "bold"), 
        axis.text.x = element_text(size= 25, face = "bold"), 
        axis.title.x = element_blank(),
        legend.title = element_blank()) 
  #theme(legend.position="none") 

Fig2E



ggsave("SuF2A.pdf",height =  6, width = 13, limitsize = F, plot = Fig2E)



Fig2E <- ggplot(data=data,aes(x=data$`Number of spanned gaps`,data$`Contig L50`,color=organism))+
  geom_point(size=5)+
  ylab("Contig L50")+
  labs(title="")+ ## Length of masked repeat elements according to genome size
  xlab("# of spanned gaps")+
  #scale_x_continuous(labels=Gb_lab)+
  #scale_y_continuous(labels=Gb_lab) +
  theme_bw()+
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5))+
  scale_color_manual(values = c(color))+ 
  geom_smooth(method="lm",se=T,alpha=0.3,color="red") +
  theme(axis.title = element_text(size=25, face = "bold"),
        axis.text.y = element_text(size= 25, face = "bold"), 
        axis.text.x = element_text(size= 25, face = "bold"), 
        axis.title.x = element_blank(),
        legend.title = element_blank()) + 
  theme(legend.position="none") 



Fig2E



ggsave("SuF2B.pdf",height =  6, width = 13, limitsize = F, plot = Fig2E)






