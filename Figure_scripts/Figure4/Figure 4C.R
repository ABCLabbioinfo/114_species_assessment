library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(DescTools)
library(ggfortify)
library(ggrepel)
library(gridExtra)
library(grid)
library(paletteer)
library(ggpubr)
library(cowplot)

#### Color mapping ###

color.df <- fread("../../Data/Fig4_data/Fig4_color.txt")
color <- color.df$color
names(color) <- color.df$label


data <- fread("../../Data/Fig4_data/Gene_anntoation.txt")

data$label[which(data$label=="Livestock")] <- "Livestock animals"
data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals",unique(data$class)))



# p=0.918
# Equitability= 0.4068+1.3727*PC1
# Adj.Rsquared : 0.841


data$Species <- str_to_sentence(data$Species)

a <- str_split(data$Species," ")
species_name <- c()


for(i in 1:length(a)){
  species_name <- c(species_name,paste0(str_to_title(substr(a[[i]][1],1,1)),".",a[[i]][2]))
  
}

data$species_name <- species_name


Fig4C <- ggplot(data,aes(x=PC1,y=equitability,color=label,label=species_name))+
  ylab("Shannon's equitability")+
  xlab("Principal component 1 (PC1)") + 
  theme_bw()+
  theme_classic()+
  scale_color_manual(values = c(color))+ 
  geom_point(size=5,alpha=0.8,stroke = 0)+
  geom_smooth(method='lm',color="#f9982f",alpha=0.2, fill = "#ffaa00")+
  geom_text_repel(fontface="italic",color="black")+
  annotate(geom="text",x=-0.1,y=1.2,label="y = 0.4068+1.3727x\nAdj.Rsquared: 0.841\nCorrelation: 0.918",size=5)+
  guides(color=guide_legend(ncol=2,byrow = FALSE)) 

my_legend <- get_legend(Fig4C)

Fig4C_legend <- as_ggplot(my_legend)

ggsave("Fig4C_legend.png",height = 10, width = 10, limitsize = F, plot = Fig4C_legend )
ggsave("Fig4C_legend.pdf",height = 10, width = 10, limitsize = F, plot = Fig4C_legend )


data$species_label <- data$species_name
data$species_name[data$label=="Human & Mouse"]
data$species_label[which(data$label %in% unique(data$class))] <- ""

Fig4C <- ggplot(data,aes(x=PC1,y=equitability,color=label,label=species_label))+
  ylab("Shannon's equitability")+
  xlab("Principal component 1 (PC1)") + 
  theme_bw()+
  theme_classic()+
  scale_color_manual(values = c(color))+ 
  geom_point(size=16,alpha=0.8,stroke = 0)+
  geom_smooth(method='lm',color="#1d3557",alpha=0.2, fill = "#99A4B5", lwd = 1.5)+
  geom_text_repel(fontface="italic",color="black", size = 8)+
  theme(legend.position="none",
        axis.title.x = element_text(size = 35, face = "bold"),
        axis.title.y = element_text(size = 35, face = "bold"),
        axis.text.y = element_text(size = 30),
        axis.text.x = element_text(size = 30)) 

Fig4C



ggsave("Fig4C.png",height = 5, width = 15, limitsize = F, plot = Fig4C)
ggsave("Fig4C.pdf",height = 11, width = 16, limitsize = F, plot = Fig4C)


