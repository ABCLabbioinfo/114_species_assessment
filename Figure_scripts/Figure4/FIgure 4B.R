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

#### Read data ####

data <- fread("../../Data/Fig4_data/Gene_anntoation.txt")

data$Species <- str_to_sentence(data$Species)



data$label[which(data$label=="Livestock")] <- "Livestock animals"


data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals",unique(data$class)))


temp <- str_split(data$Species,pattern=" ")

species_name <- c()
for(i in 1:length(temp)){
  species_name <- c(species_name,paste0(substr(temp[[i]][1],1,1),".",temp[[i]][2]))
  
}

data$species_name <- species_name

Fig4B <- ggplot(data,aes(x=PC1,y=PC2,color=label,label=species_name))+
  geom_point(size=5,alpha=0.8, stroke = 0)+
  geom_text_repel(color="black")+
  theme_bw()+
  theme_classic()+
  labs(color="Class")+
  scale_color_manual(values = c(color))+ 
  ylab(paste0("PC2 (10.90%)"))+
  xlab(paste0("PC1 (77.92%)")) +
  guides(color=guide_legend(ncol=2,byrow=FALSE))


data$species_label <- data$species_name
data$species_name[data$label=="Human & Mouse"]
data$species_label[which(data$label %in% unique(data$class))] <- ""

my_legend <- get_legend(Fig4B)

Fig4B_legend <- as_ggplot(my_legend)

ggsave("Fig4B_legend.png",height = 10, width = 10, limitsize = F, plot = Fig4B_legend )
ggsave("Fig4B_legend.pdf",height = 10, width = 10, limitsize = F, plot = Fig4B_legend )


Fig4B <- ggplot(data,aes(x=PC1,y=PC2,color=label,label=species_label))+
  geom_point(size=16,alpha=0.8, stroke = 0)+
  geom_text_repel(fontface="italic",color="black", size = 8)+
  theme_bw()+
  theme_classic()+
  labs(color="Class")+
  scale_color_manual(values = c(color))+ 
  ylab(paste0("PC2 (10.90%)"))+
  xlab(paste0("PC1 (77.92%)")) +
  theme(legend.position="none",
        axis.title.x = element_text(size = 35, face = "bold"),
        axis.title.y = element_text(size = 35, face = "bold"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30)) 


Fig4B

ggsave("Fig4B.png",height = 5, width = 15, limitsize = F, plot = Fig4B)
ggsave("Fig4B.pdf",height = 11, width = 16, limitsize = F, plot = Fig4B)
