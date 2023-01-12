library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(DescTools)
library(ggrepel)
library(ggpubr)
library(cowplot)


#### Preprocessing ####



data <- fread("../../Data/Fig4_data/Gene_annotation_proportion.txt")

colnames(data)[1] <- "species"
data$species <- tolower(data$species)
taxa_table <- fread("../../Data/Fig4_data/Taxanomy_table.txt")
taxa_table <- taxa_table[which(taxa_table$species %in% data$species),]
taxa_table <- taxa_table[order(taxa_table$species),]


data$class <- taxa_table$class
model <- c("homo sapiens","mus musculus")
livestock <- c("gallus gallus","equus caballus","anas platyrhynchos","capra hircus","sus scrofa","ovis aries","bos taurus","bos grunniens")


data$label <- data$class
data$label[which(data$species %in% model)] <- "Human & Mouse"
data$label[which(data$species %in% livestock)] <- "Livestock animals"
data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals",unique(data$class)))

data$species <- str_to_sentence(data$species)
a <- str_split(data$species," ")
species_name <- c()
for(i in 1:length(a)){
  species_name <- c(species_name,paste0(str_to_title(substr(a[[i]][1],1,1)),".",a[[i]][2]))
  
}
data$species_name <- species_name


#### Color Mapping ####
color.df <- fread("../../Data/Fig4_data/Fig4_color.txt")
color <- color.df$color
names(color) <- color.df$label
#### Plot ####




Fig4F <- ggplot(data=data,aes(x=lncRNA_proportion,y=protein_coding_proportion,color=label,label=species_name))+
  geom_point(size=5,alpha=0.8,stroke = 0)+
  ylab(paste0("Proportion of protein coding genes"))+
  xlab("Proportion of lncRNA")+
  ##  labs(title="Proportion of protein coding genes accroding to proportion of lncRNA")+
  theme_bw()+
  theme_classic()+
  scale_color_manual(values = c(color))+ 
  geom_text_repel(fontface="italic",color="black")+
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  guides(color = guide_legend(ncol=2,byrow=FALSE))+
  geom_smooth(method='lm',color="#f9982f",alpha=0.2, fill = "#ffaa00")


Fig4F


my_legend <- get_legend(Fig4F)

Fig3F_legend <- as_ggplot(my_legend)

ggsave("Fig4F_legend.png",height = 10, width = 5, limitsize = F, plot = Fig4F_legend)
ggsave("Fig4F_legend.pdf",height = 10, width = 10, limitsize = F, plot = Fig4F_legend)



data$species_label <- data$species_name
data$species_name[data$label=="Human & Mouse"]
data$species_label[which(data$label %in% unique(data$class))] <- ""


Fig4F <- ggplot(data=data,aes(x=lncRNA_proportion,y=protein_coding_proportion,color=label,label=species_label))+
  geom_point(size=16,alpha=0.8,stroke = 0)+
  ylab(paste0("Proportion of protein coding genes"))+
  xlab("Proportion of lncRNA")+
  ##  labs(title="Proportion of protein coding genes accroding to proportion of lncRNA")+
  theme_bw()+
  theme_classic()+
  scale_color_manual(values = c(color))+ 
  geom_text_repel(fontface="italic",color="black", size = 8)+
  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.position="none",
        axis.title.x = element_text(size = 35, face = "bold"),
        axis.title.y = element_text(size = 35, face = "bold"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30)) +
  geom_smooth(method='lm',color="#f9982f",alpha=0.2, fill = "#ffaa00")

Fig4F

ggsave("Fig4F.png",height = 5, width = 15, limitsize = F, plot = Fig4F)
ggsave("Fig4F.pdf",height = 11, width = 16, limitsize = F, plot = Fig4F)

