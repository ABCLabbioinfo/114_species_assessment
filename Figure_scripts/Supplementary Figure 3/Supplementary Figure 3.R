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


#### Data processing ####
temp <- fread("../../Data/SupplementaryFigure_data/Gene_anntoation.txt")
data <- fread("../../Data/SupplementaryFigure_data/Gene_annotation_proportion.txt")
data$label <- temp$label
data$class <- temp$class

data$label[which(data$label=="Livestock")] <- "Livestock animals"
data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals",unique(data$class)))


species <- data$Species


#### Suf 3A ####
color <- paletteer::paletteer_d("ggthemes::stata_s2color")[c(1,2,3,4,6,5,8,9,14,10,7,11,13)]



data$label[which(data$label=="Livestock")] <- "Livestock animals"
data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals",unique(data$class)))

data$label %>% as.factor() %>% levels() 

data$equitability %>% length()

alpha_vlaue <- c(rep(1,2),rep(0.8,11))
alpha_vlaue <- names(data$label %>% as.factor() %>% levels() )


result1 <- data %>% group_by(label) %>% 
  summarise(mean_result = median(snRNA_proportion))


result1 <- result1[order(result1$mean_result, decreasing = T), ]


color <- color.df$color
names(color) <- color.df$label



Fig3G <- data %>% ggplot(aes(x=label,y=snRNA_proportion))+ 
  geom_jitter(aes(color=label,alpha = label), position=position_jitter(width = .3), size=16,stroke = 0) + 
  geom_boxplot(color = "black",fill = NA, show.legend = F,  outlier.shape = NA,lwd = 1) + 
  xlab("") + 
  ylab("Proportion of snRNA") + 
  theme_classic() + 
  theme(axis.title.y = element_text(size = 25, face = "bold"),
        axis.text.x =  element_blank(), ## element_text(size = 30, angle = 35, hjust = 1,vjust = 1, face = "bold"),
        axis.text.y = element_text(size = 25)) + 
  scale_color_manual(values = c(color))+ 
  scale_x_discrete(limits =c(result1$label)) + 
  #theme(legend.position="none") + 
  scale_alpha_manual(values =  c(rep(1,2),rep(0.8,11)))



Fig3G


### save part
ggsave("SUF3A.png",height = 5, width = 15, limitsize = F, plot = Fig3G)
ggsave("SUF3A.pdf",height = 10, width = 15.5, limitsize = F, plot = Fig3G)



Fig3G <- data %>% ggplot(aes(x=label,y=snoRNA_proportion))+ 
  geom_jitter(aes(color=label,alpha = label), position=position_jitter(width = .3), size=16,stroke = 0) + 
  geom_boxplot(color = "black",fill = NA, show.legend = F,  outlier.shape = NA,lwd = 1) + 
  xlab("") + 
  ylab("Proportion of snoRNA") + 
  theme_classic() + 
  theme(axis.title.y = element_text(size = 25, face = "bold"),
        axis.text.x =  element_blank(), ## element_text(size = 30, angle = 35, hjust = 1,vjust = 1, face = "bold"),
        axis.text.y = element_text(size = 25)) + 
  scale_color_manual(values = c(color))+ 
  scale_x_discrete(limits =c(result1$label)) + 
  #theme(legend.position="none") + 
  scale_alpha_manual(values =  c(rep(1,2),rep(0.8,11)))



Fig3G


### save part
ggsave("SUF3B.png",height = 5, width = 15, limitsize = F, plot = Fig3G)
ggsave("SUF3B.pdf",height = 10, width = 15.5, limitsize = F, plot = Fig3G)



Fig3G <- data %>% ggplot(aes(x=label,y=scaRNA_proportion))+ 
  geom_jitter(aes(color=label,alpha = label), position=position_jitter(width = .3), size=16,stroke = 0) + 
  geom_boxplot(color = "black",fill = NA, show.legend = F,  outlier.shape = NA,lwd = 1) + 
  xlab("") + 
  ylab("Proportion of scaRNA") + 
  theme_classic() + 
  theme(axis.title.y = element_text(size = 25, face = "bold"),
        axis.text.x =  element_blank(), ## element_text(size = 30, angle = 35, hjust = 1,vjust = 1, face = "bold"),
        axis.text.y = element_text(size = 25)) + 
  scale_color_manual(values = c(color))+ 
  scale_x_discrete(limits =c(result1$label)) + 
  #theme(legend.position="none") + 
  scale_alpha_manual(values =  c(rep(1,2),rep(0.8,11)))



Fig3G


### save part
ggsave("SUF3C.png",height = 5, width = 15, limitsize = F, plot = Fig3G)
ggsave("SUF3C.pdf",height = 10, width = 15.5, limitsize = F, plot = Fig3G)



Fig3G <- data %>% ggplot(aes(x=label,y=miRNA_proportion))+ 
  geom_jitter(aes(color=label,alpha = label), position=position_jitter(width = .3), size=16,stroke = 0) + 
  geom_boxplot(color = "black",fill = NA, show.legend = F,  outlier.shape = NA,lwd = 1) + 
  xlab("") + 
  ylab("Proportion of miRNA") + 
  theme_classic() + 
  theme(axis.title.y = element_text(size = 25, face = "bold"),
        axis.text.x =  element_blank(), ## element_text(size = 30, angle = 35, hjust = 1,vjust = 1, face = "bold"),
        axis.text.y = element_text(size = 25)) + 
  scale_color_manual(values = c(color))+ 
  scale_x_discrete(limits =c(result1$label)) + 
  #theme(legend.position="none") + 
  scale_alpha_manual(values =  c(rep(1,2),rep(0.8,11)))



Fig3G


### save part
ggsave("SUF3D.png",height = 5, width = 15, limitsize = F, plot = Fig3G)
ggsave("SUF3D.pdf",height = 10, width = 15.5, limitsize = F, plot = Fig3G)

