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
temp <- fread("../../Data/Fig4_data/Gene_anntoation.txt")
data <- fread("../../Data/Fig4_data/Gene_annotation_proportion.txt")
data$label <- temp$label
data$class <- temp$class

data$label[which(data$label=="Livestock")] <- "Livestock animals"
data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals",unique(data$class)))


species <- data$Species




color.df <- fread("../../Data/Fig4_data/Fig4_color.txt")


data$label[which(data$label=="Livestock")] <- "Livestock animals"
data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals",unique(data$class)))


alpha_vlaue <- c(rep(1,2),rep(0.8,11))
alpha_vlaue <- names(data$label %>% as.factor() %>% levels() )


result1 <- data %>% group_by(label) %>% 
  summarise(mean_result = median(pseudogene_proportion))


result1 <- result1[order(result1$mean_result, decreasing = T), ]


color <- color.df$color
names(color) <- color.df$label



Fig4G <- data %>% ggplot(aes(x=label,y=pseudogene_proportion))+ 
  geom_jitter(aes(color=label,alpha = label), position=position_jitter(width = .3), size=16,stroke = 0) + 
  geom_boxplot(color = "black",fill = NA, show.legend = F,  outlier.shape = NA,lwd = 1) + 
  xlab("") + 
  ylab("Proportion of pseudogenes") + 
  theme_classic() + 
  theme(axis.title.y = element_text(size = 25, face = "bold"),
        axis.text.x =  element_blank(), ## element_text(size = 30, angle = 35, hjust = 1,vjust = 1, face = "bold"),
        axis.text.y = element_text(size = 25)) + 
  scale_color_manual(values = c(color))+ 
  scale_x_discrete(limits =c(result1$label)) + 
  theme(legend.position="none") + 
  scale_alpha_manual(values =  c(rep(1,2),rep(0.8,11)))



Fig4G


### save part
ggsave("Fig4G.png",height = 5, width = 15, limitsize = F, plot = Fig4G)
ggsave("Fig4G.pdf",height = 10, width = 15.5, limitsize = F, plot = Fig4G)

