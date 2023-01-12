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


data <- fread("../../Data/Fig4_data/Fig4A_data.txt")
data$label[which(data$label=="Livestock")] <- "Livestock animals"
data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals",unique(data$class)))




species <- data$Species

temp <- data %>% select(-c("Species","class","label","shannon","equitability","PC1","PC2","total_genes"))


result1 <- data %>% group_by(label) %>% 
  summarise(mean_result = median(type_of_genes))


result1 <- result1[order(result1$mean_result, decreasing = T), ]


color.df <- fread("../../Data/Fig4_data/Fig4_color.txt")

data$label[which(data$label=="Livestock")] <- "Livestock animals"
data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals",unique(data$class)))

data$label %>% as.factor() %>% levels() 

data$equitability %>% length()

alpha_vlaue <- c(rep(1,2),rep(0.8,11))
alpha_vlaue <- names(data$label %>% as.factor() %>% levels() )


result1 <- data %>% group_by(label) %>% 
  summarise(mean_result = median(type_of_genes))
result1 <- result1[order(result1$mean_result, decreasing = T), ]
result1

color <- color.df$color
names(color) <- color.df$label



Fig4A <- data %>% ggplot(aes(x=label,y=type_of_genes))+ 
  geom_jitter(aes(color=label,alpha = label), position=position_jitter(width = .3), size=15,stroke = 0) + 
  geom_boxplot(color = "black",fill = NA, show.legend = F,  outlier.shape = NA,lwd = 1) + 
  xlab("") + 
  ylab("Number of types for genes") + 
  theme_classic() + 
  theme(axis.title = element_text(size=25, face = "bold"),
        axis.text.y = element_text(size= 25), 
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color))+ 
  scale_x_discrete(limits =c(result1$label)) + 
  theme(legend.position="none",
        #axis.text.x = element_blank()
        ) + ## element_text(size = 30,angle = 35, hjust = 1, vjust = 1,face = "bold") 
  scale_alpha_manual(values =  c(rep(1,2),rep(0.8,11)))



Fig4A


### save part
ggsave("Fig4A.png",height = 5, width = 13, limitsize = F, plot = Fig4A)
ggsave("Fig4A.pdf",height = 10, width = 15.5, limitsize = F, plot = Fig4A)
