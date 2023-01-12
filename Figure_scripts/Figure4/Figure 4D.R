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
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

data <- fread("../../Data/Fig4_data//Gene_anntoation.txt") %>% as.data.frame()
color.df <- fread("../../Data/Fig4_data/Fig4_color.txt")

color <- color.df$color
names(color) <- color.df$label

data$PC1 <- normalize(data$PC1)

data$label[which(data$label=="Livestock")] <- "Livestock animals"
data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals",unique(data$class)))


alpha_vlaue <- c(rep(1,2),rep(0.8,11))
alpha_vlaue <- names(data$label %>% as.factor() %>% levels() )

result1 <- data %>% group_by(label) %>% 
  summarise(mean_result = median(equitability))
result1 <- result1[order(result1$mean_result, decreasing = T), ]




data$Species <- str_to_sentence(data$Species)

a <- str_split(data$Species," ")
species_name <- c()


for(i in 1:length(a)){
  species_name <- c(species_name,paste0(str_to_title(substr(a[[i]][1],1,1)),".",a[[i]][2]))
}

data$species_name <- species_name

data <-  data %>% arrange(label,desc(abs(PC1)))

data$species_name <- factor(data$species_name,levels = data$species_name)
data$species_color <- rep("black",nrow(data))
data$species_color[which(data$Species=="Homo sapiens" | data$Species=="Mus musculus")] <- "red"

Fig4D <- data %>% ggplot(aes(x=species_name,y=PC1))+
  geom_bar(aes(fill=label), stat='identity',na.rm=T)+
  theme_bw()+
  theme_classic()+
  ylab("") +
  xlab("")+
  geom_hline(yintercept = mean(data$shannon, na.rm=TRUE),color='#484848', lty='dashed', lwd=0.7) + 
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = ggplot2::element_text(face="italic",angle = 90, vjust = 0.3 ,hjust=1, color=data$species_color))+ 
  scale_alpha_manual(values =  c(rep(1,2),rep(0.8,11)))+
  scale_fill_manual(values = c(color))+
  guides(fill=guide_legend(nrow=2,byrow=FALSE))
  
Fig4D


my_legend <- get_legend(Fig4D)

Fig4D_legend <- as_ggplot(my_legend)

ggsave("Fig4D_legend.png",height = 5, width = 15, limitsize = F, plot = Fig4D_legend )
ggsave("Fig4D_legend.pdf",height = 5, width = 15, limitsize = F, plot = Fig4D_legend )



Fig4D <- data %>% ggplot(aes(x=species_name,y=PC1))+
  geom_bar(aes(fill=label), stat='identity',na.rm=T)+
  theme_bw()+
  theme_classic()+
  ylab("Transcript's diversity based on PCA") +
  xlab("")+
  geom_hline(yintercept = mean(data$PC1, na.rm=TRUE),color='#484848', lty='dashed', lwd=0.7) + 
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = ggplot2::element_text(face="italic",angle = 90, vjust = 0.3 ,hjust=1, size = 40, color=data$species_color),
        axis.text.y = ggplot2::element_text(size = 45),
        axis.title.y = ggplot2::element_text(size = 45),)+ 
  scale_alpha_manual(values =  c(rep(1,2),rep(0.8,11)))+
  scale_fill_manual(values = c(color))  +
  theme(legend.position="none")  +
  guides(fill=guide_legend(nrow=2,byrow=F)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1))


Fig4D 


ggsave("Fig4D.png",height = 5, width = 45, limitsize = F, plot = Fig4D)
ggsave("Fig4D.pdf",height =  14, width = 50, limitsize = F, plot = Fig4D)

