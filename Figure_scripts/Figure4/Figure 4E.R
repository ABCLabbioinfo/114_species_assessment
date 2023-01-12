library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(DescTools)
library(ggrepel)
library(gridExtra)
library(grid)
library(paletteer)

# Read data #

data <- fread("../../Data/Fig4_data/Gene_annotation_proportion.txt")
color.df <- fread("../../Data/Fig4_data/Fig4_color.txt")

color <- color.df$color
names(color) <- color.df$label

colnames(data)[1] <- "species"

data$species <- tolower(data$species)

taxa_table <- fread("../../Data/Taxanomy_table.txt")

taxa_table <- taxa_table[which(taxa_table$species %in% data$species),]

taxa_table <- taxa_table[order(taxa_table$species),]

data$class <- taxa_table$class

data$species <- str_to_sentence(data$species)


model <- c("homo sapiens","mus musculus")
livestock <- c("gallus gallus","equus caballus","anas platyrhynchos","capra hircus","sus scrofa","ovis aries","bos taurus","bos grunniens")



model <- str_to_sentence(model)
livestock <- str_to_sentence(livestock)

data$label <- data$class

Others <- c("Ascidiacea","Insecta", "Amphibia","Reptilia","Hyperoartia","Saccharomycetes" )




data$label[which(data$species %in% model)] <- "Human & Mouse"

data$label[which(data$species %in% livestock)] <- "Livestock animals"

data$label[which(data$label %in% Others)] <- "Others"



data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals","Aves","Mammalia","Lepidosauria","Magnoliopsida","Actinopteri","Others"))


alpha_vlaue <- c(rep(1,2),rep(0.8,6))
alpha_vlaue <- names(data$label %>% as.factor() %>% levels() )

Fig4E <- data %>% ggplot(aes(x=label,y=lncRNA_proportion))+ 
  geom_jitter(aes(color=label, alpha = label), position=position_jitter( width = .3), size=16, stroke = 0) + 
  geom_boxplot(color = "black",fill = NA, show.legend = F,  outlier.shape = NA,lwd = 1) + 
  ylab("Proportion of lncRNAs")+
  xlab("") + 
  theme_classic() + 
  theme(axis.text.y = element_text(size= 25), 
        axis.title.y = element_text(size=25, face = "bold"),
        axis.text.x = element_blank(), ## element_text(size=30, angle = 35, hjust = 1, vjust = 1, face = "bold"),
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color))+ 
#  scale_x_discrete(limits =c(result1$label)) + 
  theme(legend.position="none")+
  scale_alpha_manual(values =  c(rep(1,2),rep(0.8,6)))



Fig4E


ggsave("Fig4E.png",height = 5, width = 15, limitsize = F, plot = Fig4E)
ggsave("Fig4E.pdf",height = 10, width = 15.5, limitsize = F, plot = Fig4E)

