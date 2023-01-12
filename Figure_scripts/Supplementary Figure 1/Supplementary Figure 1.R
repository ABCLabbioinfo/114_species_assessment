library(ggplot2)
library(stringr)
library(scales)
library(DescTools)
library(ggrepel)
library(dplyr)
library(data.table)




data <- fread("../../Data/SupplementaryFigure_data/Supplementary Figure 1_data.txt")


color.df <- fread("../../Data/SupplementaryFigure_data/color.txt")
color <- color.df$color
names(color) <- color.df$label


#### Remove NA species ####


data <- data[-which(is.na(data$assembly_name)),]



#### Preprocessing ####

a <- str_split(data$species," ")
species_name <- c()


for(i in 1:length(a)){
  species_name <- c(species_name,paste0(str_to_title(substr(a[[i]][1],1,1)),".",a[[i]][2]))
  
}

data$species_name <- species_name

data$organism_class <- data$class

data$organism_class[which(data$species=="homo sapiens" | data$species=="mus musculus")] <- "Human & Mouse"


data$organism_class[which(data$organism=="Livestock")] <- "Livestock animals"


data$class <- factor(data$class)

data$organism_class <- factor(data$organism_class,levels=c("Human & Mouse","Livestock animals",levels(data$class)))


data <-  data %>% arrange(organism_class,desc(spanned_gaps))


data$species_name <- factor(data$species_name,levels = data$species_name)
data$species_color <- rep("black",nrow(data))

data$species_color[which(data$organism_class=="Human & Mouse")] <- "red" 


ggplot(data=data,aes(x=species_name,y=spanned_gaps))+
  geom_bar(stat='identity',na.rm=T,aes(fill=organism_class))+
  ylab("# of spanned gaps in genome")+
  labs(title="Number of spanned gaps in genome of 109 species",fill="Organism")+
  theme_bw()+
  theme_classic()+
  scale_fill_manual(values = c(color))+
  theme(plot.title=element_text(hjust=0.5),axis.text.x=element_text(hjust=1,angle = 90,color=data$species_color,face="italic"))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,max(data$spanned_gaps))) +
  geom_hline(yintercept = mean(data$spanned_gaps, na.rm=TRUE),color='red', lty='dashed', lwd=1)


ggsave("Supplementary Figure 1.pdf",width=15)
getwd()
