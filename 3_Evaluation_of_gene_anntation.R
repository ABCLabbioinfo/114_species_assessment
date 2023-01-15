#### Step 3. Evaluation of gene annotation quality based on transcript's diversity ####
source("Package_manager.R")

#### Step 3-1. Investigation of types of transcripts in 102 species ####

data <- fread("Data/Gene_annotation_table.txt")
data$label[which(data$label=="Livestock")] <- "Livestock animals"
data$label <- factor(data$label,levels=c("Human & Mouse","Livestock animals",unique(data$class)))
species <- data$Species

color.df <- fread("Data/Annotation_color_code.txt")
color <- color.df$color
names(color) <- color.df$label

result1 <- data %>% group_by(label) %>% 
  summarise(mean_result = median(type_of_genes))


result1 <- result1[order(result1$mean_result, decreasing = T), ]



alpha_vlaue <- c(rep(1,2),rep(0.8,11))
alpha_vlaue <- names(data$label %>% as.factor() %>% levels() )


result1 <- data %>% group_by(label) %>% 
  summarise(mean_result = median(type_of_genes))
result1 <- result1[order(result1$mean_result, decreasing = T), ]





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
  theme(legend.position="none"
        ) + 
  scale_alpha_manual(values =  c(rep(1,2),rep(0.8,11)))



Fig4A

ggsave("Fig4A.png",height = 5, width = 13, limitsize = F, plot = Fig4A)
ggsave("Fig4A.pdf",height = 10, width = 15.5, limitsize = F, plot = Fig4A)


#### Step 3-2. Dimensional reduction results for the ratio of 30 transcript types in gene annotation of each species with principal component analysis  ####



## PCA for reproductivity ##
proportion_index <- which(grepl(colnames(data),pattern="*_proportion"))
temp <- data %>% select(colnames(data)[proportion_index])
temp <- apply(temp,2,as.numeric)
pca_dt <- prcomp(temp,center=T)
pca_data <- pca_dt$x[,1:2]
summary(pca_dt)


pca_data <- as.data.frame(pca_data)
pca_data$species <- data$species_name
pca_data$class <- data$class


ggplot(data=pca_data,aes(x=PC1,y=PC2,color=class,label=species))+
  geom_point(size=3,alpha=0.8)+
  geom_text_repel()+
  theme_bw()+
  theme_classic()+
  ylab("PC2 (10.9%)")+
  xlab("PC1 (77.92%)")


## PCA for figure ##

data$Species <- str_to_sentence(data$Species)

temp <- str_split(data$Species,pattern=" ")

species_name <- c()
for(i in 1:length(temp)){
  species_name <- c(species_name,paste0(substr(temp[[i]][1],1,1),".",temp[[i]][2]))
  
}

data$species_name <- species_name


data$species_label <- data$species_name
data$species_name[data$label=="Human & Mouse"]
data$species_label[which(data$label %in% unique(data$class))] <- ""


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




#### Step 3-3. Investigation of correlation between Shannon's equitability and PC1 from PCA analysis  ####



data$Species <- str_to_sentence(data$Species)

temp <- str_split(data$Species,pattern=" ")

species_name <- c()
for(i in 1:length(temp)){
  species_name <- c(species_name,paste0(substr(temp[[i]][1],1,1),".",temp[[i]][2]))
  
}

data$species_name <- species_name

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



#### Step 3-4. Comparison of transcript's diversity index for all 102 species   ####

## Min-Max normalization function ##
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
data$PC1 <- normalize(data$PC1)


alpha_vlaue <- c(rep(1,2),rep(0.8,11))
alpha_vlaue <- names(data$label %>% as.factor() %>% levels() )

result1 <- data %>% group_by(label) %>% 
  summarise(mean_result = median(equitability))
result1 <- result1[order(result1$mean_result, decreasing = T), ]



data <-  data %>% arrange(label,desc(abs(PC1)))

data$species_name <- factor(data$species_name,levels = data$species_name)
data$species_color <- rep("black",nrow(data))
data$species_color[which(data$Species=="Homo sapiens" | data$Species=="Mus musculus")] <- "red"



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



#### Step 3-5. lncRNA proportion in the gene annotations of each species  ####



colnames(data)[1] <- "species"

data$species <- tolower(data$species)
taxa_table <- fread("Data/Taxanomy_table.txt")
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
        axis.text.x = element_blank(), 
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color))+ 

  theme(legend.position="none")+
  scale_alpha_manual(values =  c(rep(1,2),rep(0.8,6)))

ggsave("Fig4E.png",height = 5, width = 15, limitsize = F, plot = Fig4E)
ggsave("Fig4E.pdf",height = 10, width = 15.5, limitsize = F, plot = Fig4E)




#### Step 3-6. Correlation between the proportion of lncRNAs and the proportion of protein coding gene  ####


data <- fread("Data/Gene_annotation_table.txt")

data$Species <- str_to_sentence(data$Species)

temp <- str_split(data$Species,pattern=" ")

species_name <- c()
for(i in 1:length(temp)){
  species_name <- c(species_name,paste0(substr(temp[[i]][1],1,1),".",temp[[i]][2]))
  
}

data$species_name <- species_name
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




#### Step 3-7. Comparison of the proportion of annotated pseudogenes in the gene annotations of each species ####
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



