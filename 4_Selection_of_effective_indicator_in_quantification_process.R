source("Package_manager.R")

#### Step 4-1. Correlation between gene annotation and 3 quality evaluation indicators obtained empirically in the quantification process ####



corD <- fread("Data/RNASeq_quantification_data.csv")
colnames(corD) <- gsub("_"," ",colnames(corD))


# Remove unnecessary column
corD <- corD[,-c(1:3,5,6,23)]

## Pheatmap visualization
colnames(corD)
x <- corD[,c(1:3)]
y <- corD[,-c(1:3)]

y <- y[,c(1,5,6,7,3,4,10,11,8,9,2,12)] # Reorder column on y axis

plotData <- cor(y,x, method = "spearman")

colnames(plotData) <- c("Quant. success rate","Quant. failure rate (No feature; %)","Quant. failure rate (Ambiguity; %)")


rownames(plotData) <- c("Genome size","# of annotated genes","# of annotated exons","Avg. # of exons per gene","Sum of gene length","Sum of exon lengths","Avg. gene length","Avg. exon length","Prop. exonic regions","Prop. genic region","Overlapping lengths","Prop. exonic overlapping regions")

plotData.temp <- as.data.frame(plotData)


plotData <- plotData[order(rownames(plotData), decreasing = T),]

plotData <- reshape2::melt(plotData) 
plotData$value <- round(as.numeric(plotData$value),3)

colors <- c("#f0c648","#f2b95c","#dd7c2f",
            "#567e6a", "#22523a", "#1f3c35")


## Plot ##

Fig5A <- plotData %>% ggplot(aes(x = Var2, y=Var1, fill = value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)+
  geom_text(aes(label = value), 
            color = "white",size = 22,
            fontface="bold") +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20))+
  xlab("Empirical performance evaluation indicators at the quantification stage")+
  ylab("Structural statistics of gene annotation on genome")+
  scale_fill_gradientn(
    colours = colors,
    values = c(0,0.5,1))+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15),
        legend.title = element_blank(),
        axis.title = element_text(size = 20, face = "bold"),
        axis.text.y=element_text(size = 15),
        panel.grid.major = element_blank()
        )

Fig5A

ggsave("Figure5A.pdf",width = 15, height = 20,Fig5A)


#### Step 4-2. Scatter plot between average gene length and quantification success rate  ####
color <- paletteer::paletteer_d("ggthemes::stata_s2color")[c(1,2,3,4,6,5,8,9,14,10,7,11,13)]
RNAv102 <- read.csv("Data/RNASeq_quantification_data.csv")
RNAv102 <- RNAv102 %>% mutate(Species=as.factor(Species),Class=as.factor(Class),Label=as.factor(Label))

RNAv102$Species <- sapply(as.character(RNAv102$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)

plot <- ggplot(data=RNAv102,aes(x = Avg_gene_length, y = Assigned_rate ,color=Label, label = Species)) + 
  geom_jitter(size=20,alpha=0.8, stroke = 0) + 
  geom_text_repel(color = "black", fontface = "italic", size = 8) +
  scale_color_manual(values = color) + 
  ylab("Quantification success rate") + 
  xlab("Average gene length (kbp)") + 
  theme_classic() +  
  #geom_text_repel(aes(label=Species),color="black",size=3,fontface="italic") + 
  theme(legend.position="bottom",
        axis.text = element_text(size = 30,face = "bold"),
        axis.title = element_text(size = 30,face = "bold"),
        axis.ticks = element_blank(),
        axis.line = element_line(linewidth = 0.8))+
  guides(color=guide_legend(nrow=2,byrow=FALSE))


plot


ggsave(filename = paste0("Figure5B.pdf"),plot,width=25,height=15, limitsize = F)


#### Step 4-3. Association between the proportion of annotated genic regions in the genome and the rate of quantification failure due to ambiguity ####


data <- RNAv102 %>% dplyr::select(c(Species,Label,Unassigned_Ambiguity_rate,Prop_genes_in_genome))

data$Species <- sapply(as.character(data$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)

result <- rlm(Unassigned_Ambiguity_rate~Prop_genes_in_genome,data=data)


result <-lm(Unassigned_Ambiguity_rate~Prop_genes_in_genome,data=data) 


plot <- ggplot(data=data,aes(x = Prop_genes_in_genome,
                             y = Unassigned_Ambiguity_rate ,
                             color=Label)) + 
  geom_jitter(size=20, stroke = 0, alpha = 0.8) + 
  scale_color_manual(values = color) +
  geom_smooth(method = "rlm",color="#ED5564") +
  ylab("Quantification failure rate due to ambiguity") + 
  xlab("Proportion of annotated genic regions in the genome") +
  theme_classic() + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 30,face = "bold"),
        axis.title = element_text(size = 30,face = "bold"),
        axis.ticks = element_blank(),
        axis.line = element_line(linewidth = 0.8))+
  guides(color=guide_legend(nrow=2,byrow=FALSE))


plot
ggsave(filename = paste0("Figure5C.pdf"),plot,width=25,height=15, limitsize = F)

# RLM model
# Spearman correlation
SC <- round(cor(data[,"Unassigned_Ambiguity_rate"],data[,"Prop_genes_in_genome"],method="spearman"),3)
SC



#### Step 4-4. Correlation between the percentage of annotated genic regions in the genome and the rate of quantification failure due to the absence of annotation ####

RNAv102 <- RNAv102 %>% mutate(Species=as.factor(Species),Class=as.factor(Class),Label=as.factor(Label))

data <- RNAv102 %>% dplyr::select(c(Species,Label,Unassigned_NoFeatures_rate,Prop_genes_in_genome))

data$Species <- sapply(as.character(data$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)


result <- rlm(Unassigned_NoFeatures_rate~Prop_genes_in_genome,data=data)


temp <- lm(Unassigned_NoFeatures_rate~Prop_genes_in_genome,data=data)

summary(temp)
plot <- ggplot(data=data,aes(x = Prop_genes_in_genome, 
                             y = Unassigned_NoFeatures_rate ,
                             color=Label)) + 
  geom_jitter(size=20, stroke = 0, alpha = 0.8) + 
  scale_color_manual(values = color) + 
  geom_smooth(method = "rlm",color="#ED5564") +
  ylab("Quantification failure rate due to absence of annotation") + 
  xlab("Proportion of annotated genic regions in the genome")+
  theme_classic() + 
  theme(legend.position = "top",
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30,face = "bold"),
        axis.ticks = element_blank(),
        axis.line = element_line(linewidth = 0.8))+
  guides(color=guide_legend(nrow=2,byrow=FALSE))


plot


ggsave(filename = paste0("Figure5D.pdf"),plot,width=25,height=15)

# RLM model
result
# Spearman correlation
SC <- round(cor(data[,"Unassigned_NoFeatures_rate"],data[,"Prop_genes_in_genome"],method="spearman"),3)
SC



#### Step 4-5. Relationship between Transcript's diversity and quantification rate, a quantitative quality evaluation index ####



data <- RNAv102 %>% dplyr::select(c(Species,Label,Assigned_rate,Transcripts_diversity_PCA))

data$Species <- sapply(as.character(data$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)

result <- rlm(Assigned_rate~Transcripts_diversity_PCA,data=data)


result <- lm(Assigned_rate~Transcripts_diversity_PCA,data=data)

summary(result)


plot <- ggplot(data=data,aes(x = Transcripts_diversity_PCA, 
                             y = Assigned_rate ,
                             color=Label)) +
  geom_jitter(size=20, stroke = 0, alpha =0.8) + 
  geom_smooth(method = "rlm",
              color="#ED5564") + 
  scale_color_manual(values = color) + 
  ylab("Quantification success rate") + 
  xlab("Transcript's diversity index based on PCA") + 
  theme_classic() + 
  theme(legend.position = "top",
        axis.text = element_text(size = 30,face = "bold"),
        axis.title = element_text(size = 30,face = "bold"),
        axis.ticks = element_blank())+
  guides(color=guide_legend(nrow=2,byrow=FALSE))

plot

ggsave(filename = paste0("Figure5E.pdf"),plot,width=25,height=15)


#### Step 4-6. Differences of QQI in all species ####

data <- fread("Data/RNASeq_quantification_data_sample.csv")
data$Species <- as.factor(data$Species)
colnames(data) <- gsub("X1.","1-",colnames(data))
color <- paletteer::paletteer_d("ggthemes::stata_s2color")[c(1,2,3,4,6,5,8,9,14,10,7,11,13)]

# Extract Index info
indexName <- "QQI"

data$Species <- sapply(as.character(data$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)

# Re-factor Species on descending order of index 
subDF <- data %>% 
  group_by(Label,Species) %>% 
  summarise(avg=mean(get(indexName))) %>% 
  arrange(desc(avg))

data$Species <- factor(data$Species, levels = unique(subDF$Species))

t<-data %>% 
  group_by(Species) %>% 
  summarise(QQI=mean(QQI),
            Qr=mean(`Quantification rate`),
            QfA=mean(`1-Quantification failure rate due to ambiguity`),
            QfNF=mean(`1-Quantification failure rate due to absence of annotation`))

suppressWarnings({
  # Draw figure with categorized by species (*if you aim to categorized by Type, x axis should became Type*)
  fig <- ggplot(data=data,aes(x=Species,y=get(indexName))) + 
    geom_jitter(stat="identity",aes(color=Label),size=9, alpha=0.3, stroke = 0) +
    geom_boxplot(outlier.shape = NA, fill = NA, color = "black",lwd = 1.4) + 
    ylab(indexName) + 
    xlab("Species") + 
    scale_color_manual(values=color) + 
    theme_classic() + 
    ylab("Quantification quality evaluation index (QQI)") + 
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(ylim = c(0.4,1.05)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 40,face = "bold"),
          axis.text.y = element_text(size = 35),
          axis.text.x = element_text(size=35,angle = 90,vjust = 0, hjust = 1,face = "italic"),
          legend.position = "top") +
    guides(color=guide_legend(nrow=1,byrow=FALSE))
  
})

fig

ggsave("Figure5F.pdf",fig,width = 60, height = 13, limitsize = F)





