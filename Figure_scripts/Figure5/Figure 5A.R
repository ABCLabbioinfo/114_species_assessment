
## Load library
library(pacman)
pacakges <- c("dplyr","ggplot2","data.table","stringr","readr","janitor","corrplot","pheatmap","pryr","MASS","ggpmisc","ggrepel","corrplot","corrr","dplyr")
pacman::p_load(pacakges,character.only = TRUE)

##################################### Figure5A #####################################
corD <- fread("../../Data/Fig5_data/102RNA.csv")
colnames(corD) <- gsub("_"," ",colnames(corD))


# Remove unnecessary column
corD <- corD[,-c(1:3,5,6,23)]

## Pheatmap visualization
colnames(corD)
x <- corD[,c(1:3)]
y <- corD[,-c(1:3)]

  colnames(y)
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


#### Plot ####

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

