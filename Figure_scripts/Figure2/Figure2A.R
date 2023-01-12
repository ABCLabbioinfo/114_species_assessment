library(ggplot2)
library(data.table)
library(ggcorrplot)
library(pheatmap)
library(corrplot)

data <- fread("../../Data/Fig2_data/Assembly_statistics.txt")


data$`Length of masekd repeat elements` <- data$`Length of masekd repeat elements`/1
data$`Genome size` <- data$`Genome size`/1
data$`Ungapped base pair` <- data$`Ungapped base pair`/1



temp <- cor(data[,4:16])

col1 <- colorRampPalette(c("#00c3ff" , "#ffff1c"))
col2 <- colorRampPalette(c("#40E0D0" , "#FF8C00","#FF0080"))

col3 <- colorRampPalette(c("#0c1db8", "#7046aa", "#ff7882", "#fda34b"))

pdf("Fig2A.ver.1.pdf")

corrplot.mixed(temp, 
               lower = "number", upper = "ellipse",
               tl.pos = "lt", 
               order="hclust",
               addrect=4,
               rect.lwd=3,
               number.digits = 3,
               tl.col="black",
               number.cex=0.5,
               insig="blank",
               tl.cex=1,
               upper.col = col3(500),
               lower.col = col3(500))


dev.off()
corrplot3 <- recordPlot()


# col4 <- colorRampPalette(c("#6e45e2","#88d3ce"))

col4 <- colorRampPalette(c("#6e45e2","#80d0c7"))

col4 <- colorRampPalette(c("#7028e4","#80d0c7"))

pdf("Fig2A.ver.2.pdf")

corrplot.mixed(temp, 
               lower = "number", upper = "ellipse",
               order="hclust",
               addrect=4,
               rect.col = 'black',
               rect.lwd=3,
               number.digits = 3,
               number.cex=0.5,
               tl.pos = "lt", 
               insig="blank",
               tl.col="black",
               tl.cex=1,
               upper.col = col4(500),
               lower.col = col4(500))

dev.off()
