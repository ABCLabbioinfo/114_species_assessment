#### Script for heatmap with NGS applicable index and polygon chart ####
source("Package_manager.R")


#### Heatmap ####


data <- fread("Data/Applicable_index_with_97_species.csv",stringsAsFactors = TRUE) %>% as.data.frame()




data$Species <- sapply(as.character(data$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)
data <- data %>% arrange(desc(`NGS applicability index`)) 
rownames(data) <- data$Species
data <- data[,-1]
# colnames(data.1) <- gsub("."," ",colnames(data.1),fixed = TRUE)

data<- round(data,4)

data %>% names()

idx <- c("Quant.rate(Amb)","Quant.rate(Abs)","Quant.rate","Transcript's diversity",
  "AdjN50Contig","AdjN50Scaffold","UngapRate",
  "MultiMapRate","MapRate","UnimapRate",
  "NGS applicability index")

data <- data %>% dplyr::select(all_of(idx))

names(data)[names(data) == "Transcript's diversity"] <- "Transcript diversity"


plt3 <- pheatmap(data,cluster_rows = FALSE,
                 cluster_cols = FALSE,
                 fontsize_row=32,
                 fontsize_col=32,
                 number_format = "%.3f",
                 color = colorRampPalette(c("#2B3467","#BAD7E9","#FCFFE7","#EB455F"))(100),
                 display_numbers = T)

ggsave("Figrue6A-4.1.pdf",plt3,width = 18, height = 45) 



#### Polygon chart ####

data <- read.csv("Data/Applicable_index_with_97_species.csv",stringsAsFactors = TRUE)
colnames(data) <- gsub("X1.","1-",colnames(data))

livestock <- c("Bos grunniens","Bos taurus","Capra hircus","Equus caballus","Anas platyrhynchos","Ovis aries","Sus scrofa","Gallus gallus")

color <- c(rgb(153,141,203,205,maxColorValue = 255),
           rgb(141, 160, 203,205,maxColorValue = 255),
           rgb(102, 194, 165,205,maxColorValue = 255),
           rgb(191, 163, 122,205,maxColorValue = 255),
           rgb(250, 223, 140,205,maxColorValue = 255),
           rgb(166, 216, 84,205,maxColorValue = 255),
           rgb(226, 126, 128,205,maxColorValue = 255),
           rgb(222, 169, 177,205,maxColorValue = 255))

color2 <- c(rgb(153,141,203,80,maxColorValue = 255),
            rgb(141, 160, 203,80,maxColorValue = 255),
            rgb(102, 194, 165,80,maxColorValue = 255),
            rgb(191, 163, 122,80,maxColorValue = 255),
            rgb(250, 223, 140,80,maxColorValue = 255),
            rgb(166, 216, 84,80,maxColorValue = 255),
            rgb(226, 126, 128,80,maxColorValue = 255),
            rgb(222, 169, 177,80,maxColorValue = 255))

colnames(data)

for(i in 1:length(livestock)){
  print(livestock[i])
  name <- livestock[i]
  col <- color[i]
  col2 <- color2[i]
  
  df <- data %>% dplyr::filter(Species==name|Species=="Mus musculus"|Species=="Homo sapiens"|Species=="Arabidopsis thaliana") %>% arrange(NGS.applicability.index)
  df <- df[,-c(1,12)]
  df <- rbind(rep(1,10) , rep(0,10) , df)
  
  # Color vector
  colors_border=c(col,rgb(24, 2, 112,185,maxColorValue = 255),rgb(1, 59, 3,185,maxColorValue = 255),rgb(89, 0, 24,185,maxColorValue = 255))
  colors_in=c(col2,NA,NA,NA)
  
  # plot with default options:
  plt <- radarchart( df , axistype=1 , 
                     #custom polygon
                     pcol=colors_border , pfcol=colors_in , plwd=4 , plty=c(1,8,8,8),
                     #custom the grid
                     cglcol="grey50", cglty=1, axislabcol="grey50", caxislabels=seq(0,1,length=5), cglwd=1.2,
                     #custom labels
                     vlcex=0.8)
  
  
  
}



