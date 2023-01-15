
#### Script for heatmap with NGS applicable index and polygon chart ####

source("Package_manager.R")

colnames(data) <- c("Species","Class","Label","Uniquely mapped rate (%)","1 - Unmapped rate (%)","1 - Multiple mapped rate (%)","Quant. success rate","Quant. failure (No feature; %)","Quant. failure (Ambiguity; %)","Scaled transcript diversity PCA","Scaled adj. N50 in contig","Scaled adj. N50 in scaffold","Scaled non spanned gaps rates","NGS applicability index")
# Figure setting
color <- paletteer::paletteer_d("ggthemes::stata_s2color")[c(1,2,3,4,6,5,8,14,10,7,11,13)]
data$Species <- sapply(as.character(data$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)
data <- data %>% arrange(desc(`NGS applicability index`)) 
rownames(data) <- data$Species
data.1 <- data[,-c(1,2,3)]
colnames(data.1) <- gsub("."," ",colnames(data.1),fixed = TRUE)


idx <- c("Quant  failure (Ambiguity; %)",
         "Quant  failure (No feature; %)",
         "Quant  success rate",
         "Scaled transcript diversity PCA",
         "Scaled adj  N50 in contig",
         "Scaled adj  N50 in scaffold", 
         "Scaled non spanned gaps rates",
         "1 - Multiple mapped rate (%)",
         "1 - Unmapped rate (%)",
         "Uniquely mapped rate (%)",
         "NGS applicability index")

data.1 <- data.1 %>% dplyr::select(all_of(idx))


plt3 <- pheatmap(data.1,cluster_rows = FALSE,
                 cluster_cols = FALSE,
                 fontsize_row=32,
                 fontsize_col=32,
                 color = colorRampPalette(c("#2B3467","#BAD7E9","#FCFFE7","#EB455F"))(100),
                 display_numbers = T)



ggsave("Figrue6A.pdf",plt3,width = 15, height = 45) 





##################################### Polygon chart #####################################

data <- read.csv("Data/Applicable_index_with_96_species.csv",stringsAsFactors = TRUE)
colnames(data) <- gsub("X1.","1-",colnames(data))
data <- data[,-c(2,3)]
colnames(data) <- c("Species","Uniquely mapped rate (%)","1 - Unmapped rate (%)","1 - Multiple \nmapped rate (%)","Quant. success \nrate","Quant. failure (No feature; %)","Quant. failure (Ambiguity; %)","Scaled transcript diversity PCA","       Scaled adj. N50\nin contig","       Scaled adj. N50\nin scaffold","Scaled non spanned\ngaps rates","NGS")

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

for(i in 1:length(livestock)){
  print(livestock[i])
  name <- livestock[i]
  col <- color[i]
  col2 <- color2[i]
  
  df <- data %>% dplyr::filter(Species==name|Species=="Mus musculus"|Species=="Homo sapiens"|Species=="Arabidopsis thaliana") %>% arrange(NGS)
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



