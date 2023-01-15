#### Step 2 Investigation of association relationship between assembly statistics and empirical effective indicators obtained in the mapping step for genome quality evaluation ####
setRepositories(ind=1:7)
library(pacman)
library(bit64)
pacakges <- c("dplyr","ggplot2","data.table","stringr","readr","janitor","corrplot","pheatmap","ggdendro","plotly","pryr","MASS","ggpmisc","ggrepel","RColorBrewer")
pacman::p_load(pacakges,character.only = TRUE)

#### Step 2-1. Correlation between selected assembly statistics and empirical effective indicators obtained from mapping step for assessment of genome quality ####

temp <- read.csv("Data/Selected_assembly_and_mapping_statistics_data.csv")
temp <- temp[,-c(1:3)]
NAcol <- which(apply(temp,2,function(x) sum(is.na(x)))!=0)
Var0col <- which(apply(temp,2,function(x) var(x)==0))


x <- temp[,c(1:4)]
y <- temp[,c(7,18:24)]
plotData <- cor(x,y, method = "spearman")
plotData.t <- (plotData)
cor_mat <- plotData.t

x <- cor_mat

cor_mat <- cor_mat[nrow(cor_mat):1,]
dd.col <- as.dendrogram(hclust(dist(cor_mat)))
dd.row <- as.dendrogram(hclust(dist(t(cor_mat))))
dx <- dendro_data(dd.row)
dy <- dendro_data(dd.col)

# helper function for creating dendograms
ggdend <- function(df) {
  ggplot() +
    geom_segment(data = df, aes(x=x, y=y, xend=xend, yend=yend)) +
    labs(x = "", y = "") + theme_minimal() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid = element_blank())
}

# x/y dendograms
px <- ggdend(dx$segments)
py <- ggdend(dy$segments) + coord_flip() +scale_x_reverse()

px
py
ggsave("px.pdf", width = 40, height = 5, px)
ggsave("py.pdf", width = 5, height = 35, py)

# heatmap
col.ord <- order.dendrogram(dd.col)
row.ord <- order.dendrogram(dd.row)

xx <- cor_mat[col.ord, row.ord]
xx_names <- attr(xx, "dimnames")
df <- as.data.frame(xx)
colnames(df) <- xx_names[[2]]

df$names <- xx_names[[1]]

idx <- c("Spanned.gaps",
         "Masked.proportion",
         "Adj.N90.contig",
         "Adj.N50.contig",
         "Adj.N75.contig",
         "Adj.N90.scaf",
         "Adj.N50.scaf",
         "Adj.N75.scaf")

idx <- c("Unmapped.rate","Uniquely.mapped.rate","Overall.alignment.rate","Multiple.mapped.rate")

df$names <- with(df,factor(names, levels = idx[length(idx):1], ordered = TRUE))
mdf <- reshape2::melt(df, id.vars="names")
mdf$value <- sprintf("%0.3f", mdf$value)
mdf$value <- as.numeric(mdf$value)

p <- ggplot(mdf, aes(x = variable, y = names,fill = value)) + geom_tile( color = "white",
                                                                         lwd = 1.5,
                                                                         linetype = 1)+
  geom_text(aes(label = value), color = "white",size = 18) +
  scale_fill_gradientn(colours = colorRampPalette(c("#001d3d","#003566",
                                                             "#f48c06","#ffba08"))(100),space="Lab", values = c(-0.5,0,0.8,1), guide = "colourbar",) + ## "#7046aa", "#ff7882", ,"#0c1db8",  "#fda34b"
                                                               theme_minimal() +
  guides(fill = guide_colourbar(barwidth = 3,
                                barheight = 70)) +
  theme(axis.text.x = element_text(size = 40,angle = 90, vjust  = 0.5, hjust=1, face = "bold"),
        axis.text.y = element_text(size = 40, face = "bold"),
        legend.text = element_text(size = 40),
        legend.title = element_blank())

p



ggsave("p.pdf", width = 30, height = 20, limitsize = F, p)

#### Step 2-2. Relationship between multiple mapped reads rated and proportion of repeat elements in genome of 108 species ####
temp <- read.csv("Data/Selected_assembly_and_mapping_statistics_data.csv")
temp <- temp %>% mutate(Species=as.factor(Species),Class=as.factor(Class))

## Auto
x <- "Masked.proportion"
y <- "Multiple.mapped.rate"
color <- paletteer::paletteer_d("ggthemes::stata_s2color")[c(1,2,3,4,6,8,9,14,10,7,11,13)]

data <- temp %>% dplyr::select(c("Species","Label",x,y))

data$Species <- sapply(as.character(data$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)

result <- rlm(get(x)~get(y),data=data)

data$Species.1 <- data$Species

idx <- c("X.tropicalis",
         "D.melanogaster",
         "A.thaliana",
         "M.musculus",
         "H.sapiens",
         "A.platyrhynchos",
         "E.caballus",
         "S.scrofa",
         "G.gallus",
         "B.taurus",
         "C.hircus",
         "O.aries",
         "B.grunniens")

idx.1 <- which(!(data$Species.1 %in% idx))
data$Species.1[idx.1] <- ""


plot <- ggplot(data=data,aes(x = get(x), y = get(y) ,color=Label)) +
  geom_jitter(size=14, alpha=0.7, stroke = 0) +
  scale_color_manual(values = color) +
  geom_smooth(method = "rlm",color="#ED5564") +
  ylab(y) +
  xlab(x) +
  theme_classic() +
  geom_text_repel(aes(label=Species.1),color="black",size=8,fontface="italic") +
  ggtitle("") +  ## paste0("Spearman correlation : ",round(cor(data[,y],data[,x],method="spearman"),3))
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 25,face = "bold"),
        axis.title = element_text(size = 25,face = "bold"),
        axis.ticks = element_blank())+
  theme(legend.position="none")

plot

ggsave(filename = "Figure3B.pdf",plot,width=24,height=10.4)


# RLM model
result
# Spearman correlation
SC <- round(cor(data[,y],data[,x],method="spearman"),3)


#### Step 2-3. Differences in newly proposed MQI values in 108 species ####

data <- read.csv("Data/MQI_statistics_of_108_species.csv")
data <- data %>% mutate(Species=as.factor(Species))
color <- paletteer::paletteer_d("ggthemes::stata_s2color")[c(1,2,3,4,6,5,8,9,14,10,7,11,13)]


# Extract Index info
indexName <- "MQI"

data$Species <- sapply(as.character(data$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)
# Re-factor Species on descending order of index 
subDF <- data %>% group_by(Label,Species) %>% summarise(avg=mean(get(indexName))) %>% arrange(desc(avg))
data$Species <- factor(data$Species, levels = unique(subDF$Species))

# Customized x axis text
custom <- ifelse(subDF$Label == "Human & Mouse", "red", "black")

suppressWarnings({
  # Draw figure with categorized by species (*if you aim to categorized by Type, x axis should became Type*)
  fig <- ggplot(data=data,aes(x=Species,y=get(indexName))) + 
    geom_jitter(stat="identity",aes(color=Label),size=9, alpha=0.3, stroke = 0) +
    geom_boxplot(outlier.shape = NA, color = "black" , fill =NA, lwd = 1.4) + 
    ylab(indexName) + xlab("Species") + 
    scale_color_manual(values=color) + 
    theme_classic() + 
    ylab("Mapping quality index") + 
    geom_hline(yintercept = mean(data$MQI, na.rm=TRUE),color='#484848', lty='dashed', lwd=0.7) + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 40,face = "bold"),
          axis.text.y = element_text(size = 35),
          axis.text.x = element_text(size = 35, angle = 90, hjust = 1, face = "italic"),
          legend.title = element_blank(),
          legend.text = element_text(size=35))+
    theme(legend.position="none")  
})

fig

ggsave("Figure3C.pdf",fig,width = 60, height = 13,limitsize = FALSE)







