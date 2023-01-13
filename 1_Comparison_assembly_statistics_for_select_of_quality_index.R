
library(ggplot2)
library(data.table)
library(ggcorrplot)
library(pheatmap)
library(corrplot)
library(stringr)
library(pafr)
library(ggdendro)
library(plotly)
library(ggheatmap)

#### Step 1-1. Investigation of correlation between assembly statistics and repeat elements for selection of effective quality index ####

data <- fread("Data/Assembly_statistics.txt")
color.df <- fread("Data/Assembly_color_code.txt")
color <- color.df$color
names(color) <- color.df$label


data$organism[which(data$organism=="Livestock")] <- "Livestock animals"
levels <- data$organism[-which(grepl(data$organism,pattern="Human & Mouse|Livestock animals"))]
data$organism <- factor(data$organism,levels=c("Human & Mouse","Livestock animals",unique(levels)))


data$`Number of spanned gaps` <- data$`Number of spanned gaps`/1
data$`Length of masekd repeat elements` <- data$`Length of masekd repeat elements`/1
data$`Genome size` <- data$`Genome size`/1
data$`Ungapped base pair` <- data$`Ungapped base pair`/1
data$`Adjusted N50 in scaffold` <- data$`Adjusted N50 in scaffold`/1
data$`Adjusted N50 in contig` <- data$`Adjusted N50 in contig`/1

temp <- cor(data[,4:16])

col1 <- colorRampPalette(c("#00c3ff" , "#ffff1c"))
col2 <- colorRampPalette(c("#40E0D0" , "#FF8C00","#FF0080"))

col3 <- colorRampPalette(c("#0c1db8", "#7046aa", "#ff7882", "#fda34b"))

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



#### Step 1-2. The number of spanned gaps in genome of 109 species ####

ggplot(data=data,aes(x=organism,y=data$`Number of spanned gaps`,color=organism))+
  geom_jitter(aes(color=organism), 
              position=position_jitter(width = .3), 
              size=6,stroke = 0) + 
  geom_boxplot(color = "black",fill = NA, 
               show.legend = F,  
               outlier.shape = NA,
               lwd = 1) + 
  ylab("Number of spanned gaps")+
  labs(title="")+ ##Number of spanned gaps according to organism
  theme_bw()+
  theme_classic()+
  theme(axis.title = element_text(size=25, face = "bold"),
        axis.text.y = element_text(size= 25, face = "bold"), 
        axis.text.x = element_text(size= 11, face = "bold"), 
        axis.title.x = element_blank(),
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color))+
  theme(legend.position="top")+
  guides(color=guide_legend(nrow=2,byrow=FALSE))



#### Step 1-3. Comparison of adjusted N50 scaffold in genome size  ####

ggplot(data=data,aes(x=organism,y=data$`Adjusted N50 in scaffold`,color=organism))+
  geom_jitter(aes(color=organism), 
              position=position_jitter(width = .3), 
              size=6,stroke = 0) + 
  geom_boxplot(color = "black",fill = NA, 
               show.legend = F,  
               outlier.shape = NA,
               lwd = 1) + 
  ylab("Adjusted N50 in scaffold")+
  labs(title="")+
  theme_bw()+
  theme_classic()+
  theme(axis.title = element_text(size=25, face = "bold"),
        axis.text.y = element_text(size= 25, face = "bold"), 
        axis.text.x = element_text(size= 9.5, face = "bold"), 
        axis.title.x = element_blank(),
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color))+
  theme(legend.position="none")




#### Step 1-4. Comparison of adjusted N50 contig in genome size  ####

ggplot(data=data,aes(x=organism,y=data$`Adjusted N50 in contig`,color=organism))+
  geom_jitter(aes(color=organism), 
              position=position_jitter(width = .3), 
              size=6,stroke = 0) + 
  geom_boxplot(color = "black",fill = NA, 
               show.legend = F,  
               outlier.shape = NA,
               lwd = 1) + 
  ylab("Adjusted N50 in contig")+
  labs(title="")+ ##Adjusted N50 in contig according to organism
  theme_bw()+
  theme_classic()+
  theme(axis.title = element_text(size=25, face = "bold"),
        axis.text.y = element_text(size= 25, face = "bold"), 
        axis.text.x = element_text(size= 9.5, face = "bold"), 
        axis.title.x = element_blank(),
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color))+
  theme(legend.position="none")


#### Step 1-5. Correlation between length of repeat elements and genome size in species ####

ggplot(data=data,aes(x=data$`Genome size`,data$`Length of masekd repeat elements`,color=organism))+
  geom_point(size=5)+
  ylab("Length of masked repeat elements (bp)")+
  labs(title="")+ ## Length of masked repeat elements according to genome size
  xlab("Genome size (bp)")+
  scale_x_continuous(labels=Gb_lab)+
  scale_y_continuous(labels=Gb_lab) +
  theme_bw()+
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5))+
  scale_color_manual(values = c(color))+ 
  geom_smooth(method="lm",se=T,alpha=0.3,color="red") +
  theme(axis.title = element_text(size=25, face = "bold"),
        axis.text.y = element_text(size= 25, face = "bold"), 
        axis.text.x = element_text(size= 25, face = "bold"), 
        axis.title.x = element_blank(),
        legend.title = element_blank()) + 
  theme(legend.position="none") 


#### Step 1-6. Correlation between reference genome assembly statistics and repeat elements discovered in 108 species ####


repeat_element <- fread("Data/Repeat_elements_data.txt")

temp <- repeat_element %>% select(colnames(repeat_element)[which(grepl(colnames(repeat_element),pattern="*_percentage_of_sequence"))])


data <- data %>% select(-c("species","organism","species_name"))

x <- data

temp <- temp %>% select(-c("Total_interspersed_repeats_percentage_of_sequence"))
colnames(temp)<- gsub(colnames(temp),pattern="_percentage_of_sequence",replacement = "")

colnames(temp) <- gsub(colnames(temp),pattern="_",replacement = " ")
y <- temp


cor_mat <- cor(x,y,method="spearman")

cor_mat <- cor_mat[ , colSums(is.na(cor_mat))==0]


cor_mat <- na.omit(cor_mat)

## ver 3 


#dendogram data
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

idx <- c("Length of masekd repeat elements",
         "Ungapped base pair",
         "Genome size",
         "Adjusted N90 in contig",
         "Adjusted N75 in contig",
         "Adjusted N50 in contig",          
         "Proportion of repeat elements",
         "Adjusted N50 in scaffold",
         "Adjusted N90 in scaffold",
         "Adjusted N75 in scaffold",   
         "Contig L50",
         "Number of spanned gaps",
         "Number of contigs")

df$names <- with(df,factor(names, levels = idx[length(idx):1], ordered = TRUE))

mdf <- reshape2::melt(df, id.vars="names")


mdf$value <- sprintf("%0.3f", mdf$value)

mdf$value <- as.numeric(mdf$value)

p <- ggplot(mdf, aes(x = variable, y = names,fill = value)) + 
  geom_tile( color = "white",
             lwd = 1.5,
             linetype = 1)+
  geom_text(aes(label = value), 
            color = "white",
            size = 18) + 
  scale_fill_gradientn(colours = colorRampPalette(c("#264653", "#e9c46a","#e76f51"))(100),
                       space="Lab", 
                       values = c(-1,-0.1,1), guide = "colourbar") + ## "#7046aa", "#ff7882", ,"#0c1db8",  "#fda34b"
  theme_minimal() + 
  guides(fill = guide_colourbar(barwidth = 3,
                                barheight = 120)) +
  theme(axis.text.x = element_text(size = 40,angle = 90, vjust = 0.5, hjust=1, face = "bold"),
        axis.text.y = element_text(size = 40, face = "bold"),
        legend.text = element_text(size = 40),
        legend.title = element_blank())

p

ggsave("p.pdf", width = 96, height = 30, limitsize = F, p)



# hide axis ticks and grid lines
eaxis <- list(
  showticklabels = FALSE,
  showgrid = FALSE,
  zeroline = FALSE
)


p_empty <- plotly_empty()  

plot.new()

subplot(px, p_empty, p, py, nrows = 2, margin = 0.01)







