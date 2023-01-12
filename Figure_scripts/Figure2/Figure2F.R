library(ggplot2)
library(data.table)
library(ggcorrplot)
library(pheatmap)
library(corrplot)
library(dplyr)
library(pafr)
library(ggheatmap)
library(ggplot2)
library(ggdendro)
library(plotly)



data <- fread("../../Data/Fig2_data//Assembly_statistics.txt")

repeat_element <- fread("../../Data/Fig2_data/Integration_data.txt")

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

rownames(cor_mat)

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
  
  # plot_ly() %>% 
  # layout(margin = list(l = 200),
  #        xaxis = eaxis,
  #        yaxis = eaxis)
plot.new()


  # note that margin applies to entire plot, so we can
  # add it here to make tick labels more readable

subplot(px, p_empty, p, py, nrows = 2, margin = 0.01)




