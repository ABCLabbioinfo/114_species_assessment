library(pacman)
pacakges <- c("dplyr","ggplot2","data.table","stringr","readr","janitor","corrplot","pheatmap","ggdendro","plotly","pryr","MASS","ggpmisc","ggrepel","RColorBrewer")
pacman::p_load(pacakges,character.only = TRUE)

temp <- read.csv("../../Data/Fig3_data//108RNA.csv")
temp <- temp[,-c(1:3)]
NAcol <- which(apply(temp,2,function(x) sum(is.na(x)))!=0)
Var0col <- which(apply(temp,2,function(x) var(x)==0))


x <- temp[,c(1:4)]
y <- temp[,c(7,18:24)]
plotData <- cor(x,y, method = "spearman")

plotData.t <- (plotData)

cor_mat <- plotData.t

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



# hide axis ticks and grid lines
eaxis <- list(
  showticklabels = FALSE,
  showgrid = FALSE,
  zeroline = FALSE
)


p_empty <- plotly_empty()

plot.new()



subplot(px, p_empty, p, py, nrows = 2, margin = 0.01)


