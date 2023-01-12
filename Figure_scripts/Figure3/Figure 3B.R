## Load library
library(pacman)
pacakges <- c("dplyr","ggplot2","data.table","stringr","readr","janitor","corrplot","pheatmap","ggdendro","plotly","pryr","MASS","ggpmisc","ggrepel","RColorBrewer")
pacman::p_load(pacakges,character.only = TRUE)



##################################### Two elements correlation scatter plot #####################################
temp <- read.csv("../../Data/Fig3_data/108RNA.csv")
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
