
## Load library
library(pacman)
pacakges <- c("dplyr","ggplot2","data.table","stringr","readr","janitor","corrplot","pheatmap","pryr","MASS","ggpmisc","ggrepel","corrplot","corrr")
pacman::p_load(pacakges,character.only = TRUE)

##################################### Figure5F #####################################
data <- fread("../../Data/Fig5_data/102RNA_sample.csv")
data$Species <- as.factor(data$Species)
colnames(data) <- gsub("X1.","1-",colnames(data))
colnames(data)
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



# Customized x axis text
#custom <- ifelse(subDF$Label == "Human & Mouse", "red", "black")

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



##################################### Extract Legend function #####################################
get_only_legend <- function(plot) {
  # get tabular interpretation of plot
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  
  #  Mark only legend in plot
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  
  # extract legend
  legend <- plot_table$grobs[[legend_plot]]
  
  # return legend
  return(legend) 
}





