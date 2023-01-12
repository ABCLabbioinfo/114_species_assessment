

## Load library
library(pacman)
pacakges <- c("dplyr","ggplot2","data.table","stringr","readr","janitor","corrplot","pheatmap","pryr","MASS","ggpmisc","ggrepel","corrplot","corrr")
pacman::p_load(pacakges,character.only = TRUE)


##################################### Figure5E #####################################

## With "Transcripts_diversity_PCA"
color <- paletteer::paletteer_d("ggthemes::stata_s2color")[c(1,2,3,4,6,5,8,9,14,10,7,11,13)]
RNAv102 <- read.csv("../../Data/Fig5_data/102RNA.csv")
RNAv102 <- RNAv102 %>% mutate(Species=as.factor(Species),Class=as.factor(Class),Label=as.factor(Label))

data <- RNAv102 %>% dplyr::select(c(Species,Label,Assigned_rate,Transcripts_diversity_PCA))

data$Species <- sapply(as.character(data$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)

result <- rlm(Assigned_rate~Transcripts_diversity_PCA,data=data)


result <- lm(Assigned_rate~Transcripts_diversity_PCA,data=data)

summary(result)


plot <- ggplot(data=data,aes(x = Transcripts_diversity_PCA, 
                             y = Assigned_rate ,
                             color=Label)) +
  geom_jitter(size=20, stroke = 0, alpha =0.8) + 
  geom_smooth(method = "rlm",
              color="#ED5564") + 
  scale_color_manual(values = color) + 
  ylab("Quantification success rate") + 
  xlab("Transcript's diversity index based on PCA") + 
  theme_classic() + 
  #geom_text_repel(aes(label=Species),color="black",size=3,fontface="italic") + 
  theme(legend.position = "top",
        axis.text = element_text(size = 30,face = "bold"),
        axis.title = element_text(size = 30,face = "bold"),
        axis.ticks = element_blank())+
  guides(color=guide_legend(nrow=2,byrow=FALSE))


ggsave(filename = paste0("Figure5E.pdf"),plot,width=25,height=15)

