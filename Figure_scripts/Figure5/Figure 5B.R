

## Load library
library(pacman)
pacakges <- c("dplyr","ggplot2","data.table","stringr","readr","janitor","ggpmisc","ggrepel","corrplot","corrr","scales")
pacman::p_load(pacakges,character.only = TRUE)


## Figure5B: Average Gene length VS Assigned rate gene 
color <- paletteer::paletteer_d("ggthemes::stata_s2color")[c(1,2,3,4,6,5,8,9,14,10,7,11,13)]
RNAv102 <- read.csv("../../Data/Fig5_data/102RNA.csv")
RNAv102 <- RNAv102 %>% mutate(Species=as.factor(Species),Class=as.factor(Class),Label=as.factor(Label))

RNAv102$Species <- sapply(as.character(RNAv102$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)

RNAv102 %>% head

plot <- ggplot(data=RNAv102,aes(x = Avg_gene_length, y = Assigned_rate ,color=Label, label = Species)) + 
  geom_jitter(size=20,alpha=0.8, stroke = 0) + 
  geom_text_repel(color = "black", fontface = "italic", size = 8) +
  scale_color_manual(values = color) + 
  ylab("Quantification success rate") + 
  xlab("Average gene length (kbp)") + 
  theme_classic() +  
  #geom_text_repel(aes(label=Species),color="black",size=3,fontface="italic") + 
  theme(legend.position="bottom",
        axis.text = element_text(size = 30,face = "bold"),
        axis.title = element_text(size = 30,face = "bold"),
        axis.ticks = element_blank(),
        axis.line = element_line(linewidth = 0.8))+
  guides(color=guide_legend(nrow=2,byrow=FALSE))


plot


ggsave(filename = paste0("Figure5B.pdf"),plot,width=25,height=15, limitsize = F)

