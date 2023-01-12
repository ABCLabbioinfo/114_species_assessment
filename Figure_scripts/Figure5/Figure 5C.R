

## Load library
library(pacman)
pacakges <- c("dplyr","ggplot2","data.table","stringr","readr","janitor","corrplot","pheatmap","pryr","MASS","ggpmisc","ggrepel","corrplot","corrr")
pacman::p_load(pacakges,character.only = TRUE)



## Figure5C: Proportion of annotated genes in the genome VS Unassigned Ambiguity rate gene 
color <- paletteer::paletteer_d("ggthemes::stata_s2color")[c(1,2,3,4,6,5,8,9,14,10,7,11,13)]
RNAv102 <- read.csv("../../Data/Fig5_data/102RNA.csv")
RNAv102 <- RNAv102 %>% mutate(Species=as.factor(Species),Class=as.factor(Class),Label=as.factor(Label))

data <- RNAv102 %>% dplyr::select(c(Species,Label,Unassigned_Ambiguity_rate,Prop_genes_in_genome))

data$Species <- sapply(as.character(data$Species),function(x) {paste0(unlist(strsplit(unlist(strsplit(x," "))[1],""))[1],".",unlist(strsplit(x," "))[2])},USE.NAMES = FALSE)

result <- rlm(Unassigned_Ambiguity_rate~Prop_genes_in_genome,data=data)


result <-lm(Unassigned_Ambiguity_rate~Prop_genes_in_genome,data=data) 

summary(result)

plot <- ggplot(data=data,aes(x = Prop_genes_in_genome,
                             y = Unassigned_Ambiguity_rate ,
                             color=Label)) + 
  geom_jitter(size=20, stroke = 0, alpha = 0.8) + 
  scale_color_manual(values = color) +
  geom_smooth(method = "rlm",color="#ED5564") +
  ylab("Quantification failure rate due to ambiguity") + 
  xlab("Proportion of annotated genic regions in the genome") +
  theme_classic() + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 30,face = "bold"),
        axis.title = element_text(size = 30,face = "bold"),
        axis.ticks = element_blank(),
        axis.line = element_line(linewidth = 0.8))+
  guides(color=guide_legend(nrow=2,byrow=FALSE))


plot
ggsave(filename = paste0("Figure5C.pdf"),plot,width=25,height=15, limitsize = F)

# RLM model
# Spearman correlation
SC <- round(cor(data[,"Unassigned_Ambiguity_rate"],data[,"Prop_genes_in_genome"],method="spearman"),3)
SC
