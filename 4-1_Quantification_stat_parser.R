#### This script for calculation of quantification rate. We found mistakes in calculation of quantification, so we corrected calculation method for quantification rate. Below script was calculate of quantification rate for just one species ####

source("Package_manager.R")


#### Below code was for before correction of calculation in quantification rate ####

## Read FeautureCounts summary file and make them to dataframe ##
dir <- "Data/Example_data/" 
# Set each species path and access to sample file names 
files <- list.files(dir, pattern = "*.summary")
name <- "sus_scrofa"
totalList <- list()
# Read FeatureCounts summary file #
for (each in files){
  sampleID <- unlist(str_split(each,fixed(".")))[1]
  data <- fread(paste0(dir,"/",each))
  colnames(data) <- c("index","rate") 
  #data[,2] <- apply(data[,2],1,function(x){x/sum(data[,2])})
  data <- as.data.frame(t(data) %>% row_to_names(row_number = 1)) %>% mutate_if(is.character,as.numeric)
  colnames(data) <- paste0("Gene_",colnames(data),"_rate")
  data <- data %>% mutate(Species=name,SampleID=sampleID,.before="Gene_Assigned_rate") 
  totalList[[paste0(name,"/",sampleID)]] <- data
}

GeneStat <- Reduce(function(df1,df2) merge(df1,df2,all=TRUE),totalList)


## Read HISAT2 log file and save to dataframe ##
index <- c("Total_paired_reads","Percentage_of_aligned_concordantly_0_times","Percentage_of_aligned_concordantly_1_time","Percentage_of_aligned_concordantly_>1_times","Percentage _of_aligned_discordantly_1_time","Percentage_of_aligned_0_times","Percentage_of_aligned_1_time","Percentage_of_aligned_>1_times","Overall_alignment_rate")

totalList <- list()
dir <- "Data/Example_data/" 
# Set each species path and access to sample log file names 

files <- list.files(dir, pattern = "*.log")
name <- "sus_scrofa"
for (each in files){
  sampleID <- unlist(str_split(each,fixed(".")))[1]
  suppressWarnings({
    # Read sample log files
    data <- read_log(paste0(dir,"/",each),progress = FALSE,show_col_types = FALSE)
  })
  # Extract statistics from log file
  value <- unlist(c(data[1,1],data[3,2],data[4,2],data[5,2],data[8,2],data[12,2],data[13,2],data[14,2],data[15,1]))
  names(value) <- index
  data <- data.frame(value)
  data$value <- gsub("(","",data$value,fixed = TRUE)
  data$value <- gsub(")","",data$value,fixed = TRUE)
  data$value <- gsub("%","",data$value)
  data <- as.data.frame(t(data))
  data <- data %>% mutate(Species=name,SampleID=sampleID,.before="Total_paired_reads") 
  totalList[[paste0(name,"/",sampleID)]] <- data
}
HisatStat <- Reduce(function(df1,df2) merge(df1,df2,all=TRUE),totalList)


### Calculation of quantification rate part ###

##### Calculation method before correction of quantification rate #######
# Quant.rate #
# Total_paired_reads variable is total reads of each samples #
# mean(GeneStat$Gene_Assigned_rate/(as.numeric(HisatStat$Total_paired_reads)*2)) 
# 
# # Quant.rate(Abs) #
# 1-mean(GeneStat$Gene_Unassigned_NoFeatures_rate/(as.numeric(HisatStat$Total_paired_reads)*2))
# 
# # Quant.rate(Amb) #
# 1-mean(GeneStat$Gene_Unassigned_Ambiguity_rate/(as.numeric(HisatStat$Total_paired_reads)*2))



##### Calculation method After correction of quantification rate ######
# Sum of Gene_Assigned_rate, Gene_Unassigned_Ambiguity_rate and Gene_Unassigned_NoFeatures_Rate variables was mapped reads # 

# Quant.rate #
mean(GeneStat$Gene_Assigned_rate/(as.numeric(GeneStat$Gene_Assigned_rate+GeneStat$Gene_Unassigned_Ambiguity_rate+GeneStat$Gene_Unassigned_NoFeatures_rate)))


# Quant.rate(Abs) #
1-mean(GeneStat$Gene_Unassigned_NoFeatures_rate/(as.numeric(GeneStat$Gene_Assigned_rate+GeneStat$Gene_Unassigned_Ambiguity_rate+GeneStat$Gene_Unassigned_NoFeatures_rate)))


# Quant.rate(Amb) #
1-mean(GeneStat$Gene_Unassigned_Ambiguity_rate/(as.numeric(GeneStat$Gene_Assigned_rate+GeneStat$Gene_Unassigned_Ambiguity_rate+GeneStat$Gene_Unassigned_NoFeatures_rate)))


