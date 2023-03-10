# Repository description

This repositiory include source codes for study **"Benchmark study for evaluating the quality of reference genomes and gene annotations in 114 species"** submitted to the Frontiers in Veterinary Science (Under review)

# Script
**Package_manager.R**: This script is for installation and calling for required packages for this scripts. **You don't need to run this script.**

**1_Comparison_assembly_statistics_for_select_of_quality_index.R**: Script for comparison of assembly statistics for selection of effective indicators for genome quality evaluation of various species relatively.

**2_Relationship_assembly_statistics_and_mapping_statistics.R**: Script for inestigating assembly statisitcs and mapping statistics result from RNA-Sequencing data.


**3_Evaluation_of_gene_anntation.R**: This script is for quality assessment of gene annotation in various species.

**4_Selection_of_effective_indicator_in_quantification_process.R**: Script for contents of investigation of quantifiaction step in RNA-Sequencing analysis and gene annotation quality.

**4-1_Quantification_stat_parser.R**: Example codes for calculation of quantification rate in quantification step. 

**5_NGS_applicable_index.R**: Script for heatmap with NGS applicable index and polygon chart.




# Data

**Species.txt**: This file include scientific names of 114 species.

**Taxanomy_table.txt**: There are taxanomy class of 114 species, this information get from NCBI.

**Assembly_statistics.txt**: The assembly statistics getting from European Nucleotide Archive (ENA) of reference genome in 109 species was included.

**Gene_annotation_table.txt**: This table includes gene annotation from Ensembl BioMart, and result of principal component analysis with different types of genes in 102 species.

**MQI_statistics_of_108_species.csv**: Mapping quality evaluation index (MQI) information including mapping statistics of RNA-Sequencing analysis was included. 

**Repeat_elements_data.txt**: Repeat elements and proportion was saved in this file. Repeat elements was resulted from RepeatMasker.

**RNASeq_quantification_data.csv**: Result of RNA-Sequencing analysis of 102 species was saved including quantification quality evaluation index (QQI) and quantification statistics.

**RNASeq_quantification_data_sample.csv**: Effective qunatification statistics for assessment of quantification step was included in 3,060 RNA-Sequenicng samples.

**Data/Example_data/**: There are log files including hisat2 and quantification summary files for calculation of quantification rate.

**Selected_assembly_and_mapping_statistics_data.csv**: Data with assembly statistics and mapping statistics was saved.

**Applicable_index_with_97_species.csv**: The table included 10 effective statistics for assessement of genome assembly and gene annotation with NGS applicable index.

**Assembly_color_code.txt, Annotation_color_code.txt**: Figure color codes for assembly and annotation.

![Figure 6_corrected_psw](https://user-images.githubusercontent.com/122352598/215918028-38319204-8156-4a27-bf51-0ee5e4e87bed.jpg)

# Citation
TBA
