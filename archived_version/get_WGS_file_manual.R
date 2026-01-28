library(tidyverse)
library(knitr)
library(readxl)
library(dplyr)
library(kableExtra)
library(RColorBrewer)
library(scales)
library(readr)
library(DBI)
library(svDialogs)
library(xlsx)
library(writexl)
library(conflicted)

conflicts_prefer(openxlsx::write.xlsx)
conflicts_prefer(dplyr::filter)

setwd("D:/ALLYSA FILES/2024/DMU Projects/wgs-qc")

#Get the WGS QC File
get_batchname <- dlgInput("Enter batch number:", Sys.info()[" "])$res
get_samplesheet <- dlgInput("Enter sample sheet file name:", Sys.info()[" "])$res

get_file <- paste("data_files/qualifyr_report.tsv")
wgs_df <- read.delim(file= get_file)

wgs_df$sample_name <- gsub("-", "_", wgs_df$sample_name, fixed=TRUE)
wgs_df$sample_name <- toupper(wgs_df$sample_name)

#get MLST and AMR Genes tsv files
get_mlst <- paste("data_files/mlst.tsv")
mlst_df <- read.delim(file= get_mlst)

get_genes <- paste("data_files/amrfinderplus-genes.tsv")
AMR_df <- read.delim(file= get_genes)


# Define a function to remove the second underscore if the string doesn't contain the specific text
remove_second_underscore <- function(x) {
  if (!grepl('UTP', x)) {
    x <- gsub("^(.*?_.*?)_(.*)", "\\1\\2", x)
  }
  return(x)
}

# Apply the function to the column of strings
wgs_df <- wgs_df %>%
  mutate(sample_name = sapply(sample_name, remove_second_underscore))



#Check if STC sample is present in the id list
stc_sample <- grep("STC", wgs_df[['sample_name']], value = TRUE)
stc_sample_count <- length(stc_sample)

if (stc_sample_count !=0){
  wgs_df$sample_name <- gsub("STC", "STC_", wgs_df$sample_name, fixed=TRUE)
}



wgs_df[wgs_df == 'WARN'] <- 'WARNING'
wgs_df[wgs_df == 'FAIL'] <- 'FAILURE'

#Rename WGS QC dataframe column
wgs_df <- wgs_df %>% 
  rename(
    sample_id = sample_name,
    wgs_id = bactinspector.species.metric_value,
    check_result = bactinspector.result.check_result,
    dups_result = fastqc.1.Sequence.Duplication.Levels.check_result,
    contamination_value = confindr.percentage_contamination.metric_value,
    contamination_result =confindr.percentage_contamination.check_result,
    no_of_contigs_value = quast...contigs.....0.bp..metric_value,
    no_of_contigs_result = quast...contigs.....0.bp..check_result,
    gc_percent_value = quast.GC.....metric_value,
    gc_percent_result = quast.GC.....check_result,
    n50_value = quast.N50.metric_value,
    n50_result = quast.N50.check_result,
    total_length_value = quast.Total.length.....1000.bp..metric_value,
    total_length_result = quast.Total.length.....1000.bp..check_result
  )


write.xlsx(wgs_df, file = (paste("wgs_df_",get_batchname, '.xlsx', sep='')), row.names=FALSE)

#wgs_df <- read_xlsx("wgs_df_2024-12-20_P.xlsx")

#wgs_df$sample_id <- gsub('34','24',wgs_df$sample_id)

# taking input with showing the message
#get_file <- dlgInput("Enter a text filename", Sys.info()[" "])$res
id_list <- na.omit(wgs_df[['sample_id']])
id_list <- toupper(id_list)
id_list <- gsub("\\_", "_", id_list)


referred_df <- read_xlsx("data_files/2024 Referred Isolates 02.13.2024.xlsx", sheet="ARSRL")
referred_df <- subset(referred_df, select = c(accession_no,arsrl_org))
result <- referred_df[referred_df$accession_no %in% id_list, ]
colnames(result) <- c('sample_id','arsrl_org') 



#check if arsrl_result_df is exisiting
if (!exists("arsrl_result_df")) {
  arsrl_result_df <- result
}



#Check if UTP sample is present in the id list
utp_sample <- grep("UTP", wgs_df[['sample_id']], value = TRUE)
utp_sample_count <- length(utp_sample)



if(utp_sample_count !=0){
  sample_id = utp_sample
  arsrl_org = "Escherichia coli"

  arsrl_result_df <- arsrl_result_df %>% 
    add_row(sample_id = sample_id, arsrl_org=arsrl_org)
 

}


#manually add result for STC
if(stc_sample_count !=0){
  stc_df <- read_xlsx("data_files/ARSRL_SatScan_Results.xlsx")
  stc_df <- stc_df[stc_df$sample_id %in% stc_sample, ]
  
  stc_df$sample_id <- gsub("STC", "STC_", stc_df$sample_id, fixed=TRUE)
  
  arsrl_result_df <- rbind(arsrl_result_df,stc_df)

}


#Check if QC_BBR sample is present in the id list
bbr_sample <- grep("QC_BBR", wgs_df[['sample_id']], value = TRUE)
bbr_sample_count <- length(bbr_sample)

#manually add result for QC_BBR
if(bbr_sample_count !=0){
  sample_id = bbr_sample
  arsrl_org = "Bordetella bronchiseptica"
  
  arsrl_result_df <- arsrl_result_df %>% 
    add_row(sample_id = sample_id, arsrl_org=arsrl_org)

}


#id_list <- gsub("_", "\\_", id_list, fixed=TRUE)
#wgs_df <- read_xlsx("wgs_df_2024-05-03.xlsx")


#x <- wgs_df$sample_id[!wgs_df$sample_id %in% result$sample_id]





if(nrow(wgs_df) != 0){
  rmarkdown::render("wgs_qc_report_ver8.Rmd",
                    output_file = paste("qualifyr_report_",get_batchname, '.pdf', sep='')
  ) 
}else{
  cat("No file found")
}




#(wgs_df, file="wgs_qc_data.csv")
#write.csv(result, file="result_wgs_qc_data.csv")




