library(tidyverse)
library(knitr)
library(readxl)
library(kableExtra)
library(RColorBrewer)
library(scales)
library(readr)
library(DBI)
library(svDialogs)
library(writexl)
library(conflicted)

conflicts_prefer(openxlsx::write.xlsx)
conflicts_prefer(dplyr::filter)

setwd("E:/DMU Projects/wgs-qc")

#Get the WGS QC File
get_batchname <- dlgInput("Enter batch number:", Sys.info()[" "])$res
get_samplesheet <- dlgInput("Enter sample sheet file name:", Sys.info()[" "])$res
num_qr <- dlgInput("Enter number of quality reports :", Sys.info()[" "])$res
quality_report_num <- as.numeric(num_qr) - 1


if(quality_report_num > 1){
  get_file <- paste0("data_files/",get_batchname,"/quality_report.tsv")
  quality_report <- read.delim(file= get_file)
  
  get_file <- paste0("data_files/",get_batchname,"/bactopia-report.tsv")
  bactopia_report <- read.delim(file= get_file)
  
  
  
  for (i in 1:quality_report_num) {
    quality_report <- quality_report
    
    get_file <- paste0("data_files/",get_batchname,"/quality_report",i,".tsv")
    qr_df <- read.delim(file= get_file)
    
    qr_df$Name <- gsub(".fna", "", qr_df$Name, fixed=TRUE)
    
    get_file <- paste0("data_files/",get_batchname,"/bactopia-report",i,".tsv")
    br_df <- read.delim(file= get_file)
    
    
    df <- cbind()
    
    quality_report <- rbind(quality_report, df)
    
  }
  
  
}









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

# taking input with showing the message
#get_file <- dlgInput("Enter a text filename", Sys.info()[" "])$res
id_list <- na.omit(wgs_df[['sample_id']])
id_list <- gsub("\\_", "_", id_list)

# Connect to db
con <- dbConnect(RPostgres::Postgres(),dbname = 'WGS_DB', 
                 host = '10.10.103.163', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'postgres',
                 password = 'secret123')


# Escape single quotes in the strings
id_list <- gsub("'", "''", id_list)

# Convert the list of IDs to a comma-separated string enclosed in single quotes
id_string <- paste0("'", paste(id_list, collapse = "','"), "'")


query <- paste("SELECT * from wgs_app_referreddb 
               WHERE wgs_app_referreddb.sample_name IN (", id_string, ")", sep="")

df <- dbSendQuery(con, query)

result <- dbFetch(df)
result <- result %>% mutate_all(as.character)
result <- result %>% mutate_all(~as.character(ifelse(. == "nan", "", .)))

result$arsrl_org <- gsub("\\*", "",result$arsrl_org)

#result <- read_xlsx("data_files/result.xlsx")

#Check if UTP sample is present in the id list
utp_sample <- grep("UTP", wgs_df[['sample_id']], value = TRUE)
utp_sample_count <- length(utp_sample)



if(utp_sample_count !=0){
  sample_name = utp_sample
  arsrl_org = "Escherichia coli"

  arsrl_result_df <- result %>% 
    add_row(sample_name = sample_name, arsrl_org=arsrl_org)
  
  arsrl_result_df <- subset(arsrl_result_df , select = c(sample_name,arsrl_org))
  colnames(arsrl_result_df) <- c('sample_id','arsrl_org') 

}else{
  arsrl_result_df <- subset(result , select = c(sample_name,arsrl_org))
  colnames(arsrl_result_df) <- c('sample_id','arsrl_org') 
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


#wgs_df <- read_xlsx("wgs_df_2024-05-03.xlsx")

if(nrow(wgs_df) != 0){
  rmarkdown::render("wgs_qc_report_ver7.Rmd",
                    output_file = paste("qualifyr_report_",get_batchname, '.pdf', sep='')
  ) 
}else{
  cat("No file found")
}




#(wgs_df, file="wgs_qc_data.csv")
#write.csv(result, file="result_wgs_qc_data.csv")

