# Load package
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
library(data.table)
library(googlesheets4)

conflicts_prefer(openxlsx::write.xlsx)
conflicts_prefer(dplyr::filter)

setwd("E:/DMU Projects/wgs-qc")

# Google Sheet ID extracted from the URL
sheet_id <- "1rOFp9VHXeAfghFU6aOOBeBSt9Q1RC5WVAdLez4HHLU4"

# Read the sheet
df_qualifyr <- read_sheet(sheet_id, sheet = "qualifyr")
df_bactopia <- read_sheet(sheet_id, sheet = "bactopia-report")
df_checkm2 <- read_sheet(sheet_id, sheet = "checkm2")
df_mlst <- read_sheet(sheet_id, sheet = "mlst", col_names = FALSE)
df_amrfinderplus <- read_sheet(sheet_id, sheet = "amrfinderplus")




#Get the WGS QC File
get_batchname <- dlgInput("Enter batch number:", Sys.info()[" "])$res
get_samplesheet <- dlgInput("Enter sample sheet file name:", Sys.info()[" "])$res





# Specify the path to your text file
file_path <- paste('data_files/',get_batchname,'.txt', sep='') 

# Read the IDs from the text file
id_list <- scan(file_path, what = "")

#list of query for txt file
#id_list <- scan("to_query.txt", character(), quote = "")

#change dash to underscore
id_list <- gsub("-", "_", id_list)

# Escape single quotes in the strings
id_list <- gsub("'", "''", id_list)

# Convert the list of IDs to a comma-separated string enclosed in single quotes
#id_string <- paste0("'", paste(id_list, collapse = "','"), "'")




#merge qualifyr, checkm2 and bactopia results
#rename column
colnames(df_qualifyr)[which(names(df_qualifyr) == "bactinspector.species.metric_value")] <- "species"
colnames(df_qualifyr)[which(names(df_qualifyr) == "sample_name")] <- "name"
colnames(df_bactopia)[which(names(df_bactopia) == "sample")] <- "name"
colnames(df_bactopia)[which(names(df_bactopia) == "species")] <- "species_name"

#checkm2 column names to lower
names(df_checkm2) <- tolower(names(df_checkm2))

#remove ".fna"
df_checkm2$name <- gsub(".fna", "", df_checkm2$name, fixed=TRUE)

#merge df
quality_report <- merge(df_qualifyr, df_checkm2, by = "name")
quality_report <- merge(quality_report, df_bactopia, by = "name")


wgs_df <- subset(quality_report, select = c(name, species, completeness, contamination, rank, qc_final_coverage,
                                            assembler_total_contig_length, gc_content , assembler_total_contig,
                                            assembler_n50_contig_length, qc_final_qual_mean))


wgs_df$name <- gsub("-", "_", wgs_df$name, fixed=TRUE)
wgs_df$name <- toupper(wgs_df$name)



#filter wgs_df based on id_string
wgs_df <- wgs_df[wgs_df$name %in% id_list, ]




# Define a function to remove the second underscore if the string doesn't contain the specific text
remove_second_underscore <- function(x) {
  if (!grepl('UTP', x)) {
    x <- gsub("^(.*?_.*?)_(.*)", "\\1\\2", x)
  }
  return(x)
}




# Apply the function to the column of strings
wgs_df <- wgs_df %>%
  rename(sample_id = name) %>%
  mutate(sample_id = sapply(sample_id, remove_second_underscore))



#Check if STC sample is present in the id list
stc_sample <- grep("STC", wgs_df[['sample_id']], value = TRUE)
stc_sample_count <- length(stc_sample)

if (stc_sample_count !=0){
  wgs_df$sample_id <- gsub("STC", "STC_", wgs_df$sample_id, fixed=TRUE)
}



referred_df <- read_xlsx("data_files/Combined Data on Referred Isolates_06.13.25.xlsx", sheet="ARSRL")
referred_df <- subset(referred_df, select = c(accession_no,arsrl_org))
result <- referred_df[referred_df$accession_no %in% id_list, ]
colnames(result) <- c('sample_id','arsrl_org') 

# Returns string without leading or trailing white space
result$arsrl_org <- gsub("^\\s+|\\s+$", "", result$arsrl_org)

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
  
  arsrl_result_df <- result %>% 
    add_row(sample_id = sample_id, arsrl_org=arsrl_org)
  
  arsrl_result_df <- subset(arsrl_result_df , select = c(sample_id,arsrl_org))
  colnames(arsrl_result_df) <- c('sample_id','arsrl_org') 
  
}else{
  arsrl_result_df <- subset(result , select = c(sample_id,arsrl_org))
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


wgs_df1 <- wgs_df
#wgs_df <- wgs_df1




if(nrow(wgs_df) != 0){
  rmarkdown::render("wgs_qc_report_ver5.Rmd",
                    output_file = paste("qualifyr_report_",get_batchname, '.pdf', sep='')
  ) 
}else{
  cat("No file found")
}
