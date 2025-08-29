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
sheet_id <- "1NHu-RIgDeTPKgrnTTHS9qVbCe04tIeYX6k73Jm93gDw"

# Read the sheet
df_batch <- read_sheet(sheet_id, sheet = "batch")
df_gambit <- read_sheet(sheet_id, sheet = "gambit")
df_fastp <- read_sheet(sheet_id, sheet = "fastp_summary")
df_assembly <- read_sheet(sheet_id, sheet = "assembly-scan")
df_checkm2 <- read_sheet(sheet_id, sheet = "checkm2")
df_mlst <- read_sheet(sheet_id, sheet = "mlst")
df_amrfinderplus <- read_sheet(sheet_id, sheet = "amrfinderplus")

# Read referred database
referred_sheet_id <- "1JKtyRyLh0-ck2xCk0oIH4iidgs21996UKj69lvvlXAU"
referred_df <- read_sheet(referred_sheet_id, sheet = 1)


# enter batch code
batch_code <- dlgInput("Enter Batch Code:", Sys.info()[" "])$res

#get samplesheet file
get_samplesheet <- dlgInput("Enter sample sheet file name:", Sys.info()[" "])$res



#filter batch df
batch_df <- df_batch[df_batch$batch_code == batch_code, ]
sample_name <- batch_df$sample_name
sample_name <- gsub("-", "_", sample_name, fixed=TRUE)

#remove batch number from sample_name
sample_name_clean <- sub("(.*)_[^_]*$", "\\1", sample_name)


#merge qualifyr, gambit,checkm2 and bactopia results
#rename column
colnames(df_gambit)[which(names(df_gambit) == "sample")] <- "name"
colnames(df_assembly)[which(names(df_assembly) == "sample")] <- "name"
colnames(df_fastp)[which(names(df_fastp) == "sample")] <- "name"


#checkm2 column names to lower
names(df_checkm2) <- tolower(names(df_checkm2))

#remove ".fna"
df_checkm2$name <- gsub(".fna", "", df_checkm2$name, fixed=TRUE)


# Get species name
df_gambit$species <- case_when(
  df_gambit$predicted.rank == "species" ~ df_gambit$predicted.name,
  TRUE ~ df_gambit$next.name
)


# Compute total GC Content
df_assembly$gc_content <- as.numeric(df_assembly$contig_percent_g) + as.numeric(df_assembly$contig_percent_c) 



# Subset Columns per dataframe
gambit_sub_df <- subset(df_gambit, select = c(name, species))
checkm2_sub_df <- subset(df_checkm2, select = c(name, completeness, contamination))
assembly_sub_df <- subset(df_assembly, select = c(name, total_contig, total_contig_length, gc_content, n50_contig_length))
fastp_sub_df <- subset(df_fastp, select = c(name, after_total_bases, combined_qual_mean))



# put all data frames into list
df_list <- list(gambit_sub_df, checkm2_sub_df, assembly_sub_df, fastp_sub_df)

# merge all data frames in list
wgs_df <- df_list %>% reduce(full_join, by='name')


wgs_df$name <- gsub("-", "_", wgs_df$name, fixed=TRUE)
wgs_df$name <- toupper(wgs_df$name)



#filter wgs_df based on sample_name
wgs_df <- wgs_df[wgs_df$name %in% sample_name, ]




# Apply the function to the column of strings
wgs_df <- wgs_df %>% rename(sample_id = name)



#Check if STC sample is present in the id list
stc_sample <- grep("STC", wgs_df[['sample_id']], value = TRUE)
stc_sample_count <- length(stc_sample)

if (stc_sample_count !=0){
  wgs_df$sample_id <- gsub("STC", "STC_", wgs_df$sample_id, fixed=TRUE)
}




referred_df <- subset(referred_df, select = c(accession_no,arsrl_org))
result <- referred_df[referred_df$accession_no %in% sample_name_clean, ]
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
  sample_id = sub("(.*)_[^_]*$", "\\1", utp_sample)
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
  stc_df$sample_id <- sub("(.*)_[^_]*$", "\\1", stc_df$sample_id)
  
  arsrl_result_df <- rbind(arsrl_result_df,stc_df)
  
}


#Check if QC_BBR sample is present in the id list
bbr_sample <- grep("QC_BBR", wgs_df[['sample_id']], value = TRUE)
bbr_sample_count <- length(bbr_sample)

#manually add result for QC_BBR
if(bbr_sample_count !=0){
  sample_id = sub("(.*)_[^_]*$", "\\1", bbr_sample)
  arsrl_org = "Bordetella bronchiseptica"
  
  arsrl_result_df <- arsrl_result_df %>% 
    add_row(sample_id = sample_id, arsrl_org=arsrl_org)
  
}


#Check if QC_BBR sample is present in the id list
vc_sample <- grep("VC", wgs_df[['sample_id']], value = TRUE)
vc_sample_count <- length(vc_sample)

#manually add result for QC_BBR
if(vc_sample_count !=0){
  sample_id = sub("(.*)_[^_]*$", "\\1", vc_sample)
  arsrl_org = "Neisseria gonorrhoeae"
  
  arsrl_result_df <- arsrl_result_df %>% 
    add_row(sample_id = sample_id, arsrl_org=arsrl_org)
  
}



# Referred isolates with WGS result, but the referred data have not been endorsed for encoding to DMU
arsrl_id <- arsrl_result_df$sample_id
no_referred_data <- setdiff(arsrl_id, sample_name_clean)




wgs_df1 <- wgs_df
#wgs_df <- wgs_df1
arsrl_result_df1 <- arsrl_result_df
#arsrl_result_df <- arsrl_result_df1


if(nrow(wgs_df) != 0){
  rmarkdown::render("wgs_qc_report.Rmd",
                    output_file = paste("ARSRL_WGS_QC_report_",batch_code, '.pdf', sep='')
  ) 
}else{
  cat("No file found")
}
