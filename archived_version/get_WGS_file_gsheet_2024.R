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
library(stringr)

conflicts_prefer(openxlsx::write.xlsx)
conflicts_prefer(dplyr::filter)

setwd("E:/DMU Projects/wgs-qc")


# Google Sheet ID extracted from the URL
sheet_id <- "1rOFp9VHXeAfghFU6aOOBeBSt9Q1RC5WVAdLez4HHLU4"

# Read the sheet
df_batch <- read_sheet(sheet_id, sheet = "batch")
#df_qualifyr <- read_sheet(sheet_id, sheet = "qualifyr")
df_gambit <- read_sheet(sheet_id, sheet = "gambit")
df_bactopia <- read_sheet(sheet_id, sheet = "bactopia-report")
df_assembly <- read_sheet(sheet_id, sheet = "assembly-scan")
df_checkm2 <- read_sheet(sheet_id, sheet = "checkm2")
df_mlst <- read_sheet(sheet_id, sheet = "mlst")
df_amrfinderplus <- read_sheet(sheet_id, sheet = "amrfinderplus")


batch_year <- c("24ARS", "25ARS")

#filter batch df
batch_df <- df_batch[grepl(paste(batch_year, collapse="|"), df_batch$sample_name), ]
sample_name <- batch_df$sample_name
sample_name <- gsub("-", "_", sample_name, fixed=TRUE)

#remove batch number from sample_name
sample_name_clean <- sub("(_[0-9]+.*)$", "", sample_name)


#merge qualifyr, gambit,checkm2 and bactopia results
#rename column
colnames(df_gambit)[which(names(df_gambit) == "24ARS_SampleSheet")] <- "name"
colnames(df_assembly)[which(names(df_assembly) == "sample")] <- "name"
colnames(df_bactopia)[which(names(df_bactopia) == "sample")] <- "name"


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
bactopia_sub_df <- subset(df_bactopia, select = c(name, qc_original_total_bp, qc_final_qual_mean))



# put all data frames into list
df_list <- list(gambit_sub_df, checkm2_sub_df, assembly_sub_df, bactopia_sub_df)

# merge all data frames in list
wgs_df <- df_list %>% reduce(full_join, by='name')


wgs_df$name <- gsub("-", "_", wgs_df$name, fixed=TRUE)
wgs_df$name <- toupper(wgs_df$name)




file_name <- paste0("data_files/24ARS_wgs_df.xlsx")
write_xlsx(wgs_df, file_name)


#get samplesheet file
get_samplesheet <- dlgInput("Enter sample sheet file name:", Sys.info()[" "])$res




# Apply the function to the column of strings
wgs_df <- wgs_df %>% rename(sample_name = name)


referred_df <- read_xlsx("data_files/Combined Data on Referred Isolates_07.01.25.xlsx", sheet="ARSRL")
referred_df <- subset(referred_df, select = c(accession_no,arsrl_org))
result <- referred_df[referred_df$accession_no %in% sample_name_clean, ]
colnames(result) <- c('sample_id','arsrl_org') 

# Returns string without leading or trailing white space
result$arsrl_org <- gsub("^\\s+|\\s+$", "", result$arsrl_org)

#check if arsrl_result_df is exisiting
if (!exists("arsrl_result_df")) {
  arsrl_result_df <- result
}




#remove batch name in sample_id
wgs_df <- wgs_df %>%
  mutate(
    sample_id = case_when(
      str_count(sample_name, "_") == 4 ~ sub("^([^_]+_[^_]+_[^_]+_[^_]+)_.*$", "\\1", sample_name),
      str_count(sample_name, "_") == 3 ~ sub("^([^_]+_[^_]+_[^_]+)_.*$", "\\1", sample_name),
      str_count(sample_name, "_") == 2 ~ sub("^(([^_]+_[^_]+)).*", "\\1", sample_name),
      TRUE ~ sample_name  # Default case (keep original)
    )
  )


wgs_df_subset <- subset(wgs_df, select = c(sample_id, sample_name))

arsrl_result_df <- merge(arsrl_result_df, wgs_df_subset, by = "sample_id")




#Check if QC_BBR sample is present in the id list
utp_sample <- grep("UTP", wgs_df[['sample_id']], value = TRUE)
utp_sample_count <- length(utp_sample)
wgs_utp <- wgs_df[wgs_df$sample_id %in% utp_sample, ]
wgs_utp <- subset(wgs_utp , select = c(sample_name, sample_id))

#manually add result for QC_BBR
if(utp_sample_count !=0){
  wgs_utp$arsrl_org = "Escherichia coli"
  
  #reorder by column index
  wgs_utp <- wgs_utp[, c(2,3,1)]
  
  arsrl_result_df <- rbind(arsrl_result_df,wgs_utp)
  
}




#Check if STC sample is present in the id list
stc_sample <- grep("STC", wgs_df[['sample_id']], value = TRUE)
stc_sample_count <- length(stc_sample)
wgs_stc <- wgs_df[wgs_df$sample_id %in% stc_sample, ]
wgs_stc <- subset(wgs_stc , select = c(sample_name, sample_id))



#manually add result for STC
if(stc_sample_count !=0){
  stc_df <- read_xlsx("data_files/ARSRL_SatScan_Results.xlsx")
  stc_df$sample_id <- gsub("STC", "STC_", stc_df$sample_id, fixed=TRUE)
  
  
  stc_df <- merge(wgs_stc, stc_df, by = "sample_id")
  
  #reorder by column index
  stc_df <- stc_df[, c(1,3,2)]
  

  arsrl_result_df <- rbind(arsrl_result_df,stc_df)
  
}






#Check if QC_BBR sample is present in the id list
bbr_sample <- grep("QC_BBR", wgs_df[['sample_id']], value = TRUE)
bbr_sample_count <- length(bbr_sample)
wgs_bbr <- wgs_df[wgs_df$sample_id %in% bbr_sample, ]
wgs_bbr <- subset(wgs_bbr , select = c(sample_name, sample_id))

#manually add result for QC_BBR
if(utp_sample_count !=0){
  wgs_bbr$arsrl_org = "Bordetella bronchiseptica"
  
  #reorder by column index
  wgs_bbr <- wgs_bbr[, c(2,3,1)]
  
  arsrl_result_df <- rbind(arsrl_result_df,wgs_bbr)
  
}

#Check if QC_BBR sample is present in the id list
vc_sample <- grep("VC", wgs_df[['sample_id']], value = TRUE)
vc_sample_count <- length(vc_sample)
wgs_vc <- wgs_df[wgs_df$sample_id %in% vc_sample, ]
wgs_vc <- subset(wgs_vc , select = c(sample_name, sample_id))

#manually add result for QC_BBR
if(vc_sample_count !=0){
  wgs_vc$arsrl_org = "Neisseria gonorrhoeae"
  
  #reorder by column index
  wgs_vc <- wgs_vc[, c(2,3,1)]
  
  arsrl_result_df <- rbind(arsrl_result_df,wgs_vc)

}



wgs_df1 <- wgs_df
#wgs_df <- wgs_df1



#missing <- setdiff(wgs_df$sample_id, arsrl_result_df$sample_id)




if(nrow(wgs_df) != 0){
  rmarkdown::render("wgs_qc_report_2024.Rmd",
                    output_file = paste("qualifyr_report_",batch_code, '.pdf', sep='')
  ) 
}else{
  cat("No file found")
}
