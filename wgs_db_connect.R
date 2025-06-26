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
library(data.table)


conflicts_prefer(dplyr::filter)

setwd("E:/DMU Projects/wgs-qc")


#Get the WGS QC File
get_batchname <- dlgInput("Enter batch number:", Sys.info()[" "])$res
get_samplesheet <- dlgInput("Enter sample sheet file name:", Sys.info()[" "])$res



#open connections
con_bactopia <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "bactopia",
  host     = "10.10.103.60",
  port     = 5432,
  user     = "arsp1",
  password = "@rsp1111"
)

con_ghru2 <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "ghru2",
  host     = "10.10.103.60",
  port     = 5432,
  user     = "arsp1",
  password = "@rsp1111"
)


get_tbl <- function(con, tbl) {
  dbGetQuery(con, sprintf('SELECT * FROM "%s";', tbl))
}

#load df
df_bactopia      <- get_tbl(con_bactopia,    "bactopia-report")
df_checkm2       <- get_tbl(con_bactopia,    "checkm2-report")
df_mlst          <- get_tbl(con_bactopia,    "mlst-report")
df_amrfinderplus <- get_tbl(con_bactopia,    "amrfinderplus-report")
df_speciation    <- get_tbl(con_ghru2,       "ghru2-speciation")


dbDisconnect(con_bactopia)
dbDisconnect(con_ghru2)




#merge checkm2 and bactopia results
#rename column
colnames(df_bactopia)[which(names(df_bactopia) == "sample")] <- "name"

#remove ".fna"
df_checkm2$name <- gsub(".fna", "", df_checkm2$name, fixed=TRUE)

#merge df
quality_report <- merge(df_checkm2, df_bactopia, by = "name")


wgs_df <- subset(quality_report, select = c(name, species, completeness, contamination, rank, qc_final_coverage,
                            assembler_total_contig_length, gc_content , assembler_total_contig,
                            assembler_n50_contig_length, qc_final_qual_mean))


wgs_df$name <- gsub("-", "_", wgs_df$name, fixed=TRUE)
wgs_df$name <- toupper(wgs_df$name)



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





# taking input with showing the message
#get_file <- dlgInput("Enter a text filename", Sys.info()[" "])$res
id_list <- na.omit(wgs_df[['sample_id']])
id_list <- toupper(id_list)
id_list <- gsub("\\_", "_", id_list)


referred_df <- read_xlsx("data_files/Combined Data on Referred Isolates_05.09.25.xlsx", sheet="ARSRL")
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


#wgs_df1 <- wgs_df
wgs_df <- wgs_df1



#wgs_df <- read_xlsx("wgs_df_2024-05-03.xlsx")

if(nrow(wgs_df) != 0){
  rmarkdown::render("wgs_qc_report_ver4.Rmd",
                    output_file = paste("qualifyr_report_",get_batchname, '.pdf', sep='')
  ) 
}else{
  cat("No file found")
}


