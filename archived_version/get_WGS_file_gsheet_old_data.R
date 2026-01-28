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
sheet_id <- "1NHu-RIgDeTPKgrnTTHS9qVbCe04tIeYX6k73Jm93gDw"

# Read the sheet
df_batch <- read_sheet(sheet_id, sheet = "batch")
df_gambit <- read_sheet(sheet_id, sheet = "gambit")
df_fastp <- read_sheet(sheet_id, sheet = "fastp_summary")
df_assembly <- read_sheet(sheet_id, sheet = "assembly-scan")
df_checkm2 <- read_sheet(sheet_id, sheet = "checkm2")
df_mlst <- read_sheet(sheet_id, sheet = "mlst")
df_amrfinderplus <- read_sheet(sheet_id, sheet = "amrfinderplus")


# List sample_name
sample_name <- df_batch$sample_name
sample_name <- gsub("-", "_", sample_name, fixed=TRUE)

#remove batch number from sample_name
sample_name_clean <- sub("(_[0-9]+.*)$", "", sample_name)


#merge qualifyr, gambit,checkm2 and fastp results
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
#bactopia_sub_df <- subset(df_bactopia, select = c(name, qc_original_total_bp, qc_final_qual_mean))



# put all data frames into list
df_list <- list(gambit_sub_df, checkm2_sub_df, assembly_sub_df, fastp_sub_df)

# merge all data frames in list
wgs_df <- df_list %>% reduce(full_join, by='name')


wgs_df$name <- gsub("-", "_", wgs_df$name, fixed=TRUE)
wgs_df$name <- toupper(wgs_df$name)


wgs_df1 <- wgs_df
#wgs_df <- wgs_df1


#rename species column to wgs_org
wgs_df <- wgs_df %>% 
  rename(wgs_id = species)

#complete wgs_org name
wgs_df$wgs_id <- ifelse(wgs_df$wgs_id == "Acinetobacter", "Acinetobacter baumannii", 
                        ifelse(wgs_df$wgs_id == "Escherichia", "Escherichia coli", wgs_df$wgs_id))

#load organism code reference file
org_df <- read_xlsx("data_files/ORG GROUPINGS_09042024.xlsx", "extended_list_complete")
org_df <- subset(org_df, select = c(WHONET_ORG_CODE, ORGANISM))

#rename ORGANISM_name column to wgs_id
org_df <- org_df %>% rename(wgs_id = ORGANISM)


wgs_df <- merge(wgs_df, org_df, by = "wgs_id", all.x = TRUE)

#rename ORG_ARS column to ORG
wgs_df <- wgs_df %>% rename(org_code = WHONET_ORG_CODE)


wgs_df$org_code_clean <- wgs_df$org_code



wgs_df$org_code_clean[grepl("Salmonella",wgs_df$wgs_id)] = "sal"
wgs_df$org_code_clean[grepl("Salmonella enterica",wgs_df$wgs_id)] = "sat"
wgs_df$org_code_clean[grepl("Shigella",wgs_df$wgs_id)] = "shi"
wgs_df$org_code_clean[grepl("Vibrio",wgs_df$wgs_id)] = "vic"
wgs_df$org_code_clean[grepl("Klebsiella",wgs_df$wgs_id)] = "kpn"
wgs_df$org_code_clean[grepl("Pseudomonas",wgs_df$wgs_id)] = "pae"
wgs_df$org_code_clean[grepl("Acinetobacter",wgs_df$wgs_id)] = "aba"
wgs_df$org_code_clean[grepl("Streptococcus",wgs_df$wgs_id)] = "spn"
wgs_df$org_code_clean[grepl("Streptococcus pseudopneumoniae",wgs_df$wgs_id)] = "spn"
wgs_df$org_code_clean[grepl("Streptococcus pyogenes",wgs_df$wgs_id)] = "spy"
wgs_df$org_code_clean[grepl("Streptococcus oralis",wgs_df$wgs_id)] = "sol"
wgs_df$org_code_clean[grepl("Burkholderia",wgs_df$wgs_id)] = "pce"
wgs_df$org_code_clean[grepl("Staphylococcus",wgs_df$wgs_id)] = "sau"
wgs_df$org_code_clean[grepl("Staphylococcus epidermidis",wgs_df$wgs_id)] = "sep"
wgs_df$org_code_clean[grepl("Raoultella planticola",wgs_df$wgs_id)] = "kpn"



#file_name <- paste0("data_files/historical_summary_wgs_df.xlsx")
#write_xlsx(wgs_df, file_name)

## Create reference data with ±2% tolerance built in
bactopia_genome_size <- data.frame(
  org_code_clean = c("aba", "pae","kpn", "eco","efa", "efm","sau", "spn","hin", "ngo","sal", "sat", "pce", "shi", "vic", "sma"),
  genome_size = c(4000000, 6300000, 5400000, 5000000, 3000000, 2900000, 2800000, 2200000, 1800000, 2200000, 
                  4900000, 4700000, 8200000, 4500000, 4000000, 8200000)
)

# compute total final coverage
wgs_df <- wgs_df %>%
  left_join(bactopia_genome_size, by = "org_code_clean") %>%
  mutate(
    final_coverage = (after_total_bases/genome_size)
  )



## Create reference data of to check genome size
size_ranges <- data.frame(
  org_code_clean = c("aba", "pae","kpn", "eco","efa", "efm","sau", "spn","hin", "ngo","sal", "sat", "pce", "shi","vic", "sma"),
  size_min = c(3400000, 5500000, 5000000, 4500000, 2800000, 2600000, 2700000, 2000000, 
               1700000, 2100000, 4500000, 4600000, 7600000, 4100000, 3900000, 4900000),  
  size_max = c(4200000, 7000000, 5700000, 5500000, 3200000, 3100000,3000000, 2300000, 
               1900000, 2300000, 5300000, 4800000, 8700000, 4800000, 4200000, 5100000) 
)

# Modify size ranges: subtract 10% from min, add 10% to max
size_ranges <- size_ranges %>%
  mutate(
    size_min = size_min * 0.9,
    size_max = size_max * 1.1
  )

## Check if GC is within ±2% of expected range
wgs_df <- wgs_df %>%
  left_join(size_ranges, by = "org_code_clean") %>%
  mutate(
    genome_size_result = case_when(
      total_contig_length >= size_min & total_contig_length <= size_max ~ "passed",
      total_contig_length < size_min ~ "failed",
      total_contig_length > size_max ~ "failed"
    )
  ) %>%
  select(org_code_clean, total_contig_length, genome_size_result, everything())




## Create reference data with ±2% tolerance built in
organism_gc_ranges <- data.frame(
  org_code_clean = c("aba", "pae","kpn", "eco","efa", "efm","sau", "spn","hin", "ngo","sal", "sat", "pce", "shi", "vic", "sma"),
  GC_min = c(37, 63, 55, 49, 35, 36, 31, 38, 36, 50, 50, 50, 64.7, 48.7, 45, 57.7),  # Original - 2%
  GC_max = c(41, 69, 59, 53, 39, 40, 35, 42, 40, 54, 54, 54, 68.7, 52.7, 49, 61.7)   # Original + 2%
)


## Check if GC is within ±2% of expected range
wgs_df <- wgs_df %>%
  left_join(organism_gc_ranges, by = "org_code_clean") %>%
  mutate(
    GC_result = case_when(
      gc_content >= GC_min & gc_content <= GC_max ~ "passed",
      gc_content < GC_min ~ "failed",
      gc_content > GC_max ~ "failed"
    ),
    Original_GC_range = case_when(
      org_code_clean == "pae" ~ "65-67%",
      org_code_clean %in% c("aba", "kpn",
                            "eco", "efa",
                            "efm", "sau",
                            "spn", "hin",
                            "ngo", "sal",
                            "sat", "pce",
                            "shi", "vic",
                            "sma") ~ paste0("≈", GC_min + 2, "%")
    )
  ) %>%
  select(org_code_clean, gc_content, Original_GC_range, GC_result, everything())




wgs_df <- wgs_df %>%
  rowwise() %>%
  mutate(
    condition_checks = list(c(
      "Completeness" = completeness >= 90,
      "Contamination" = contamination <= 5,
      "Depth of coverage" = final_coverage >= 20,
      "Genome size" = genome_size_result == "passed",
      "GC content" = GC_result == "passed",
      "Contig count" = total_contig < 500,
      "N50" = n50_contig_length > 20000,
      "Mean read Q-score" = combined_qual_mean >= 30
    )),
    
    # Set result to FALSE if any condition is FALSE or NA
    result = all(unlist(condition_checks), na.rm = FALSE),
    
    # Only list failed conditions (FALSE), remove NA
    failed_conditions = {
      cond_values <- unlist(condition_checks)
      failed <- names(cond_values[!is.na(cond_values) & cond_values == FALSE])
      if (length(failed) == 0) NA_character_ else paste(failed, collapse = ", ")
    }
  ) %>%
  ungroup()

#add row number
wgs_df$iso_num <- seq.int(nrow(wgs_df))


file_name <- paste0("data_files/historical_summary_wgs_df.xlsx")
write_xlsx(wgs_df, file_name)





retain_column <- c('name','failed_conditions','result','wgs_id','completeness','contamination','final_coverage','total_contig_length','gc_content','total_contig','n50_contig_length','combined_qual_mean')


wgs_summary_df <- subset(wgs_df, select = retain_column)


# create new wgs_summary_table with additional column for result

# Replace zeros with NA on data frame 
wgs_summary_df[wgs_summary_df == "  NA"] <- ""


wgs_summary_result <- wgs_summary_df %>%
  mutate(
    batch_code = sub(".*_(.*)", "\\1", name),
    batch_code = sub("(\\d{4})(\\d{2})(\\d{2})(.)", "\\1-\\2-\\3_\\4", batch_code),
    result = ifelse(is.na(result), "failed",
                    ifelse(result != TRUE, "Failed", "Passed"))
  ) %>%
  rename(sample_id = name) %>%
  relocate(batch_code, .before = sample_id) %>%
  arrange(batch_code)


# Create a conversion function to convert Basepairs to MEgabasepairs
bp_to_mb <- function(bp) {
  x <- bp / 1e6
  x <- format(round(x, 2), nsmall = 2)
  x <- paste0(x, " Mb")
  return(x)
}

#convert bp to mb
wgs_summary_result$total_contig_length <- ifelse(is.na(wgs_summary_result$total_contig_length), "", bp_to_mb(unlist(wgs_summary_result$total_contig_length)))

wgs_summary_result$combined_qual_mean <- ifelse(is.na(wgs_summary_result$combined_qual_mean), "", format(round(unlist(wgs_summary_result$combined_qual_mean), 1), nsmall = 1))
wgs_summary_result$final_coverage <- ifelse(is.na(wgs_summary_result$final_coverage), "", format(round(unlist(wgs_summary_result$final_coverage), 1), nsmall = 1))



# append googlesheet "result" tab
sheet_append(ss = sheet_id, sheet = "result_historical", data = wgs_summary_result)


file_name <- paste0("data_files/historical_wgs_df_result.xlsx")
write_xlsx(wgs_summary_result, file_name)














