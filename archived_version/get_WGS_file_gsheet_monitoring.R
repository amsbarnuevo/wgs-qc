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
library(lubridate)

conflicts_prefer(openxlsx::write.xlsx)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::last)

setwd("E:/DMU Projects/wgs-qc")


# enter batch code
#batch_year <- dlgInput("Enter Year:", Sys.info()[" "])$res

# Google Sheet ID extracted from the URL (referred monitoring)
referred_monitoring_sheet_id <- "1ycRarF6Hi0jObgvKFF9Fe7YkuNll5Ya3inqMtJYK0Lg"

# Read the sheet
df_referred_monitoring_raw <- read_sheet(referred_monitoring_sheet_id, range = "all!A:G")
names(df_referred_monitoring_raw) <- tolower(names(df_referred_monitoring_raw))
df_referred_monitoring_raw$accession_no <- gsub("-", "_", df_referred_monitoring_raw$accession_no, fixed=TRUE)


# Filter rows where 'accession_no' contains the word "batch_year"
#df_referred_monitoring <- df_referred_monitoring_raw[grepl(batch_year, df_referred_monitoring_raw$accession_no), ]
#referred_ac <- df_referred_monitoring_raw$accession_no


df_referred_monitoring <-  df_referred_monitoring_raw[!duplicated(df_referred_monitoring_raw[ , "accession_no"]),]



# Google Sheet ID extracted from the URL (referred database)
referred_sheet_id <- "1JKtyRyLh0-ck2xCk0oIH4iidgs21996UKj69lvvlXAU"

# Read the sheet
df_referred_raw <- read_sheet(referred_sheet_id, sheet = 1)


# Filter rows where 'accession_no' contains the word "referred_ac"
#df_referred_isolates <- df_referred_raw[df_referred_raw$accession_no %in% referred_ac, ]

# Subset df_referred_raw
df_referred_isolates <- subset(df_referred_raw, select = c(accession_no, date_birth, age, referral_date, spec_date, date_admis))

#Change column name
names(df_referred_isolates)[names(df_referred_isolates) == 'referral_date'] <- 'date_referred'
names(df_referred_isolates)[names(df_referred_isolates) == 'spec_date'] <- 'specimen_date'


# Merge referred database and referred monitoring
df_referred <- merge(df_referred_isolates, df_referred_monitoring, by = c("accession_no", "date_referred", "specimen_date"), all=TRUE)




# Google Sheet ID extracted from the URL (monitoring)
monitoring_sheet_id <- "1LAY9ThmmLWmMo5gwpNPhveSX0JbFPOY8o1XUwpb-eRY"

# Read the sheet
df_monitoring <- read_sheet(monitoring_sheet_id, sheet = "all")


monitoring_colnames <- colnames(df_monitoring)



# Google Sheet ID extracted from the URL
sheet_id <- "1rOFp9VHXeAfghFU6aOOBeBSt9Q1RC5WVAdLez4HHLU4"

# Read the sheet
df_result <- read_sheet(sheet_id, sheet = "result")

# Subset df_result
df_result_subset <- subset(df_result, select = c(batch_code, sample_id, wgs_id, results))



#remove batch name in sample_id
df_result_subset <- df_result_subset %>%
  mutate(
    sample_id = case_when(
      str_count(sample_id, "_") == 4 ~ sub("^([^_]+_[^_]+_[^_]+_[^_]+)_.*$", "\\1", sample_id),
      str_count(sample_id, "_") == 3 ~ sub("^([^_]+_[^_]+_[^_]+)_.*$", "\\1", sample_id),
      str_count(sample_id, "_") == 2 ~ sub("^(([^_]+_[^_]+)).*", "\\1", sample_id),
      TRUE ~ sample_id  # Default case (keep original)
    )
  )



#Change column name
names(df_result_subset)[names(df_result_subset) == 'sample_id'] <- 'accession_no'




# Merge referred database and referred monitoring
df_result_sequenced <- merge(df_referred, df_result_subset, by = "accession_no", all=TRUE)

# Create WGS columnast
df_result_sequenced$wgs <- ifelse(is.na(df_result_sequenced$wgs_id), "","Yes")



# Remove rows where both col1 AND col2 are NA
df_result_sequenced_clean <- df_result_sequenced[!(is.na(df_result_sequenced$counter) & is.na(df_result_sequenced$wgs_id)), ]

#edit format
df_result_sequenced_clean$date_referred <- format(as.Date(df_result_sequenced_clean$date_referred), "%Y-%m-%d")
df_result_sequenced_clean$specimen_date <- format(as.Date(df_result_sequenced_clean$specimen_date), "%Y-%m-%d")
df_result_sequenced_clean$date_birth <- format(as.Date(df_result_sequenced_clean$date_birth), "%Y-%m-%d")
df_result_sequenced_clean$date_admis <- format(as.Date(df_result_sequenced_clean$date_admis), "%Y-%m-%d")



# append googlesheet "result" tab
#sheet_append(ss = monitoring_sheet_id, sheet = 1, data = df_result_sequenced_clean)







#result <- df_result_sequenced_clean %>%
#  group_by(accession_no) %>%
#  summarize(
#    batch_codes = paste(unique(batch_code), collapse = ", "),
#    n_batches = n_distinct(batch_code),  # Counts unique batches
#    wgs_id = last(wgs_id),       # Takes the last entry in the group
#    results = last(results)      # Takes the last entry in the group
#  ) %>%
#  ungroup()







result <- df_result_sequenced_clean %>%
  group_by(accession_no) %>%
  mutate(n_batches = n()) %>%  # Count total batches per accession_no
  summarize(
    batch_codes = paste(unique(batch_code), collapse = ", "),
    wgs_id = if (n_batches[1] > 1) {
      # For multi-batch: last "Passed" if exists, else last result (could be "Failed")
      if (any(results == "Passed")) {
        last(wgs_id[results == "Passed"])
      } else {
        last(wgs_id)
      }
    } else {
      # For single batch: keep original
      last(wgs_id)
    },
    results = if (n_batches[1] > 1) {
      # For multi-batch: "Passed" if exists, else last result
      if (any(results == "Passed")) {
        "Passed"
      } else {
        last(results)
      }
    } else {
      # For single batch: keep original
      last(results)
    }
  ) %>%
  ungroup()






df_result_sequenced_unique <-  df_result_sequenced_clean[!duplicated(df_result_sequenced_clean[ , "accession_no"]),]
df_result_sequenced_unique <- subset(df_result_sequenced_unique, select = c( accession_no, date_referred, specimen_date, date_birth, 
                                                                             age, date_admis ,laboratory, organism, ast, counter, wgs))

df_result_sequenced_final <- merge(df_result_sequenced_unique, result, by = "accession_no")

df_result_sequenced_final <- df_result_sequenced_final %>% 
  relocate(wgs, .after = results) %>%
  mutate(across(c(batch_codes), ~ ifelse(. == "NA", "", .)))


# append googlesheet "all" tab
#sheet_append(ss = monitoring_sheet_id, sheet = "all", data = df_result_sequenced_final)

# rewrite googlesheet "all" tab
sheet_write(ss = monitoring_sheet_id, sheet = "all", data = df_result_sequenced_final)



df_result_sequenced_kpn <- df_result_sequenced_final %>%
  filter(organism == "kpn")

# rewrite googlesheet "all" tab
sheet_write(ss = monitoring_sheet_id, sheet = "kpn", data = df_result_sequenced_kpn)






# Google Sheet ID extracted from the URL
emerging_sheet_id <- "1rPX88DQlgpzvzFhBkd9fO2d9NdLd-Q5gzufqwnvQdUE"

# Read the sheet
df_emerging_raw <- read_sheet(emerging_sheet_id, sheet = "list of all detected isolates", col_names = FALSE)

# Extract row 3 and properly flatten list-type names
new_headers <- df_emerging_raw[3, ] %>% 
  flatten() %>%                # Flatten any list columns
  as.character() %>%           # Convert to character
  # Optional: Clean up any remaining list tags
  gsub('list\\(|"|\\)', "", .) %>%
  gsub(" ", "_", .)

# Remove first 3 rows and set clean headers
df_emerging <- df_emerging_raw %>%
  slice(-c(1:3)) %>%
  set_names(new_headers) %>%
  rename(
    accession_no = AccessionNo,
    date_referred = Referral_Date,
    specimen_date = Spec_Date,
    laboratory = SiteCode,
    organism = OrganismCode
    ) %>%
  rename_with(tolower)

# Subset df_result
df_emerging_subset <- subset(df_emerging, select = c(accession_no, date_referred, specimen_date, date_admis, laboratory, organism))


df_result_sequenced_emerging <- merge(df_emerging_subset, result, by = "accession_no")

# replace NA and NULL values to Blank
df_result_sequenced_emerging <- df_result_sequenced_emerging %>% 
  mutate(across(c(batch_codes), ~ ifelse(. == "NA", "", .))) %>% 
  mutate(across(c(date_admis), ~ ifelse(. == "NULL", "", .)))


# Format date
df_result_sequenced_emerging <- df_result_sequenced_emerging %>%
  mutate(across(
    .cols = c(date_referred, specimen_date, date_admis),  # specify your columns
    .fns = ~ format(as.POSIXct(as.numeric(.), origin = "1970-01-01"), "%Y-%m-%d")
  ))



# rewrite googlesheet "emerging" tab
sheet_write(ss = monitoring_sheet_id, sheet = "emerging", data = df_result_sequenced_emerging)
