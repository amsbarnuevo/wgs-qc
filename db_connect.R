library(DBI)


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
