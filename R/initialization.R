#' Make first version of the datbase
#'
#' Make first version of the database using the spreadsheet with sequences made by Val Feiland.
#' This was only use once to initialize the database.
#' Since then, the FASTA file has been edited directly
#'
#' @param output_path Where to save the FASTA file output
#' @param source_db The URL to the google drive folder for oomycetedb
#'
#' @export
init_database <- function(output_path = "initial_release.fa", source_db = "https://docs.google.com/spreadsheets/d/1Waua9iUPWLRqkDE0KkgAANFVEXas-8I6nCgSF8bG_Mw") {

  # Turn off authentication for google drive
  # This is to avoid entering in a password manually
  googledrive::drive_auth_config(active = FALSE)

  # Download the spreadsheet with database information
  ss_obj <- googlesheets::gs_key(googledrive::as_id(source_db))
  db_file_path <- tempfile(fileext = ".csv")
  googlesheets::gs_download(from = ss_obj,
                            to = db_file_path,
                            ws = 2,
                            overwrite = TRUE)
  raw_data <- readr::read_csv(db_file_path)

  # Get taxonomy from NCBI
  raw_data$Organism <- ifelse(is.na(raw_data$Organism), "", raw_data$Organism)
  genus_data <- taxa::lookup_tax_data(raw_data$Taxonomy, type = "taxon_name")
  sp_data <- taxa::lookup_tax_data(paste(raw_data$Taxonomy, raw_data$Organism),
                                   type = "taxon_name")
  gn_taxonomy <- taxa::classifications(genus_data)[names(genus_data$data$query_data)]
  sp_taxonomy <- taxa::classifications(sp_data)[names(sp_data$data$query_data)]
  sp_taxonomy[sp_taxonomy == "unknown taxon"] <- paste0(gn_taxonomy[sp_taxonomy == "unknown taxon"],
                                                        ";",
                                                        raw_data$Organism[sp_taxonomy == "unknown taxon"])
  raw_data$classification <- sp_taxonomy

  # Make database headers
  headers <- paste0("id=", seq_len(nrow(raw_data)), "|",
                    "name=", raw_data$Name, "|",
                    "source=", raw_data$Description, "|",
                    "tax_id=", names(sp_data$data$query_data), "|",
                    "taxonomy=", raw_data$classification)
  headers <- gsub(headers, pattern = " ", replacement = "_")

  # Make FASTA file
  readr::write_lines(paste0(">", headers, "\n", raw_data$Sequence),
                     path = output_path)
}
