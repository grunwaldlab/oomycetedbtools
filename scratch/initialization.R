# Make first version of the database
#
# Make first version of the database using the FASTA file with sequences made by Val Feiland.
# This was only use once to initialize the database.
# Since then, the spreadsheet file has been edited directly.

library(metacoder)
library(purrr)
library(dplyr)
library(stringr)
library(readODS)
library(taxize)

fasta_path = file.path("scratch", "2021-06-10_release_03_rps10.fasta")
output_path = file.path("scratch", "oomycetedb_source.ods")

# Make sequence data table
seqs <- read_fasta(fasta_path)
seq_data <- map_dfr(names(seqs), function(header) {
  header_data <- strsplit(header, split = "|", fixed = TRUE)[[1]] %>%
    str_match(pattern = "^(.+)=(.*)$")
  as_tibble(as.list(set_names(header_data[, 3], header_data[, 2])))
})
seq_data$sequence <- unname(seqs)
seq_data$oodb_id <- as.numeric(seq_data$oodb_id)
seq_data <- arrange(seq_data, oodb_id)
seq_data$strain <- ifelse(seq_data$strain == "na", "", seq_data$strain)
seq_data$ncbi_acc <- ifelse(seq_data$ncbi_acc == "TBD", "", seq_data$ncbi_acc)
seq_data$ncbi_taxid <- ifelse(seq_data$ncbi_taxid %in% c("", "NA"), "", seq_data$ncbi_taxid)
seq_data$ncbi_taxid <- gsub(seq_data$ncbi_taxid, pattern = "[()]+", replacement = "")
seq_data$oodb_id <- paste0("oodb_", 1:nrow(seq_data)) # Make new IDs, since existing ones are not unique
seq_data <- transmute(
  seq_data,
  oodb_id,
  name,
  strain,
  genbank_id = ncbi_acc,
  taxon_id = ncbi_taxid,
  public = TRUE,
  auto_update = TRUE,
  source = "",
  notes = "",
  sequence
)

# Make taxon data table
tax_data <- tibble(
  taxon_id = unique(seq_data$taxon_id[seq_data$taxon_id != ""]),
  auto_update = TRUE,
  notes = ""
)
class_data <- taxize::classification(tax_data$taxon_id, db = "ncbi")
tax_data$classification <- map_chr(class_data, function(x) {
  paste(x$name, collapse = ";")
})

# Combine into single spreadsheet
write_ods(seq_data, path = output_path, sheet = "sequence_data")
write_ods(tax_data, path = output_path, sheet = "taxon_data", append = TRUE)

