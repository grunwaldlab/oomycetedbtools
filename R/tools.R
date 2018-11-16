get_blast_databases <- function(db_dir) {
  # Get possible blast database names
  db_file_names <- tools::file_path_sans_ext(list.files(db_dir))
  output <- unique(db_file_names)

  # Remove any that do not appear the correct number of times
  is_database <- vapply(output, function(x) as.list(table(db_file_names))[[x]] == 6, logical(1))
  output <- output[is_database]

  return(rev(output))
}


get_latest_release_fa <- function() {
  local_release_names <- list.files(local_release_dir, pattern = paste0(release_name_prefix, "[0-9]+\\.fa"))
  local_release_nums <- stringr::str_match(local_release_names, pattern = paste0(release_name_prefix, "([0-9]+)\\.fa"))[, 2]
  local_release_nums <- as.numeric(local_release_nums)
  file.path(local_release_dir, local_release_names[which.max(local_release_nums)])
}



#' Taken from https://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r
has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}



#' Read a FASTA file
#'
#' Reads a FASTA file. This is the FASTA parser for metacoder. It simply tries
#' to read a FASTA file into a named character vector with minimal fuss. It does
#' not do any checks for valid characters etc. Other FASTA parsers you might
#' want to consider include \code{\link[ape]{read.FASTA}} or
#' \code{\link[seqinr]{read.fasta}}.
#'
#' @param file_path (\code{character} of length 1) The path to a file to read.
#'
#' @return named \code{character} vector
#'
#' @examples
#'
#' # Get example FASTA file
#' fasta_path <- system.file(file.path("extdata", "silva_subset.fa"),
#'                           package = "metacoder")
#'
#' # Read fasta file
#' my_seqs <- read_fasta(fasta_path)
#'
#' @export
read_fasta <- function(file_path) {
  # Read raw string
  raw_data <- readr::read_file(file_path)

  # Return an empty vector an a warning if no sequences are found
  if (raw_data == "") {
    warning(paste0("No sequences found in the file: ", file_path))
    return(character(0))
  }

  # Find location of every header start
  split_data <- stringr::str_split(raw_data, pattern = "\n>", simplify = TRUE)

  # Split the data for each sequence into lines
  split_data <- stringr::str_split(split_data, pattern = "\n")

  # The first lines are headers, so remvove those
  headers <- vapply(split_data, FUN = `[`, FUN.VALUE = character(1), 1)
  split_data <- lapply(split_data, FUN = `[`, -1)

  # Remove the > from the first sequence. The others were removed by the split
  headers[1] <- sub(headers[1], pattern = "^>", replacement = "")

  # Combine multiple lines into single sequences
  seqs <- vapply(split_data, FUN = paste0, FUN.VALUE = character(1), collapse = "")

  # Combine and return results
  return(stats::setNames(seqs, headers))
}


