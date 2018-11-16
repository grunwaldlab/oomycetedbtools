#' Update website
#'
#' Syncs the website with the information contained in the Google Drive folder.
#' This is run by a [cron job](https://en.wikipedia.org/wiki/Cron) on the web
#' server every day.
#'
#'  It does the following:
#'
#' * Syncs the local (on server) releases spreadsheet with the Google Drive
#' releases spreadsheet
#' * Looks for releases numbers that are in the spreadsheet, but have not yet
#' been downloaded
#' * For each new release, the FASTA file for the release (specified by the file name in the
#' spreasheet) is downloaded to the server and a new BLAST database is made for it
#'
#' The defaults for all the parameters are stored in the `config` list, defined in
#' "R/configuration.R". Typically, no parameters should need to be used.
#'
#' @param website_root The absolute path to the root folder of the website
#' @param googledrive_root_url The url to the Google Drive folder containing database resoureces.
#' @param release_spreadsheet_path The path (including the filename) to the version of the release
#'   spreadsheet on the server releative to the root of the website directory.
#' @param release_spreadsheet_name The name of the release spreadsheet on Google Drive with no
#'   extension.
#' @param local_release_dir The path to the directoy containing the archived FASTA files for all
#'   releases on the server relative to the root of the website.
#' @param release_name_prefix Text added to the begining of the archived FASTA files on the server.
#' @param blast_database_dir The path to the directoy containing the archived blast datbases for all
#'   releases on the server relative to the root of the website.
#' @param blast_path The path to the folder with blast executables
#'
#' @export
update_website <- function(website_root             = config$website_root,
                           googledrive_root_url     = config$googledrive_root_url,
                           release_spreadsheet_path = config$release_spreadsheet_path,
                           release_spreadsheet_name = config$release_spreadsheet_name,
                           local_release_dir        = config$local_release_dir,
                           release_name_prefix      = config$release_name_prefix,
                           blast_database_dir       = config$blast_database_dir,
                           blast_path               = config$blast_path) {

  # Turn off authentication for google drive
  # This is to avoid entering in a password manually
  googledrive::drive_auth_config(active = FALSE)

  # Update local spreadsheet using version on Google Drive
  update_releases_spreadsheet(website_root = website_root,
                              googledrive_root_url = googledrive_root_url,
                              release_spreadsheet_path = release_spreadsheet_path,
                              release_spreadsheet_name = release_spreadsheet_name)

  # Check for new releases
  release_data <- new_releases_data(website_root = website_root,
                                    googledrive_root_url = googledrive_root_url,
                                    release_spreadsheet_path = release_spreadsheet_path,
                                    local_release_dir = local_release_dir,
                                    release_name_prefix = release_name_prefix)


  # Process new releases
  message('Adding ', nrow(release_data), ' releases.')
  make_one_release <- function(index) {

    # Store a local copy of the database
    new_release_file_name <- paste0(release_name_prefix, release_data$release_number[index], ".fa")
    new_release_file_path <- file.path(website_root, local_release_dir, new_release_file_name)
    googledrive::drive_download(file = googledrive::as_id(release_data$drive_ids[index]),
                                path = new_release_file_path)

    # Create a blast database for it
    make_blast_database(fasta_path = new_release_file_path,
                        out_dir_path = file.path(website_root, blast_database_dir),
                        blast_path = file.path(website_root, blast_path))

  }

  not_used <- lapply(seq_len(nrow(release_data)), make_one_release)
}



#' Update release spreadsheet
#'
#' Replaces the local (on server) releases spreadsheet with the one on Google Drive.
#'
#' @inheritParams update_website
#'
#' @return `NULL`
#'
#' @export
update_releases_spreadsheet <- function(website_root             = config$website_root,
                                        googledrive_root_url     = config$googledrive_root_url,
                                        release_spreadsheet_path = config$release_spreadsheet_path,
                                        release_spreadsheet_name = config$release_spreadsheet_name) {

  # Get list of files in Google Drive database folder
  drive_files <- googledrive::drive_ls(path = googledrive::as_id(googledrive_root_url))

  # Get file id from name
  if (! release_spreadsheet_name %in% drive_files$name) {
    stop('Cant find release spreadsheet named "', release_spreadsheet_name,
         '". Check that the file exists in Google Drive or ',
         'change the name of the file to look for in "configuration.R".')
  }
  releases_spreadsheet_id <- drive_files$id[drive_files$name == release_spreadsheet_name]
  release_spreadsheet_obj <- googlesheets::gs_key(releases_spreadsheet_id)

  # Download and overwrite local version of the spreadsheet
  googlesheets::gs_download(from = release_spreadsheet_obj,
                            to = file.path(website_root, release_spreadsheet_path),
                            overwrite = TRUE)
}



#' Get data on new releases
#'
#' Get rows of the releases spreadsheet corresponding to new releases
#'
#' @inheritParams update_website
#'
#' @return tibble
#'
#' @export
new_releases_data <- function(website_root             = config$website_root,
                              googledrive_root_url     = config$googledrive_root_url,
                              release_spreadsheet_path = config$release_spreadsheet_path,
                              local_release_dir        = config$local_release_dir,
                              release_name_prefix      = config$release_name_prefix) {

  # Get list of files in Google Drive database folder
  drive_files <- googledrive::drive_ls(path = googledrive::as_id(googledrive_root_url))

  # Get the number codes for releases with FASTA files already on the server
  local_release_names <- list.files(file.path(website_root, local_release_dir),
                                    pattern = paste0(release_name_prefix, "[0-9]+\\.fa"))
  local_release_nums <- stringr::str_match(local_release_names,
                                           pattern = paste0(release_name_prefix, "([0-9]+)\\.fa"))[, 2]

  # Get the number code for releases on the releases spreadsheet
  release_data <- get_release_data(file.path(website_root, release_spreadsheet_path))
  remote_release_nums <- release_data$release_number
  new_release_indexes <- which(! remote_release_nums %in% local_release_nums)

  # Get the names of the FASTA file for each release
  new_releases <- release_data[new_release_indexes, ]

  # Check that fasta files exist on Google Drive
  invalid_releases <- new_releases$source_file[! new_releases$source_file %in% drive_files$name]
  if (length(invalid_releases) > 1) {
    stop('Cannot find the following FASTA files needed to processes new release of the database:\n',
         paste0("   ", invalid_releases, collapse = "\n" ))
  }

  # Add google drive ids of FASTA files for new releases
  new_releases$drive_ids <- vapply(new_releases$source_file,
                            function(n) drive_files$id[drive_files$name == n],
                            character(1))

  return(new_releases)
}


#' Make a BLAST database
#'
#' Make a BLAST database in the specificed location from a FASTA file.
#' The name of the BLAST database is made using the name of the input file.
#'
#' @param fasta_path The path to the FASTA file to use to make the database from.
#' @param out_dir_path The path to the directory to store the BLAST database in.
#' @inheritParams update_website
#'
#' @return `NULL`
#'
#' @export
make_blast_database <- function(fasta_path, out_dir_path, blast_path = config$blast_path) {

  # Make temporary version of the database with indexes instead of full headers
  #   This is needed because BLAST will not return header info after the first space..
  ref_seqs <- metacoder::read_fasta(fasta_path)
  names(ref_seqs) <- as.character(seq_along(ref_seqs))
  temp_database_path <- tempfile()
  readr::write_lines(paste0(">", names(ref_seqs), "\n", ref_seqs), temp_database_path)

  # Make blast database with temporary file
  blast_db_name <- tools::file_path_sans_ext(basename(fasta_path))
  blast_db_path <- file.path(out_dir_path, blast_db_name)
  makeblastdb_command <- paste(file.path(blast_path, "makeblastdb"),
                               "-in", temp_database_path,
                               "-parse_seqids",
                               "-dbtype nucl",
                               "-out", blast_db_path)
  system(makeblastdb_command)
}



#' Get releases spreadsheet
#'
#' Returns the releases spread sheet stored on the server.
#'
#' @inheritParams update_website
#'
#' @return tibble
#'
#' @export
get_release_data <- function(release_spreadsheet_path = config$release_spreadsheet_path) {
  readr::read_csv(release_spreadsheet_path)
}

