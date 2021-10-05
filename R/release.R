#' Release a version of the database to the OomyceteDB
#'
#' This is an interactive function that walks the user through the steps needed
#' to add a release of OomyceteDB to the `oomycetedbdata` package. The
#' `oomycetedbdata` package is the source of the data for the OomyceteDB
#' website, so this will effectively add a release to the website. This function
#' will do the following things: * Validate that the database is formatted
#' correctly, as described in [the database format guide](database_format.html).
#' * Show the version of the database to be released and any changes to the user
#' for a manual check. * Either upload the release to Github or store/modify a
#' version of the `oomycetedbdata` package on the local computer for manual
#' upload.
#'
#' @param overwrite (`TRUE`/`FALSE` of length 1) Whether or not to replace the
#'   previous release with this release rather than creating a new one. Default:
#'   `FALSE`.
#' @param description (`character` of length 1) A description of this release.
#'   This will be shown publicly with the release. Default: prompt user.
#' @param validate (`TRUE`/`FALSE` of length 1) Whether or not to validate the
#'   database source before attempting to create a release. It is highly
#'   recommended to validate before a release. Default: TRUE.
#' @inheritParams validate
#'
#' @export
release <- function(
  database_path = NULL,
  release_path = NULL,
  interactive = TRUE,
  validate = TRUE,
  auto_update = FALSE,
  overwrite = FALSE,
  description = NULL,
  google_user = NULL,
  google_pass = NULL,
  github_user = NULL,
  github_pass = NULL,
  config_path = find_config('oomycetedbtools_config.yml')
) {

  # Look for and apply configuration file by overwriting defaults of unspecified arguments
  #  NOTE: This function modifies variables in this environment instead of returning values
  apply_config_file(config_path)

  # Constants
  release_prefix <- "oodb_rps10_v"
  release_history_name <- "release_history.csv"

  # Validate parameter values
  release_path <- file.path(dirname(normalizePath(config_path)), release_path)
  release_history_path <- file.path(release_path, release_history_name)
  repo_path <- dirname(config_path) # Change in future
  repo <- git2r::repository(repo_path)

  # Check that release path git repo is prepared to accept a new release
  repo_staus = git2r::status(repo)
  if (length(repo_staus$staged) + length(repo_staus$unstaged) + length(repo_staus$untracked) > 0) {
    cli::cli_alert_danger("The target database repository has uncommited/untracked changes that must be commited or removed before a release can be added\n")
    print(repo_staus)
    return(invisible(FALSE))
  }
  cli::cli_alert_info("Pulling changes from Github to ensure the repository is up to date.")
  git2r::pull(repo)

  # Validate input database format
  if (validate) {
    format_is_valid <- validate(
      database_path = database_path,
      interactive = interactive,
      auto_update = auto_update,
      google_user = google_user,
      google_pass = google_pass,
      github_user = github_user,
      github_pass = github_pass,
      config_path = NULL
    )
    if (! format_is_valid) {
      return(invisible(FALSE))
    }
  }

  # Show differences between this and the last version to the user
  this_release <- format_release(database_path)
  last_release <- get_last_release(release_history_path)
  if (is.null(last_release)) {
    cli::cli_alert_info("This is the first release")
  } else {
    compare_releases(this_release, last_release)
    invisible(readline(prompt="The differences between this release and the last are being displayed. Press [enter] to continue when ready."))
  }

  # Ask user for description and spell check description
  if (is.null(description)) {
    repeat {
      cat('Enter a short description of the changes made for this release. This will be displayed publicly:\n\n')
      description <- readline('')

      # TODO: spell check

      if (nchar(description) == 0) {
        cat('No description provided. Try again or press "Esc" to cancel this release.\n')
      } else {
        break
      }
    }
  }

  # Update database with new release
  msg_text <- "Creating new release"
  cat(paste0("  ", msg_text, "\r"))
  release_data <- create_release(release_data = this_release,
                                 release_path = release_path,
                                 release_history_path = release_history_path,
                                 description = description)
  cli::cli_alert_success(msg_text)

  # Rebuild website
  msg_text <- "Rebuilding website"
  cat(paste0("  ", msg_text, "\r"))
  suppressWarnings(rmarkdown::render_site(input = repo_path, quiet = TRUE))
  cli::cli_alert_success(msg_text)

  # Commit and push changes to github
  msg_text <- "Pushing updates to Github"
  cat(paste0("  ", msg_text, "\r"))
  rel_num <- release_data$release_number[which.max(release_data$release_number)]
  git2r::add(repo, path = "*")
  git2r::commit(repo, message = paste0("Automated release ", max(release_data$release_number), "\n\n",
                                       release_data$description[which.max(release_data$release_number)]))
  git2r::push(repo)
  cli::cli_alert_success(msg_text)

  cli::cli_alert_success("\nCreation of new release successful.\n")
  return(invisible(TRUE))
}



format_release <- function(database_path) {
  # Download data from google sheets
  sequence_data <- googlesheets4::read_sheet(database_path, sheet = "sequence_data")
  taxon_data <- googlesheets4::read_sheet(database_path, sheet = "taxon_data")

  # Remove columns not meant for the public
  sequence_data <- sequence_data[, c("oodb_id", "name", "strain", "genbank_id", "taxon_id", "sequence")]
  taxon_data <- taxon_data[, c("taxon_id", "classification")]

  # Combine into a single table
  output <- dplyr::left_join(sequence_data, taxon_data, by = "taxon_id")

  # Reorder columns
  output[, c("oodb_id", "name", "strain", "genbank_id", "taxon_id",  "classification", "sequence")]
}


#' Get path to latest release
#'
#' Get the path to the current release of the database
#'
#' @param path A file path to either the release history CSV or a folder with a oomycetedbtools config file.
#'
#' @keywords internal
get_last_release <- function(release_history_path) {
  release_history <- get_release_data(release_history_path)
  if (is.null(release_history)) {
    return(NULL)
  } else {
    path <- release_history$csv_path[which.max(as.numeric(release_history$release_number))]
    path <- file.path(dirname(release_history_path), path)
    output <- suppressMessages(readr::read_csv(path))
    return(output)
  }
}

#' Get release metadata table
#'
#' Return the parsed table of data on the releases.
#'
#' @param path A file path to either the release history CSV or a folder with a oomycetedbtools config file.
#'
#' @export
get_release_data <- function(path = getwd()) {
  if (file.exists(path)) {
    # If the path is to a CSV, then parse that file
    if (grepl(path, pattern = "\\.csv$")) {
      release_hist_path <- path
    }
    # If the path is to a YAML, then assume it is the config file
    if (grepl(path, pattern = "\\.yml$")) {
      config_data <- yaml::read_yaml(path)
      release_hist_path <- file.path(config_data$release_path, "release_history.csv")
    }
    # If it is a folder, look for a config file
    if (file.info(path)$isdir) {
      config_data <- yaml::read_yaml(find_config('oomycetedbtools_config.yml', dir_path = path))
      release_hist_path <- file.path(config_data$release_path, "release_history.csv")
    }
    # Parse the CSV
    release_history <- suppressMessages(readr::read_csv(release_hist_path))
    if (nrow(release_history) > 0) {
      return(release_history)
    }
  }
  return(NULL)
}

compare_releases <- function(release_1, release_2) {
  temp_file_1 <- tempfile()
  temp_file_2 <- tempfile()
  readr::write_csv(dplyr::select(release_1, oodb_id, name, strain, genbank_id, taxon_id), file = temp_file_1)
  readr::write_csv(dplyr::select(release_2, oodb_id, name, strain, genbank_id, taxon_id), file = temp_file_2)
  # readr::write_csv(release_1, file = temp_file_1)
  # readr::write_csv(release_2, file = temp_file_2)
  old_max_print <- options("max.print")$max.print
  options(max.print=10000)
  print(suppressWarnings(diffobj::diffCsv(temp_file_1, temp_file_2,
                         tar.banner = "New release", cur.banner = "Last release",
                         pager="auto", mode = "sidebyside")))
  options(max.print=old_max_print)
}


create_release <- function(release_data, release_path, release_history_path, description) {
  # Update release history
  release_history <- get_release_data(release_history_path)
  if (is.null(release_history)) {
    release_history <- data.frame(release_number = numeric(0),
                                  release_date = character(0),
                                  csv_path = character(0),
                                  fasta_path = character(0),
                                  description = character(0),
                                  stringsAsFactors = FALSE)
  }
  if (nrow(release_history) == 0) {
    this_rel_num <- 1
  } else {
    this_rel_num <- max(release_history$release_number) + 1
  }
  this_rel_path_fasta <- file.path(release_path, paste0("oodb_rps10_v", this_rel_num, ".fasta"))
  this_rel_path_csv <- file.path(release_path, paste0("oodb_rps10_v", this_rel_num, ".csv"))
  this_rel_data <- list(release_number = this_rel_num,
                        release_date = Sys.Date(),
                        csv_path = basename(this_rel_path_csv),
                        fasta_path = basename(this_rel_path_fasta),
                        description = description)
  release_history <- rbind(release_history, as.data.frame(this_rel_data))

  # Add CSV release
  readr::write_csv(release_data, file = this_rel_path_csv)

  # Add FASTA release
  fasta_header_cols <- colnames(release_data)[colnames(release_data) != "sequence"]
  header <- vapply(1:nrow(release_data), FUN.VALUE = character(1), function(i) {
    paste0(fasta_header_cols, "=", release_data[i, fasta_header_cols], collapse = '|')
  })
  readr::write_lines(paste0('>', header, '\n', release_data$sequence), file = this_rel_path_fasta)

  # Update release history
  readr::write_csv(release_history, file = release_history_path)

  return(invisible(this_rel_data))
}
