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
  release_data <- create_release(release_data = this_release,
                                 release_path = release_path,
                                 release_history_path = release_history_path,
                                 description = description)

  # Rebuild website
  rmarkdown::render_site(input = repo_path)

  # Commit and push changes to github
  git2r::add(repo, path = "*")
  git2r::commit(repo, message = paste0("Automated release ", release_data$release_number, "\n\n", release_data$description))
  git2r::push(repo)

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



get_last_release <- function(release_history_path) {
  release_history <- get_release_history(release_history_path)
  if (is.null(release_history)) {
    return(NULL)
  } else {
    path <- release_history$file_name[which.max(as.numeric(release_history$number))]
    return(readr::read_csv(path))
  }
}


get_release_history <- function(release_history_path) {
  if (file.exists(release_history_path)) {
    release_history <- readr::read_csv(release_history_path)
    if (nrow(release_history) > 0) {
      return(release_history)
    }
  }
  return(NULL)
}

compare_releases <- function(release_1, release_2) {
  diffobj::diffCsv(release_1, release_2, pager="auto")
}


create_release <- function(release_data, release_path, release_history_path, description) {
  # Update release history
  release_history <- get_release_history(release_history_path)
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

  return(invisible(this_rel_data))
}
