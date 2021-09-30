#' Validate the source database format
#'
#' Check that the database spreadsheet does not have any issues that would cause
#' problems when hosting the website.
#'
#' @param database_path (`character` of length 1) The file path or URL to the
#'   spreadsheet with the sequence and taxonomy data, formatted as described in
#'   [the database format guide](database_format.html). Default: `NULL`, prompt
#'   user if needed.
#' @param release_path (`character` of length 1) The file path or URL to the
#'   git/github repository the releases are stored in. Default: `NULL`, prompt
#'   user if needed.
#' @param interactive (`TRUE`/`FALSE` of length 1) Whether or not to prompt the
#'   user. This function is designed to be interactive, so `TRUE` is
#'   recommended. Default: `TRUE`.
#' @param auto_update (`TRUE`/`FALSE` of length 1) Whether or not to allow
#'   automated edits to the source database specified by `database_path`. Before
#'   any edits are made, a backup of the file will be made in the same location
#'   prefixed with ".backup_". Default: `FALSE`.
#' @param verbose  (`TRUE`/`FALSE` of length 1) Print extra information.
#' @param google_user (`character` of length 1) A Google Drive user ID with
#'   read/write permissions for the database folder. Required if `database_path`
#'   is a URL to a Google spreadsheet. Default: `NULL`, prompt user if needed.
#' @param google_pass (`character` of length 1) The password associated with the
#'   `google_user` option.  Required if `database_path` is a URL to a Google
#'   spreadsheet. Default: `NULL`, prompt user if needed.
#' @param github_user (`character` of length 1) A Github user ID with read/write
#'   permissions for the `oomycetedbdata` repository. Required if `release_path`
#'   is a URL to a Github repository. Default: `NULL`, prompt user if needed.
#' @param github_pass (`character` of length 1) The password associated with the
#'   `github_user` option. Required if `release_path` is a URL to a Github
#'   repository. Default: `NULL`, prompt user if needed.
#' @param config_path (`character` of length 1) The file path to a configuration
#'   file with defaults for these parameters.
#'
#' @export
validate <- function(
  database_path = NULL,
  release_path = NULL,
  interactive = TRUE,
  auto_update = FALSE,
  verbose = TRUE,
  google_user = NULL,
  google_pass = NULL,
  github_user = NULL,
  github_pass = NULL,
  config_path = find_config('oomycetedbtools_config.yml')
) {

  # Look for and apply configuration file by overwriting defaults of unspecified arguments
  #  NOTE: This function modifies variables in this environment as well as returning values
  arguments <- apply_config_file(config_path)

  # Define list of checks to be made
  check_functions <- list(
    'Checking if database spreadsheet can be accessed' = check_database_source_present, # Is the file present at the specified path?
    'Checking database spreadsheet format' = check_database_format#, # Correct columns and sheet names? Merged cells? Tabular?
    # 'Checking sequence IDs' = check_sequence_ids, # Unique? Add new ones if needed
    # 'Checking for invalid characters in sequences' = check_sequence, # Only valid characters? Any changes since last release?
    # 'Checking genbank IDs' = check_genbank_ids, # Are Genbank accession numbers valid if present?
    # 'Checking taxon IDs' = check_taxon_ids, # Are taxon IDs valid? Are they present in the `taxon_data` sheet?
  )

  # Define function to run each check, repeating as needed
  run_checks <- function(check_list) {
    check_index <- 1
    check_result <- rep("not checked", length(check_list))
    while (check_index <= length(check_list)) {
      check_result[check_index] <- run_check(
        check_func = check_list[[check_index]],
        check_desc = names(check_list)[check_index],
        database_path = database_path,
        release_path = release_path,
        auto_update = auto_update,
        verbose = verbose
      )
      if (check_result[check_index] == "retry") {
        next
      } else if (check_result[check_index] == "failed") {
        break
      }
      check_index <- check_index + 1
    }
    return(check_result)
  }

  # Run checks until all pass after applying any fixes
  check_result <- run_checks(check_functions)
  while (any(check_result %in% c("fixed"))) {
    check_result <- run_checks(check_functions)
  }

  # Report overall result
  return(all(check_result == "passed"))
}


#' Run a check function
#'
#' Run a check function with a progress indicator and handle output for failed checks.
#'
#' @param check_func Function with code for check
#' @param check_desc The description of the check shown to the user while it runs
#' @param ... Passed to check `check_func`
#'
#' @keywords internal
run_check <- function(check_func, check_desc, ...) {
  cli::cat_rule(check_desc, line_col = "#CCCCCC")
  result <- check_func(...)
  return(result)
}

#' Print an error message
#'
#' Print a simplified description of an error message.
#'
#' @param error_msg The message to print.
#' @param error_key If provided, this is used to lookup what to print based on the `error_msg`.
#'
#' @keywords internal
print_error_report <- function(error_msg, error_key = NULL) {
  if (is.null(error_key)) {
    cat(error_msg)
  } else {
    matches_key <- vapply(seq_along(error_key), FUN.VALUE = logical(1), function(i) {
      grepl(error_msg, pattern = names(error_key)[i])
    })
    if (sum(matches_key) > 1) {
      stop('Multiple errors match')
    } else if (sum(matches_key) == 0) {
      cat(error_msg)
    } else {
      cat(error_key[matches_key])
    }
  }
  return(invisible(NULL))
}


#' Check that database is present
#'
#' Checks that the database file exists and is readable
#'
#' @inheritParams validate
#'
#' @keywords internal
check_database_source_present <- function(database_path = NULL, release_path = NULL,
                                          interactive = TRUE, auto_update = FALSE, ...) {
  status <- "passed"

  # Error description table
  error_key <- c("Client error: \\(404\\) NOT_FOUND" = paste0("Could not find anything at the following URL:\n  ", database_path, "\n"),
                 "Could not resolve host" = "Could not contact Google Sheets. Check internet connection.\n",
                 "Client error: \\(403\\) PERMISSION_DENIED" = paste0("You dont have permission to interact with the spreadsheet at the following URL:\n  ", database_path, "\nWhen prompted to give permission to the Tidyverse API, make sure to allow read/write access.\n"))

  # Can read a table at URL
  msg_text <- "Checking if table can be read from URL"
  cat(paste0("  ", msg_text, "\r"))
  error_msg <- NULL
  result <- tryCatch(sheet <- googlesheets4::read_sheet(database_path),
                     error = function(e) error_msg <<- e$message)

  # Report result
  if (tibble::is_tibble(result) && is.null(error_msg)) {
    cli::cli_alert_success(msg_text)
  } else {
    cli::cli_alert_danger(msg_text)
    print_error_report(error_msg, error_key)
    status <- "failed"
  }
  return(status)
}



#' Check database format
#'
#' Checks that the sheets and column of the spreadsheet file are in the correct
#' format for creating a release.
#'
#' @inheritParams validate
#'
#' @keywords internal
check_database_format <- function(database_path = NULL, release_path = NULL,
                                  interactive = TRUE, auto_update = FALSE, ...) {

  # Check that the correct sheets are present
  msg_text <- "Checking if table can be read from URL"
  cat(paste0("  ", msg_text, "\r"))
}
