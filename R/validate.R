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
#' @param stop_on_fail  (`TRUE`/`FALSE` of length 1) Whether or not to stop the
#'   validation on the fist failed check. Even when `FALSE`, some checks will
#'   stop the validation when they fail. Default: `FALSE`.
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
  stop_on_fail = FALSE,
  verbose = TRUE,
  google_user = NULL,
  google_pass = NULL,
  github_user = NULL,
  github_pass = NULL,
  config_path = find_config('oomycetedbtools_config.yml')
) {

  # Status types:
  #  passed: test passed without any changes
  #  fixed: test passed after user or automated changes
  #  retry: the test should be run again
  #  failed: the test failed, but other tests should continue to be run
  #  stop: the test failed and no others should be attempted

  # Look for and apply configuration file by overwriting defaults of unspecified arguments
  #  NOTE: This function modifies variables in this environment as well as returning values
  arguments <- apply_config_file(config_path)

  # Define list of checks to be made
  check_functions <- list(
    'Checking if database spreadsheet can be accessed' = check_database_source_present, # Is the file present at the specified path?
    'Checking database spreadsheet format' = check_database_format, # Correct columns and sheet names? Merged cells? Tabular?
    'Checking sequence IDs' = check_sequence_ids, # Unique? Add new ones if needed
    'Checking for invalid characters in sequences' = check_sequence, # Only valid characters? Any changes since last release?
    'Checking genbank IDs' = check_genbank_ids, # Are Genbank accession numbers valid if present?
    'Checking taxon IDs' = check_taxon_ids # Are taxon IDs valid? Are they present in the `taxon_data` sheet?
  )

  # Define function to run each check, repeating as needed
  run_checks <- function(check_list) {
    check_index <- 1
    check_result <- rep("not checked", length(check_list))
    while (check_index <= length(check_list)) {
      check_status <- run_check(
        check_func = check_list[[check_index]],
        check_desc = names(check_list)[check_index],
        database_path = database_path,
        release_path = release_path,
        auto_update = auto_update,
        verbose = verbose
      )
      if (stop_on_fail && check_status == "failed") {
        check_status <- "stop"
      }
      if (check_status == "stop") {
        check_result[check_index] <- check_status
        break
      }
      if (check_status == "retry") {
        next
      }
      if (check_status == "passed" && check_result[check_index] == "retry") {
        check_result[check_index] <- "fixed"
      }
      check_result[check_index] <- check_status
      check_index <- check_index + 1
    }
    return(check_result)
  }
  check_result <- run_checks(check_functions)

  # Report any failures
  test_failed <- check_result %in% c("failed", "stop")
  if (any(test_failed)) {
    print_error_report(paste0("\n", sum(test_failed), " checks failed. Validation unsuccessful.\n"))
    return(invisible(FALSE))
  }

  # Run checks until all pass after applying any fixes
  while (any(check_result %in% c("fixed"))) {
    cat(paste0("\n", sum(test_failed), " checks were fixed. Restarting validation to ensure that fixes did not cause other problems.\n"))
    check_result <- run_checks(check_functions)
  }

  # Report overall result
  cat("\nAll checks passed. Validation successful.\n")
  return(invisible(TRUE))
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
                 "Client error: \\(403\\) PERMISSION_DENIED" = paste0("You dont have permission to interact with the spreadsheet at the following URL:\n  ",
                                                                      database_path,
                                                                      "\nWhen prompted to give permission to the Tidyverse API, make sure to click the option to allow edit access.\n"))

  # Can read a table at URL
  msg_text <- "Checking if table can be read from URL"
  cat(paste0("  ", msg_text, "\r"))
  error_msg <- NULL
  result <- tryCatch(sheet <- googlesheets4::gs4_get(database_path),
                     error = function(e) error_msg <<- e$message)

  # Report result
  if (is.null(error_msg)) {
    cli::cli_alert_success(msg_text)
  } else {
    cli::cli_alert_danger(msg_text)
    print_error_report(error_msg, error_key)
    status <- "stop"
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
  status = "passed"

  # Check that the correct sheets are present
  msg_text <- "Checking for needed sheets in spreadsheet file"
  cat(paste0("  ", msg_text, "\r"))
  ss_data <- googlesheets4::gs4_get(database_path)
  if (all(c("sequence_data", "taxon_data") %in% ss_data$sheets$name)) {
    cli::cli_alert_success(msg_text)
  } else {
    cli::cli_alert_danger(msg_text)
    print_error_report("The spreadsheet file must contain sheets titled 'sequence_data' and 'taxon_data'.")
    return("stop")
  }

  # Check for needed columns
  required_seq_cols <- c("oodb_id", "name", "strain", "genbank_id", "taxon_id",
                         "public", "auto_update", "source", "notes", "sequence")
  required_tax_cols <- c("taxon_id", "auto_update", "notes", "classification")
  check_cols <- function(sheet_name, required_cols) {
    msg_text <- paste0("Checking column names of '", sheet_name, "'")
    cat(paste0("  ", msg_text, "\r"))
    table <- suppressMessages(googlesheets4::read_sheet(database_path, sheet = sheet_name))
    missing_cols <- colnames(table)[! colnames(table) %in% required_cols]
    if (length(missing_cols) == 0) {
      cli::cli_alert_success(msg_text)
      return(table)
    } else {
      cli::cli_alert_danger(msg_text)
      print_error_report(paste0("The '", sheet_name, "' sheet is missing the following required columns:\n  ",
                                paste0(missing_cols, collapse = ", "),
                                "\nCheck that columns have not been renamed, including changes in letter case."))
      return(NULL)
    }
  }
  sequence_data <- check_cols("sequence_data", required_seq_cols)
  taxon_data <- check_cols("taxon_data", required_tax_cols)
  if (is.null(sequence_data) || is.null(taxon_data)) {
    return("stop")
  }

  # Check for ability to sequence data parse columns
  msg_text <- "Checking column formatting of 'sequence_data'"
  cat(paste0("  ", msg_text, "\r"))
  is_valid <- grepl(sequence_data$oodb_id, pattern = "^oodb_[0-9]+$") | is.na(sequence_data$oodb_id)
  if (any(! is_valid)) {
    cli::cli_alert_danger(msg_text)
    print_error_report(paste0("The 'oodb_id' column contains the following invalid IDs:\n  ",
                              paste0(sequence_data$oodb_id[! is_valid], ' (row ', which(! is_valid), ')', collapse = ", "),
                              "\nIDs must start with 'oodb_' or be left blank."))
    return("failed")
  }
  is_valid <- sequence_data$public %in% c("TRUE", "FALSE") | is.na(sequence_data$public)
  if (any(! is_valid)) {
    cli::cli_alert_danger(msg_text)
    print_error_report(paste0("The 'public' column contains the following invalid values:\n  ",
                              paste0(sequence_data$public[! is_valid], ' (row ', which(! is_valid), ')', collapse = ", "),
                              "\nValues must be 'TRUE', 'FALSE', or left blank."))
    return("failed")
  }
  is_valid <- sequence_data$auto_update %in% c("TRUE", "FALSE") | is.na(sequence_data$auto_update)
  if (any(! is_valid)) {
    cli::cli_alert_danger(msg_text)
    print_error_report(paste0("The 'auto_update' column contains the following invalid values:\n  ",
                              paste0(sequence_data$auto_update[! is_valid], ' (row ', which(! is_valid), ')', collapse = ", "),
                              "\nValues must be 'TRUE', 'FALSE', or left blank."))
    return("failed")
  }
  cli::cli_alert_success(msg_text)

  # Check for ability to taxon data parse columns
  msg_text <- "Checking column formatting of 'taxon_data'"
  cat(paste0("  ", msg_text, "\r"))

  is_valid <- grepl(taxon_data$taxon_id, pattern = "^oodb_tax_") |  grepl(taxon_data$taxon_id, pattern = "[0-9]+")
  if (any(! is_valid)) {
    cli::cli_alert_danger(msg_text)
    print_error_report(paste0("The 'taxon_id' column contains the following invalid IDs:\n  ",
                              paste0(taxon_data$taxon_id[! is_valid], ' (row ', which(! is_valid), ')', collapse = ", "),
                              "\nIDs must start with 'oodb_' or be left blank."))
    return("failed")
  }

  is_valid <- taxon_data$auto_update %in% c("TRUE", "FALSE") | is.na(taxon_data$auto_update)
  if (any(! is_valid)) {
    cli::cli_alert_danger(msg_text)
    print_error_report(paste0("The 'auto_update' column contains the following invalid values:\n  ",
                              paste0(taxon_data$auto_update[! is_valid], ' (row ', which(! is_valid), ')', collapse = ", "),
                              "\nValues must be 'TRUE', 'FALSE', or left blank."))
    return("failed")
  }

  cli::cli_alert_success(msg_text)

  return(status)
}


#' Check sequence IDs
#'
#' Checks that sequence IDs are unique in all releases
#'
#' @inheritParams validate
#'
#' @keywords internal
check_sequence_ids <- function(database_path = NULL, release_path = NULL,
                               interactive = TRUE, auto_update = FALSE, ...) {
  status <- "passed"

  msg_text <- "Checking that sequence IDs are unique"
  cat(paste0("  ", msg_text, "\r"))

  sequence_data <- suppressMessages(googlesheets4::read_sheet(database_path, sheet = "sequence_data"))
  seq_ids <- stringr::str_match(sequence_data$oodb_id, pattern = "^oodb_([0-9]+)$")[, 2]
  if (any(duplicated(seq_ids))) {
    cli::cli_alert_danger(msg_text)
    print_error_report(paste0("The following sequence IDs in 'oodb_id' are not unique:\n  ",
                              paste0(sequence_data$oodb_id[duplicated(seq_ids)], ' (row ', which(duplicated(seq_ids)), ')', collapse = ", "),
                              "\nAll sequence IDs must be unique."))
    return("failed")
  } else {
    cli::cli_alert_success(msg_text)
  }

  return(status)
}


#' Check sequences
#'
#' Checks sequences for invalid characters and warn about sequence changes between versions.
#'
#' @inheritParams validate
#'
#' @keywords internal
check_sequence <- function(database_path = NULL, release_path = NULL,
                               interactive = TRUE, auto_update = FALSE, ...) {
  status <- "passed"

  msg_text <- "Checking sequences"
  cat(paste0("  ", msg_text, "\r"))

  sequence_data <- suppressMessages(googlesheets4::read_sheet(database_path, sheet = "sequence_data"))
  is_valid <- grepl(sequence_data$sequence, pattern = "^[ACGTRYSWKMBDHVN]+$", ignore.case = TRUE)
  if (any(! is_valid)) {
    cli::cli_alert_danger(msg_text)
    print_error_report(paste0("The following sequences have invalid characters:\n  ",
                              paste0(' row ', which(! is_valid), collapse = ", "),
                              "\nOnly the following characters are allowed: ACGTRYSWKMBDHVN."))
    return("failed")
  } else {
    cli::cli_alert_success(msg_text)
  }

  return(status)
}


#' Check Genbank IDs
#'
#' Checks that Genbank IDs are valid.
#'
#' @inheritParams validate
#'
#' @keywords internal
check_genbank_ids <- function(database_path = NULL, release_path = NULL,
                           interactive = TRUE, auto_update = FALSE, ...) {
  status <- "passed"

  msg_text <- "Checking Genbank IDs"
  cat(paste0("  ", msg_text, "\r"))

  sequence_data <- suppressMessages(googlesheets4::read_sheet(database_path, sheet = "sequence_data"))
  is_valid <- is_valid_genbank_acc(sequence_data$genbank_id)
  passed <- is_valid | is.na(is_valid)
  if (any(! passed)) {
    cli::cli_alert_danger(msg_text)
    print_error_report(paste0("The following Genbank accession numbers are not valid:\n  ",
                              paste0(sequence_data$genbank_id[! passed], ' (row ', which(! passed), ')', collapse = ", "),
                              "\n"))
    return("failed")
  } else {
    cli::cli_alert_success(msg_text)
  }

  return(status)
}

is_valid_genbank_acc <- function(ids, batch_size = 20) {
  batches <- split(ids[! is.na(ids)], ceiling(seq_along(ids[! is.na(ids)])/batch_size))
  pb <- progress::progress_bar$new(total = length(batches))
  result <- vapply(seq_along(batches), FUN.VALUE = character(1), function(i) {
    pb$tick()
    Sys.sleep(0.5)
    output <- tryCatch(rentrez::entrez_fetch(db = "nuccore", id = batches[[i]], rettype = "seqid", retmode = "text"),
                       error = function(e) {
                         if (grepl(e$message, pattern = 'ID list is empty')) {
                           return("")
                         } else {
                           stop(e)
                         }
                       })
  })
  vapply(ids, FUN.VALUE = logical(1), function(id) any(grepl(result, pattern = id)))
}


#' Check Genbank taxon IDs
#'
#' Checks that Genbank taxon IDs are valid.
#'
#' @inheritParams validate
#'
#' @keywords internal
check_taxon_ids <- function(database_path = NULL, release_path = NULL,
                              interactive = TRUE, auto_update = FALSE, ...) {
  status <- "passed"

  # Check that all IDs in sequences data are in taxon data
  msg_text <- "Checking that all taxon IDs are in taxon data"
  cat(paste0("  ", msg_text, "\r"))
  sequence_data <- suppressMessages(googlesheets4::read_sheet(database_path, sheet = "sequence_data"))
  taxon_data <- suppressMessages(googlesheets4::read_sheet(database_path, sheet = "taxon_data"))
  missing_ids <- unique(! sequence_data$taxon_ids %in% taxon_data$taxon_ids)
  if (length(missing_ids) > 0) {
    cli::cli_alert_danger(msg_text)
    print_error_report(paste0("The following taxon IDs in 'sequence_data' are not in 'taxon_data':\n  ",
                              paste0(sequence_data$genbank_id[! is_valid], ' (row ', which(! is_valid), ')', collapse = ", "),
                              "\nAll taxon IDs used must occur exactly once in the 'taxon_data' sheet.\n"))
    return("failed")
  } else {
    cli::cli_alert_success(msg_text)
  }

  # Check that IDs are valid
  msg_text <- paste0("Checking taxon IDs in 'taxon_data'")
  cat(paste0("  ", msg_text, "\r"))
  is_custom_id <- grepl(taxon_data$taxon_id, pattern = "^oodb_tax_[0-9]+$")
  ncbi_ids_to_check <- unique(taxon_data$taxon_id[! is_custom_id & ! is.na(taxon_data$taxon_id)])
  is_valid <- is_valid_genbank_tax(ncbi_ids_to_check)
  if (any(! is_valid)) {
    cli::cli_alert_danger(msg_text)
    print_error_report(paste0("The 'taxon_data' sheet contains the following invalid taxon IDs:\n  ",
                              paste0(ncbi_ids_to_check[! is_valid], collapse = ", "),
                              "\nAll taxon IDs must be either valid NCBI taxon IDs or be a number with the prefix 'oodb_tax_'.\n"))
    return('failed')
  } else {
    cli::cli_alert_success(msg_text)
  }

  return(status)
}

is_valid_genbank_tax <- function(ids, batch_size = 20) {
  batches <- split(ids[! is.na(ids)], ceiling(seq_along(ids[! is.na(ids)])/batch_size))
  pb <- progress::progress_bar$new(total = length(batches))
  unlist(lapply(seq_along(batches), function(i) {
    pb$tick()
    Sys.sleep(0.5)
    ! is.na(taxize::as.uid(batches[[i]]))
  }))
}

